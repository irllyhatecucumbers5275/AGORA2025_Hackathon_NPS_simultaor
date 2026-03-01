# code2

# packages
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

# ===== 사용자 조정 파라미터 =====
start_year      <- 2022
end_year        <- 2072
start_work_age  <- 22    # 취업 시작 연령(근로기간 계산에 사용)
Z_years         <- 22    # 연금 수령 기간(년)
avg_wage0_trn   <- 0.00004  # 2022년 1인 연평균 임금(조원). 4천만원 = 0.00004조원
min_age         <- 0
max_age         <- 100     # 데이터 범위에 맞게 필요시 조정

# 기대수익비(ER, expected return ratio) 램프: 은퇴연도 기준 2.0 -> 1.0 선형
er_target <- function(retire_year) {
  p <- (retire_year - start_year) / (end_year - start_year)
  p <- pmin(pmax(p, 0), 1)
  2.0 - 1.0 * p
}

# ===== 데이터 로드 =====
age_df   <- read_csv("REALREALAGE.csv",
                     show_col_types = FALSE) %>%
  # 기대: columns = age(평균나이, numeric), year, age_pop
  mutate(age = as.numeric(age)) %>%
  filter(year >= start_year, year <= end_year,
         age >= min_age, age <= max_age)

macro    <- read_csv("MACRO.csv",
                     show_col_types = FALSE) %>%
  filter(year >= start_year, year <= end_year)

# ===== MACRO 전처리 =====
macro1 <- macro %>%
  mutate(
    # coverage가 없으면 근사식으로 계산
    coverage_calc = enrollment * (1 - exemption) * (1 - regional_share) +
      regional_share * regional_collection,
    coverage = ifelse(is.na(coverage), coverage_calc, coverage),
    
    # CPI, real wage growth는 예시에서 "1.02 = 2%↑" 형태라고 가정 → 그대로 곱
    cpi_gross  = cpi,
    wr_gross   = wage_growth_real,
    
    # 기금수익률: 퍼센트(예: 5)로 들어오면 0.05로, 이미 0.05면 그대로
    fund_yield = ifelse(fund_yield_nominal > 1, fund_yield_nominal/100, fund_yield_nominal),
    
    # 명목 임금지수 (base=1 at start_year)
    wage_index_nom = cumprod(replace_na(wr_gross, 1) * replace_na(cpi_gross, 1)),
    wage_index_nom = wage_index_nom / wage_index_nom[year == start_year]
  )

# 초기 적립금(조원)
init_fund <- macro1$initial_fund_trn_krw[macro1$year == start_year][1]
if (is.na(init_fund)) stop("MACRO.csv의 initial_fund_trn_krw(2022)가 필요합니다.")

# ===== 은퇴연령 시계열 만들기 =====
# retirement_age_R가 연도별로 변할 수 있으므로 연도별로 갖다 씀
ret_age_tbl <- macro1 %>% select(year, retirement_age_R)

# ===== 인구 테이블 정렬 & 은퇴자 수 =====
# 현년도 은퇴자 수(= 올해 처음 은퇴연령에 진입하는 인구)를 근사:
# retirees_t = sum(age >= R), new_retirees_t = max(retirees_t - retirees_{t-1}, 0)
retire_pop <- age_df %>%
  left_join(ret_age_tbl, by="year") %>%
  group_by(year) %>%
  summarise(
    retirees_stock = sum(age_pop[age >= retirement_age_R], na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(year) %>%
  mutate(
    lag_stock      = dplyr::lag(retirees_stock, default = 0),
    new_retirees   = pmax(retirees_stock - lag_stock, 0)
  )

# ===== 신규 은퇴코호트의 k(평생 납부) 근사와 b0(연간기초급여) =====
# k_per_capita ≈ τ × 평균 coverage(근로기간 평균) × 평균임금(은퇴연도) × 근로기간(년)
# 근로기간 = retirement_age_R - start_work_age
macro2 <- macro1 %>%
  mutate(avg_cov_roll = coverage) # 단순: 그 해 coverage 사용(보수적/간단)

cohort_seed <- retire_pop %>%
  left_join(macro2 %>% select(year, tau, coverage, avg_cov_roll, wage_index_nom, cpi_gross, fund_yield, retirement_age_R, labor_force),
            by="year") %>%
  mutate(
    working_years = pmax(retirement_age_R - start_work_age, 0),
    
    # 은퇴연도 1인 평균임금(조원)
    avg_wage_trn  = avg_wage0_trn * wage_index_nom,
    
    # 1인 평생 납부 k(조원) – 매우 보수적 단순화
    k_per_capita  = tau * avg_cov_roll * avg_wage_trn * working_years,
    
    # 해당 은퇴세대 기대수익비 목표
    er_ratio      = er_target(year),
    
    # 1인 연간 기초급여(은퇴 첫해, 조원)
    b0_per_capita = ifelse(Z_years > 0, (er_ratio * k_per_capita) / Z_years, 0),
    
    # 신규 은퇴자 수
    newN          = new_retirees
  ) %>%
  select(year, newN, b0_per_capita, cpi_gross)

# ===== 연금지급 스케줄(코호트 누적 CPI 인상 반영) =====
# 각 은퇴년 cohort가 Z_years 동안 b0_per_capita * (은퇴후 누적 CPI)로 지급된다고 가정
years <- seq(start_year, end_year, by=1)

# 은퇴연도별로 이후 CPI 누적을 계산하기 위해, 연도별 cpi_gross 벡터 준비
cpi_map <- macro1 %>% select(year, cpi_gross)

# 지급행렬(연도x코호트), and sum up
benefit_by_year <- tibble(year = years, benefit_total = 0)

for (ry in years) {
  # 해당 은퇴코호트
  row <- cohort_seed %>% filter(year == ry)
  if (nrow(row) == 0 || row$newN[1] == 0) next
  
  base_b <- row$b0_per_capita[1]
  newN   <- row$newN[1]
  
  # 은퇴후 t=0,~ , Z_years-1 동안 지급
  pay_years <- ry + (0:(Z_years-1))
  pay_years <- pay_years[pay_years >= start_year & pay_years <= end_year]
  if (length(pay_years) == 0) next
  
  # 누적 CPI(은퇴연도 대비)- prod_{s=ry+1..t}(cpi_gross_s)
  cpi_vec <- cpi_map %>% filter(year >= ry, year <= end_year)
  # 누적계산
  cpi_cum <- rep(1, nrow(cpi_vec))
  if (nrow(cpi_vec) > 1) {
    for (i in 2:nrow(cpi_vec)) {
      cpi_cum[i] <- cpi_cum[i-1] * cpi_vec$cpi_gross[i]
    }
  }
  # 연도 매칭
  pay_tbl <- tibble(year = cpi_vec$year, cpi_cum = cpi_cum) %>%
    filter(year %in% pay_years) %>%
    mutate(benefit = newN * base_b * cpi_cum)
  
  # 누적
  benefit_by_year <- benefit_by_year %>%
    left_join(pay_tbl %>% select(year, benefit), by="year") %>%
    mutate(benefit_total = benefit_total + replace_na(benefit, 0)) %>%
    select(year, benefit_total)
}

# total income on insurance itself
# contribution_t = τ × coverage × labor_force × average REAAAL WAGE
flow_tbl <- macro1 %>%
  select(year, tau, coverage, labor_force, wage_index_nom, fund_yield, initial_fund_trn_krw) %>%
  mutate(
    avg_wage_trn   = avg_wage0_trn * wage_index_nom,
    contributions  = tau * coverage * labor_force * avg_wage_trn
  ) %>%
  left_join(benefit_by_year, by="year") %>%
  mutate(benefits = replace_na(benefit_total, 0)) %>%
  arrange(year)

# SIMulation, for accounted cash flow

fund_path <- flow_tbl %>%
  mutate(
    fund_start = ifelse(year == start_year, initial_fund_trn_krw, NA_real_)
  ) %>%
  mutate(
    fund = NA_real_
  )

for (i in seq_len(nrow(fund_path))) {
  y  <- fund_path$year[i]
  fy <- fund_path$fund_yield[i]
  contrib <- fund_path$contributions[i]
  bene    <- fund_path$benefits[i]
  
  if (i == 1) {
    prev_fund <- init_fund
  } else {
    prev_fund <- fund_path$fund[i-1]
  }
  
  fund_path$fund[i] <- (prev_fund + contrib - bene) * (1 + fy)
}

# code for PLOT


p1 <- ggplot(fund_path, aes(x = year, y = fund)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +  # <-- 여기 수정
  labs(title = "신연금 체계하 국민연금 적립금 추정 경로 (2022–2072)",
       x = "연도", y = "적립금(조원)",
       caption = "보험료수입 = τ × coverage × labor_force × 평균임금(명목)\n급여 = 은퇴코호트 b0 × CPI누적, Z년 동안. 기대수익비 2.0→1.0(은퇴연도 기준 선형)") +
  theme_minimal(base_size = 13)

print(p1)

# 보조 --  연간 현금흐름도 확인


# cash flow graph
p2 <- flow_tbl %>%
  select(year, contributions, benefits) %>%
  pivot_longer(-year, names_to="flow", values_to="amount") %>%
  ggplot(aes(year, amount, color = flow)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +  # <-- 여기 수정
  scale_color_discrete(labels = c("보험료수입", "급여지급")) +
  labs(title = "연금 현금흐름(보험료수입 vs 급여지급)",
       x = "연도", y = "금액(조원)", color = "") +
  theme_minimal(base_size = 13)


# result graph
fund_summary <- fund_path %>%
  select(year, fund) %>%
  mutate(status = ifelse(fund > 0, "흑자", "적자/고갈"))
print(head(fund_summary, 5))
print(tail(fund_summary, 5))

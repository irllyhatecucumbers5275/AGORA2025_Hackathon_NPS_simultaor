# code version 0
"
■2022년 말 기금 금액

■기여율: 13%. 현행 보험료율에 고정한 보험료를 산정한다고 가정

■ 평균과세소득 추계 - 구해야함
지역가입자 소득추계 - 과거 추이에 비해 감소폭 적음

■ 인플레이션 - 2%가 타겟 레이트, 고정
■ 나이별 인구수 데이터

write_csv(lngcohot01,"longdata_agecohot_poplation.csv")

"

# ==============================
# 국민연금 개혁 시뮬레이터 (2022~2072)
# 25~64세 납부 / 65세 이상 수급 / 기대수익비 2.0→1.0
# ==============================

library(tidyverse)

# ---- 1) 파일 경로 설정 ----
macro_path <- "NPS_macro_institutional_assumptions_2022_2072.csv"
pop_path   <- "longdata_agecohot_poplation.csv"

# ---- 2) 데이터 불러오기 ----
assump <- read.csv(macro_path, check.names = FALSE, encoding = "UTF-8")
pop    <- read.csv(pop_path,   check.names = FALSE, encoding = "UTF-8")

# ---- 3) 기본 설정 ----
t0 <- 2022                     # 기준연도
tau <- unique(assump$tau)[1]   # 보험료율 (예: 0.09)
R_age <- 65                    # 수급 개시 연령
G0_trn <- 758.149              # 2022년말 국민연금기금 (조원 단위)

# ---- 4) 인구 데이터 전처리 ----
pop <- pop %>%
  rename(age_band = ...1, year = year, pop = age_pop) %>%
  mutate(
    age_low  = as.numeric(substr(age_band, 1, 2)),
    age_high = as.numeric(substr(age_band, 4, 5)),
    age_mid0 = (age_low + age_high)/2
  ) %>%
  select(year, age_low, age_high, age_mid0, pop)

# ---- 5) 거시 변수 정리 ----
assump <- assump %>%
  mutate(
    cpi = cpi / 100,
    fund_yield_nominal = fund_yield_nominal / 100
  )

dat <- pop %>%
  left_join(assump %>%
              select(year, cpi, tau, coverage, wage_growth_nominal_approx,
                     fund_yield_nominal, base_benefit_monthly),
            by = "year") %>%
  mutate(
    # 평균임금: 2022년 기준 4000만원 = 0.00004조원
    W = 0.00004 * cumprod(1 + wage_growth_nominal_approx),
    age_mid_t = age_mid0 + (year - t0),
    is_worker = age_mid_t >= 25 & age_mid_t < 65,
    is_retired = age_mid_t >= 65
  )

# ---- 6) 기대수익비 경로 정의 ----
make_e_path <- function(s, years, t0) {
  tibble(year = years,
         e = pmax(1.0, 2.0 - s * (years - t0)))
}
# ---- 7) 기금 시뮬레이터 ----
simulate_fund <- function(s, U = 25, G0_trn = 758.149, forbid_negative = TRUE) {
  years <- sort(unique(assump$year))
  e_v   <- make_e_path(s, years, t0) %>% deframe()
  cpi_v <- deframe(assump %>% select(year, cpi))
  r_v   <- deframe(assump %>% select(year, fund_yield_nominal))
  W_v   <- deframe(dat %>% distinct(year, W))
  
  # 초기 기금 세팅
  G <- setNames(rep(NA_real_, length(years)), years)
  G[as.character(t0)] <- G0_trn
  
  # 2022년 기준 1인당 연금액(기초)
  b_legacy_monthly_2022 <- assump %>% filter(year == 2022) %>% pull(base_benefit_monthly)
  idx <- 1
  b_legacy_ann_v <- c()
  for (y in years) {
    if (y == 2022) idx <- 1 else idx <- idx * (1 + cpi_v[as.character(y)])
    b_legacy_ann_v[as.character(y)] <- b_legacy_monthly_2022 * idx * 12 / 1e8 # 원→조원
  }
  
  for (y in years) {
    dat_y <- dat %>% filter(year == y)
    
    # 근로자 납부: 25~64세
    workers_y <- dat_y %>% filter(is_worker)
    C_t <- tau * W_v[as.character(y)] * sum(workers_y$pop * dat_y$coverage[1], na.rm = TRUE)
    C_t <- ifelse(is.na(C_t), 0, C_t)
    
    # 65 up, takers takeawaaaaaay
    retirees_y <- dat_y %>% filter(is_retired)
    P_t <- sum(retirees_y$pop, na.rm = TRUE) * b_legacy_ann_v[as.character(y)]
    P_t <- ifelse(is.na(P_t), 0, P_t)
    
    # 기대수익비 보정
    e_t <- e_v[as.character(y)]
    P_t <- P_t * e_t / 2.0  # 2.0→1.0로 조정 반영
    
    # 기금 업데이트
    G_prev <- if (y == t0) G[as.character(y)] else G[as.character(y - 1)]
    G_now <- (G_prev + C_t - P_t) * (1 + r_v[as.character(y)])
    if (is.na(G_now)) G_now <- G_prev
    G[as.character(y)] <- G_now
    
    # 기금이 음수면 중단
    if (forbid_negative && !is.na(G_now) && G_now < 0) {
      return(list(ok = FALSE, G = G))
    }
  }
  list(ok = TRUE, G = G)
}


# ---- 8) 기대수익비 하락속도 s* 탐색 ----
find_s_star <- function(s_lo = 0, s_hi = 0.1, tol = 1e-4) {
  while ((s_hi - s_lo) > tol) {
    s_mid <- 0.5 * (s_lo + s_hi)
    res <- simulate_fund(s_mid)
    if (res$ok) s_lo <- s_mid else s_hi <- s_mid
  }
  list(s_star = s_lo, path = simulate_fund(s_lo))
}

# ---- 9) 실행 및 시각화 ----
res <- find_s_star()
s_star <- res$s_star
G_path <- enframe(res$path$G, name = "year", value = "G_trn")

cat(sprintf("\n기금이 고갈되지 않는 최대 기대수익비 감소속도 s* ≈ %.5f\n", s_star))

ggplot(G_path, aes(x = as.integer(year), y = G_trn)) +
  geom_line(color = "#0072B2", linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = sprintf("국민연금 적립금 시뮬레이션 (s* = %.5f)", s_star),
    subtitle = "25~64세 납부 / 65세 이상 수급 / 기대수익비 2.0→1.0",
    x = "연도",
    y = "기금 잔액 (조원)"
  ) +
  theme_minimal(base_size = 14)



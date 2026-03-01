
# code 1
pop <- read_csv("longdata_agecohot_poplation.csv")     # 연령별 인구데이터
assump <- read_csv("assump_macro.csv")                 # 거시경제 가정 (네가 만든 표 IV-1, IV-2 기반)

# go for head
pop <- pop %>%
  mutate(
    age_low = as.numeric(substr(age_band, 1, 2)),
    age_high = as.numeric(substr(age_band, 4, 5)),
    age_mid = (age_low + age_high) / 2
  )

assump <- assump %>%
  mutate(
    year = as.integer(year),
    fund_yield_nominal = fund_yield_nominal / 100,
    cpi = cpi / 100,
    coverage = coverage / 100,
    tau = tau / 100
  )

# defdef

t0 <- 2022               
GDP_base <- 2200         
benefit_ratio <- 0.45    # 45pc rate // when ret
G0_trn <- 758.149        # 적립금
base_e <- 2.0            # base ER 


make_e_path <- function(s, years, t0) {
  tibble(
    year = years,
    e = pmax(1.0, base_e - s * (years - t0))
  )
}

#MAIN simulator PARAMETER SETUP
simulate_fund <- function(s, forbid_negative = TRUE) {
  
  years <- sort(unique(pop$year))
  e_v   <- make_e_path(s, years, t0) %>% deframe()
  r_v   <- deframe(assump %>% select(year, fund_yield_nominal))
  cov_v <- deframe(assump %>% select(year, coverage))
  tau_v <- deframe(assump %>% select(year, tau))
  
  # 연도별 총인구
  total_pop_y <- pop %>%
    group_by(year) %>%
    summarise(total_pop = sum(pop, na.rm = TRUE)) %>%
    ungroup()
  
  # 기금 초기화
  G <- setNames(rep(NA_real_, length(years)), years)
  G[as.character(t0)] <- G0_trn
  
  for (y in years) {
    
    dat_y <- pop %>%
      filter(year == y) %>%
      left_join(total_pop_y, by = "year") %>%
      mutate(
        pop_share = pop / total_pop,
        is_worker = age_mid >= 25 & age_mid < 65,
        is_retire = age_mid >= 65
      )
    
    worker_share <- sum(dat_y$pop_share[dat_y$is_worker], na.rm = TRUE)
    retire_share <- sum(dat_y$pop_share[dat_y$is_retire], na.rm = TRUE)
    
    e_t <- e_v[as.character(y)]
    r_t <- r_v[as.character(y)]
    cov_t <- cov_v[as.character(y)]
    tau_t <- tau_v[as.character(y)]
    
    # gdp 대비 비율 기반 수입,지출 계산
    C_t <- tau_t * cov_t * worker_share * GDP_base
    P_t <- e_t * benefit_ratio * retire_share * GDP_base
    
    # 적립금 업데이트
    if (y == t0) {
      G_prev <- G[as.character(y)]
    } else {
      G_prev <- G[as.character(y - 1)]
    }
    
    G_now <- (G_prev + C_t - P_t) * (1 + r_t)
    if (is.na(G_now)) G_now <- G_prev
    G[as.character(y)] <- G_now
    
    # what if it runs out?
    if (forbid_negative && G_now < 0) return(list(ok = FALSE, G = G))
  }
  
  list(ok = TRUE, G = G)
}

# setup for the ER RATE
find_s_star <- function(s_lo = 0, s_hi = 0.1, tol = 1e-4) {
  while ((s_hi - s_lo) > tol) {
    s_mid <- 0.5 * (s_lo + s_hi)
    res <- simulate_fund(s_mid)
    if (res$ok) {
      s_lo <- s_mid
    } else {
      s_hi <- s_mid
    }
  }
  list(s_star = s_lo, path = simulate_fund(s_lo))
}

#RES_ults
res <- find_s_star()
s_star <- res$s_star
G_path <- enframe(res$path$G, name = "year", value = "G_trn")

# plot
ggplot(G_path, aes(x = as.integer(year), y = G_trn)) +
  geom_line(color = "#0072B2", linewidth = 1.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = sprintf("국민연금 적립금 시뮬레이션 (s* = %.5f)", s_star),
    subtitle = "인구동태 반영 / 25~64세 납부 / 65세 이상 수급 / 기대수익비 2.0→1.0",
    x = "연도",
    y = "기금 잔액 (조원)"
  ) +
  theme_minimal(base_size = 14)
## ============================================================
## 생명보험 · 연금보험료 계산기  (Project #3, Part 4)
## 데이터: 2010년 완전생명표 / 연이자율 i는 각 탭 사이드바에서 직접 조정 (기본값: 3조 지정 7%)
## ------------------------------------------------------------
## - 생명보험 탭 : n년 납입 / m년 만기 정기(term) 생명보험 월(회)납 보험료
## - 연금 탭     : 거치 후 종신연금 월(회)납 보험료
## - 사이드바에서 성별을 선택해 한 명 기준으로 계산 (색상은 남녀 구분 없이 단일 톤)
## - 손익분기(원금회수) 나이를 그래프 + 핵심지표 카드로 시각화
## - 납입주기(연납/분기납/월납) 선택 가능
## - 입력값 범위를 벗어나면 계산 대신 안내 문구 표시
## - 디자인: 보험사 웹사이트 톤의 네이비 + 골드 단일 포인트 컬러, 히어로 배너
## ============================================================

## 배포 서버에서 런타임 중 누락된 패키지를 자동 설치하려 할 때
## "trying to use CRAN without setting a mirror" 에러로 죽지 않도록,
## 라이브러리 로드보다 먼저 CRAN 미러를 명시적으로 지정합니다.
options(repos = c(CRAN = "https://cloud.r-project.org"))

library(shiny)
library(bslib)
library(DT)
library(plotly)

options(shiny.sanitize.errors = TRUE)

## ------------------------------------------------------------
## 1. 데이터 로드 (이자율과 무관한 부분만 앱 시작 시 1회 계산)
## ------------------------------------------------------------

life <- read.csv("data/life_table.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
# life 컬럼: age, sex("남자"/"여자"), lx(생존자수), dx(사망자수), qx(사망확률), Lx(정지인구), ex(기대여명)

MAX_AGE <- 100   # 2010 생명표가 제공하는 최대 연령 (100세 = 100세 이상 통합)

# 성별별 lx, dx 벡터 생성 (이름 = 나이 문자열) — 이자율과 무관하므로 한 번만 계산
build_life_vec <- function(sex_label) {
  d <- life[life$sex == sex_label, ]
  d <- d[order(d$age), ]
  list(lx = setNames(d$lx, as.character(d$age)),
       dx = setNames(d$dx, as.character(d$age)))
}
## 내부 키는 항상 ASCII("male"/"female")를 쓰고, 한글은 화면 표시 문자열로만 사용합니다.
## (일부 Windows R 환경의 로캘/인코딩 차이로 한글을 리스트 이름·$접근자로 쓰면 깨질 수 있어 방지)
LIFE <- list(male = build_life_vec("남자"), female = build_life_vec("여자"))

lx_at <- function(life_sex, age) {
  if (age > MAX_AGE || age < 0) return(0)
  unname(life_sex$lx[as.character(age)])
}
dx_at <- function(life_sex, age) {
  if (age > MAX_AGE || age < 0) return(0)
  unname(life_sex$dx[as.character(age)])
}

# UDD(사망의 균등분포) 가정 하의 소수 나이 생존자수 l(x+t)  (과제 Part(2)에서 쓴 UDD 가정과 동일)
# l(x+k+f) = l(x+k) - f * d(x+k),  k = floor(t), f = t - k
l_frac <- function(life_sex, x, t) {
  if (t <= 0) return(lx_at(life_sex, x))
  k    <- floor(t)
  frac <- t - k
  age_k <- x + k
  if (age_k > MAX_AGE) return(0)
  lk <- lx_at(life_sex, age_k)
  if (frac == 0 || age_k >= MAX_AGE) return(lk)
  dk <- dx_at(life_sex, age_k)
  lk - frac * dk
}

## ------------------------------------------------------------
## 2. 이자율에 의존하는 D_x, C_x, M_x, N_x  ─ v(할인율)를 인자로 받는 함수
##    (연이자율 슬라이더가 바뀔 때마다 서버에서 다시 계산됩니다)
## ------------------------------------------------------------

# D_x = v^x * l_x ,  C_x = v^(x+1) * d_x  (연말급부 기준, Part(3) 정의 그대로)
precompute <- function(life_sex, v) {
  ages <- 0:MAX_AGE
  lx <- sapply(ages, function(a) lx_at(life_sex, a))
  dx <- sapply(ages, function(a) dx_at(life_sex, a))
  Dx <- v^ages * lx
  Cx <- v^(ages + 1) * dx
  Mx <- rev(cumsum(rev(Cx)))   # M_x = C_x + C_(x+1) + ... + C_100
  Nx <- rev(cumsum(rev(Dx)))   # N_x = D_x + D_(x+1) + ... + D_100
  names(Dx) <- names(Cx) <- names(Mx) <- names(Nx) <- as.character(ages)
  list(Dx = Dx, Cx = Cx, Mx = Mx, Nx = Nx)
}

get_D <- function(act, age) if (age > MAX_AGE || age < 0) 0 else unname(act$Dx[as.character(age)])
get_M <- function(act, age) if (age > MAX_AGE) 0 else unname(act$Mx[as.character(age)])
get_N <- function(act, age) if (age > MAX_AGE) 0 else unname(act$Nx[as.character(age)])

## ------------------------------------------------------------
## 3. 보험수리 함수 (act/life_sex/v를 명시적으로 전달 → 이자율이 바뀌면 그대로 재사용)
## ------------------------------------------------------------

# 정기(term) 생명보험 순보험료율 : A^1_{x:m} = (M_x - M_(x+m)) / D_x
term_insurance_A <- function(act, x, m) {
  Dx <- get_D(act, x)
  if (is.na(Dx) || Dx == 0) return(NA_real_)
  (get_M(act, x) - get_M(act, x + m)) / Dx
}

# n년 유기 생명연금(선급) : a_{x:n}^{(freq)} , UDD 가정 하 직접 합산
#   a_{x:n}^{(freq)} = (1/freq) * sum_{j=0}^{n*freq-1} v^(j/freq) * l(x+j/freq)/l_x
temp_annuity_due <- function(life_sex, v, x, n, freq) {
  n_periods <- round(n * freq)
  if (n_periods <= 0) return(0)
  lx0 <- lx_at(life_sex, x)
  if (is.na(lx0) || lx0 == 0) return(NA_real_)
  js <- 0:(n_periods - 1)
  ts <- js / freq
  lt <- sapply(ts, function(t) l_frac(life_sex, x, t))
  sum(v^ts * (lt / lx0)) / freq
}

# 거치 종신연금(선급) : x+defer세부터 사망 시까지 매 1/freq년마다 지급
#   {}_defer|a_x^{(freq)} = (1/freq) * sum_{j=0}^{...} v^(defer+j/freq) * l(x+defer+j/freq)/l_x
deferred_annuity_due <- function(life_sex, v, x, defer, freq) {
  years <- MAX_AGE - (x + defer)
  if (years <= 0) return(0)
  n_periods <- round(years * freq)
  lx0 <- lx_at(life_sex, x)
  if (is.na(lx0) || lx0 == 0) return(NA_real_)
  js <- 0:(n_periods - 1)
  ts <- defer + js / freq
  lt <- sapply(ts, function(t) l_frac(life_sex, x, t))
  sum(v^ts * (lt / lx0)) / freq
}

# 생명보험료 계산 : n년 납입 / m년 만기 정기보험, 보험금 benefit_won, 납입주기 freq(1/4/12)
calc_life_insurance <- function(act, life_sex, v, age, n_pay, m_term, benefit_won, freq) {
  A1    <- term_insurance_A(act, age, m_term)
  a_pay <- temp_annuity_due(life_sex, v, age, n_pay, freq)
  annual <- A1 * benefit_won / a_pay
  list(A1 = A1, a_pay = a_pay, annual = annual, per_payment = annual / freq)
}

# 연금보험료 계산 : defer년 거치 후 종신연금 annual_benefit_won(연액), 수령주기 payout_freq,
#                   n_pay년 납입, 납입주기 pay_freq
calc_annuity <- function(act, life_sex, v, age, defer, n_pay, annual_benefit_won, pay_freq, payout_freq) {
  B     <- annual_benefit_won * deferred_annuity_due(life_sex, v, age, defer, payout_freq)
  a_pay <- temp_annuity_due(life_sex, v, age, n_pay, pay_freq)
  annual <- B / a_pay
  list(B = B, a_pay = a_pay, annual = annual, per_payment = annual / pay_freq)
}

## ------------------------------------------------------------
## 4. 생명표 표시용 wide 테이블 (남/여 나란히, 이자율과 무관 — 1회만 생성)
## ------------------------------------------------------------

wide_life <- merge(
  life[life$sex == "남자", c("age", "lx", "dx", "qx", "ex")],
  life[life$sex == "여자", c("age", "lx", "dx", "qx", "ex")],
  by = "age", suffixes = c("_남자", "_여자")
)
wide_life <- wide_life[order(wide_life$age), ]
names(wide_life) <- c("나이", "생존자수(남)", "사망자수(남)", "사망확률(남)", "기대여명(남)",
                       "생존자수(여)", "사망자수(여)", "사망확률(여)", "기대여명(여)")

make_life_table_dt <- function() {
  datatable(
    wide_life,
    rownames = FALSE,
    filter = "none",
    options = list(pageLength = 10, order = list(list(0, "asc")),
                   language = list(search = "검색:", info = "총 _TOTAL_건 중 _START_~_END_",
                                    paginate = list(previous = "이전", `next` = "다음"))),
    class = "stripe hover compact"
  )
}

## ------------------------------------------------------------
## 5. 색상 / 테마 (모던 시크 — 네이비 + 골드 단일 톤, 남녀 색상 구분 없음)
## ------------------------------------------------------------

NAVY      <- "#101B2E"   # 브랜드 프라이머리 (딥 네이비) — 성별과 무관하게 모든 강조에 동일 사용
NAVY_SOFT <- "#223354"   # 히어로 배너 그라데이션용 보조 네이비
GOLD      <- "#B8934A"   # 유일한 포인트 컬러 (성별 구분 없이 동일하게 사용)
INK       <- "#1B2230"
MUTED     <- "#697180"
SURFACE   <- "#F3F4F7"
CARD_BD   <- "#E4E7ED"

app_theme <- bs_theme(
  version      = 5,
  bg           = SURFACE,
  fg           = INK,
  primary      = NAVY,
  secondary    = GOLD,
  base_font    = font_google("Noto Sans KR"),
  heading_font = font_google("Noto Sans KR"),
  "border-radius"     = "0.6rem",
  "card-border-color" = CARD_BD,
  "card-bg"           = "#FFFFFF"
)

fmt_won <- function(x) paste0(format(round(x), big.mark = ",", scientific = FALSE), "원")

## 보험사 웹사이트 톤의 커스텀 CSS: 히어로 배너 / 지표 카드 / 핵심지표 스트립
app_css <- HTML(paste0("
body { letter-spacing: -0.01em; }
.navbar { border-bottom: 1px solid ", CARD_BD, "; }

/* ---- 히어로 배너 ---- */
.hero-banner {
  background: linear-gradient(120deg, ", NAVY, " 0%, ", NAVY_SOFT, " 100%);
  color: #F5F6F8;
  padding: 1.5rem 2rem 1.3rem 2rem;
}
.hero-banner .hero-eyebrow {
  font-size: 0.72rem; letter-spacing: 0.16em; text-transform: uppercase;
  color: ", GOLD, "; font-weight: 700; margin-bottom: 0.3rem;
}
.hero-banner h1 {
  font-size: 1.55rem; font-weight: 700; margin: 0 0 0.35rem 0; color: #FFFFFF;
}
.hero-banner .hero-sub { font-size: 0.86rem; color: #C7CDD9; margin-bottom: 0.9rem; }
.hero-badge {
  display: inline-flex; align-items: center; gap: 0.4rem;
  background: rgba(255,255,255,0.08); border: 1px solid rgba(255,255,255,0.16);
  border-radius: 999px; padding: 0.3rem 0.8rem; font-size: 0.76rem;
  color: #E7EAF0; margin: 0 0.5rem 0.4rem 0;
}
.hero-badge .dot { width: 6px; height: 6px; border-radius: 50%; background: ", GOLD, "; display: inline-block; }

/* ---- 사이드바 ---- */
.bslib-sidebar-layout > .sidebar {
  background: #FFFFFF; border-right: 1px solid ", CARD_BD, ";
}
.sidebar-section-label {
  font-size: 0.7rem; letter-spacing: 0.1em; text-transform: uppercase;
  color: ", GOLD, "; font-weight: 700; margin: 0.2rem 0 0.7rem 0;
}
.sidebar-divider { border: none; border-top: 1px solid ", CARD_BD, "; margin: 1.1rem 0 1rem 0; }
.payout-hint {
  font-size: 0.8rem; line-height: 1.4; color: ", MUTED, ";
  background: ", SURFACE, "; border: 1px solid ", CARD_BD, "; border-radius: 0.4rem;
  padding: 0.5rem 0.7rem; margin: -0.4rem 0 1rem 0;
}

/* ---- 섹션 라벨 / 결과 카드 ---- */
.section-label {
  font-size: 0.72rem; letter-spacing: 0.12em; text-transform: uppercase;
  color: ", MUTED, "; font-weight: 700; margin: 0.2rem 0 0.5rem 0;
}
.metric-card {
  border-top: 3px solid ", GOLD, " !important;
  min-height: 118px;
}
.metric-card .value-box-title { margin-bottom: 0.3rem; }
.metric-card .value-box-value { line-height: 1.35; }
.metric-card .value-box-showcase { align-self: center; }

/* ---- 그래프 카드 위 핵심지표 스트립 (가시성을 위해 여백/글자 크기를 넉넉하게) ---- */
.metric-strip { display: flex; gap: 0.7rem; flex-wrap: wrap; margin-bottom: 1.2rem; }
.metric-chip {
  background: ", SURFACE, "; border: 1px solid ", CARD_BD, "; border-radius: 0.55rem;
  padding: 0.75rem 1.1rem; min-width: 152px;
}
.metric-chip .metric-chip-label {
  display: block; font-size: 0.75rem; color: ", MUTED, "; margin-bottom: 0.25rem;
}
.metric-chip .metric-chip-value { font-weight: 700; color: ", NAVY, "; font-size: 1.15rem; line-height: 1.3; }
.metric-chip.accent .metric-chip-value { color: ", GOLD, "; }

.card-header { font-weight: 600; color: ", INK, "; background: #FFFFFF; }
.card-body { padding: 1.15rem 1.3rem; }
.footer-note { color: ", MUTED, "; }
"))

## ------------------------------------------------------------
## 6. UI
## ------------------------------------------------------------

freq_choices <- c("연납" = "1", "분기납" = "4", "월납" = "12")
sex_choices  <- c("남자" = "male", "여자" = "female")

life_sidebar <- sidebar(
  width = 340,
  title = "입력 정보",
  div(class = "sidebar-section-label", "가입자정보"),
  radioButtons("li_sex", "성별", choices = sex_choices, selected = "male", inline = TRUE),
  numericInput("li_age", "나이 x (세)", value = 40, min = 0, max = 99, step = 1),
  hr(class = "sidebar-divider"),
  div(class = "sidebar-section-label", "상품정보"),
  numericInput("li_i_rate", "연이자율 i (%)", value = 7, min = 1, max = 15, step = 0.5),
  div(class = "mb-2",
      actionLink("li_reset_rate", "3조 기본값(7%)", class = "small text-nowrap")
  ),
  sliderInput("li_pay", "납입만기 n (년) — 보험료를 납입하는 기간",
              min = 1, max = 20, value = 20, step = 1),
  sliderInput("li_term", "지급만기 m (년) — 이 기간 내 사망 시 보험금 지급 (n년 이상)",
              min = 1, max = 60, value = 20, step = 1),
  numericInput("li_benefit", "사망보험금 b (만원)", value = 10000, min = 1, step = 100),
  accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "고급 옵션",
      icon = tags$span("⚙"),
      radioButtons("li_freq", "납입주기 선택", choices = freq_choices, selected = "12", inline = TRUE)
    )
  ),
  uiOutput("li_alert")
)

annuity_sidebar <- sidebar(
  width = 340,
  title = "입력 정보",
  div(class = "sidebar-section-label", "가입자정보"),
  radioButtons("an_sex", "성별", choices = sex_choices, selected = "male", inline = TRUE),
  numericInput("an_age", "나이 x (세)", value = 30, min = 0, max = 99, step = 1),
  numericInput("an_retire", "은퇴시점 r (세)", value = 65, min = 1, max = 100, step = 1),
  hr(class = "sidebar-divider"),
  div(class = "sidebar-section-label", "상품정보"),
  numericInput("an_i_rate", "연이자율 i (%)", value = 7, min = 1, max = 15, step = 0.5),
  div(class = "mb-2",
      actionLink("an_reset_rate", "3조 기본값(7%)", class = "small text-nowrap")
  ),
  sliderInput("an_pay", "납입만기 n (년) — 보험료 납입 기간 (은퇴 전까지)",
              min = 1, max = 35, value = 20, step = 1),
  numericInput("an_start", "지급개시 나이 (세)", value = 65, min = 1, max = 100, step = 1),
  numericInput("an_benefit", "연금 수령액 b (만원/년)", value = 1000, min = 1, step = 100),
  uiOutput("an_payout_hint"),
  accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "고급 옵션",
      icon = tags$span("⚙"),
      radioButtons("an_payfreq", "보험료 납입주기", choices = freq_choices, selected = "12", inline = TRUE),
      radioButtons("an_payoutfreq", "연금 수령주기", choices = c("연 1회" = "1", "매월" = "12"),
                   selected = "1", inline = TRUE)
    )
  ),
  uiOutput("an_alert")
)

hero_banner <- div(
  class = "hero-banner",
  div(class = "hero-eyebrow", "ACTUARIAL PREMIUM CALCULATOR · 3조"),
  tags$h1("생명보험 · 연금보험료 계산기"),
  div(class = "hero-sub", "2010년 완전생명표 기반 정기생명보험 · 거치연금 순보험료 산출 시스템"),
  div(
    span(class = "hero-badge", span(class = "dot"), "데이터: KOSIS 2010 완전생명표"),
    span(class = "hero-badge", span(class = "dot"), "기준 이자율: 3조 i = 7%"),
    span(class = "hero-badge", span(class = "dot"), "UDD 가정 · 실시간 계산")
  )
)

ui <- page_navbar(
  title = "3조 Premium Calculator",
  theme = app_theme,
  fillable = FALSE,   # 화면 높이에 억지로 맞추지 않고, 내용이 많으면 페이지 전체가 자연스럽게 스크롤되도록 함
  header = tagList(
    tags$head(tags$style(app_css)),
    hero_banner
  ),
  nav_panel(
    "생명보험",
    layout_sidebar(
      sidebar = life_sidebar,
      div(class = "section-label", "계산 결과"),
      uiOutput("li_result_boxes"),
      card(
        class = "mt-3",
        card_header("손익분기 분석 — 사망연령별 누적납입보험료 vs 사망보험금"),
        uiOutput("li_metric_strip"),
        plotlyOutput("li_plot", height = "480px")
      ),
      accordion(
        open = FALSE,
        accordion_panel("2010년 생명표 보기 (남녀 비교)",
                         DTOutput("li_table"))
      )
    )
  ),
  nav_panel(
    "연금",
    layout_sidebar(
      sidebar = annuity_sidebar,
      div(class = "section-label", "계산 결과"),
      uiOutput("an_result_boxes"),
      card(
        class = "mt-3",
        card_header("원금회수 분석 — 나이별 누적납입액 vs 누적연금수령액"),
        uiOutput("an_metric_strip"),
        plotlyOutput("an_plot", height = "480px")
      ),
      accordion(
        open = FALSE,
        accordion_panel("2010년 생명표 보기 (남녀 비교)",
                         DTOutput("an_table"))
      )
    )
  ),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small footer-note", "자료: KOSIS 2010 완전생명표"))
)

## ------------------------------------------------------------
## 7. Server
## ------------------------------------------------------------

server <- function(input, output, session) {

  freq_label <- function(freq) switch(as.character(freq), "1" = "연납", "4" = "분기납", "12" = "월납", "회당")
  sex_label  <- function(sex) if (sex == "male") "남자" else "여자"
  sex_icon   <- function(sex) if (sex == "male") "♂" else "♀"
  # 남녀 색상은 구분하지 않습니다 — 모든 강조 색상은 NAVY/GOLD 단일 톤을 그대로 씁니다.

  # 두 탭은 각자의 "입력 정보" 안에 자기 이자율 입력칸을 갖고 있어서 서로 독립적으로 조정됩니다.
  to_rate_vals <- function(i_pct) {
    i <- i_pct / 100
    r <- log(1 + i)
    v <- exp(-r)
    list(i = i, r = r, v = v)
  }
  # 이자율 입력칸이 비어있거나(NA, 지우고 다시 입력하는 중) 범위를 벗어나면
  # 계산이 죽지 않도록 3조 기본값(7%)으로 안전하게 대체합니다.
  safe_i_pct <- function(i_pct) {
    if (is.null(i_pct) || is.na(i_pct) || i_pct <= -100 || i_pct > 100) return(7)
    i_pct
  }

  ## ---------- 생명보험 탭 ----------

  observeEvent(input$li_reset_rate, {
    updateNumericInput(session, "li_i_rate", value = 7)
  })

  li_rate_vals <- reactive({
    to_rate_vals(safe_i_pct(input$li_i_rate))
  })

  # 이 탭에서 선택한 이자율로 D_x/M_x/N_x를 다시 계산 (선택된 성별만)
  li_act <- reactive({
    precompute(LIFE[[input$li_sex]], li_rate_vals()$v)
  })

  # 나이가 바뀌면 슬라이더 최대값을 100세를 넘지 않도록 갱신
  observeEvent(input$li_age, {
    age <- input$li_age
    if (is.na(age)) return(invisible())
    max_years <- max(1, MAX_AGE - age)
    updateSliderInput(session, "li_term", max = max_years,
                       value = min(input$li_term, max_years))
  })
  observeEvent(input$li_term, {
    updateSliderInput(session, "li_pay", max = input$li_term,
                       value = min(input$li_pay, input$li_term))
  })

  li_errors <- reactive({
    errs <- c()
    age <- input$li_age
    if (is.na(age) || age < 0 || age > 99) {
      return("나이는 0~99세 사이여야 합니다.")
    }
    if ((age + input$li_term) > MAX_AGE)
      errs <- c(errs, sprintf("지급만기는 1~%d년 사이여야 합니다 (생명표는 100세까지 제공).", MAX_AGE - age))
    if (input$li_pay > input$li_term)
      errs <- c(errs, "납입만기(n)는 지급만기(m)보다 클 수 없습니다.")
    if (is.na(input$li_benefit) || input$li_benefit <= 0)
      errs <- c(errs, "사망보험금은 0보다 커야 합니다.")
    errs
  })

  output$li_alert <- renderUI({
    errs <- li_errors()
    if (length(errs) == 0) return(NULL)
    div(class = "alert alert-warning small mt-2 mb-0",
        tags$strong("입력값을 확인해주세요"),
        tags$ul(style = "margin-bottom:0; padding-left: 1.1rem;", lapply(errs, tags$li)))
  })

  li_results <- reactive({
    validate(need(length(li_errors()) == 0, "왼쪽 입력값을 확인해주세요."))
    freq <- as.numeric(input$li_freq)
    sex  <- input$li_sex
    calc_life_insurance(li_act(), LIFE[[sex]], li_rate_vals()$v, input$li_age, input$li_pay,
                         input$li_term, input$li_benefit * 10000, freq)
  })

  output$li_result_boxes <- renderUI({
    res <- li_results()
    sex <- input$li_sex
    lbl <- freq_label(as.numeric(input$li_freq))
    value_box(
      title = paste0(sex_label(sex), " ", lbl, " 보험료"),
      value = tags$span(style = "font-size:1.6rem; font-weight:700;", fmt_won(res$per_payment)),
      showcase = tags$span(style = paste0("font-size:2.1rem; color:", NAVY, ";"), sex_icon(sex)),
      theme = value_box_theme(bg = "#FFFFFF", fg = NAVY),
      class = "metric-card"
    )
  })

  # 그래프와 핵심지표 스트립이 같은 계산 결과를 공유하도록 한 곳에서만 계산합니다.
  li_curve <- reactive({
    res <- li_results()
    age <- input$li_age; n_pay <- input$li_pay; m_term <- input$li_term; benefit_won <- input$li_benefit * 10000

    ages <- (age + 1):MAX_AGE
    years_lived <- ages - age
    benefit <- ifelse(years_lived <= m_term, benefit_won, 0)
    premium <- pmin(years_lived, n_pay) * res$annual

    idx <- which(benefit > 0 & premium >= benefit)
    be <- if (length(idx) == 0) NA_integer_ else ages[min(idx)]
    list(ages = ages, benefit = benefit, premium = premium, breakeven = be)
  })

  output$li_metric_strip <- renderUI({
    res <- li_results(); cv <- li_curve()
    total_paid <- res$annual * input$li_pay
    div(class = "metric-strip",
        div(class = "metric-chip accent",
            span(class = "metric-chip-label", "손익분기 연령"),
            span(class = "metric-chip-value",
                 if (is.na(cv$breakeven)) "만기 내 미도달" else paste0(cv$breakeven, "세"))),
        div(class = "metric-chip",
            span(class = "metric-chip-label", paste0(input$li_pay, "년간 총 납입액")),
            span(class = "metric-chip-value", fmt_won(total_paid))),
        div(class = "metric-chip",
            span(class = "metric-chip-label", "사망보험금"),
            span(class = "metric-chip-value", fmt_won(input$li_benefit * 10000)))
    )
  })

  output$li_plot <- renderPlotly({
    cv <- li_curve()
    y_max <- max(cv$benefit, cv$premium, na.rm = TRUE)

    p <- plot_ly()
    p <- add_lines(p, x = cv$ages, y = cv$benefit, name = "사망보험금",
                    line = list(color = NAVY, width = 2.2),
                    hovertemplate = "%{x}세: %{y:,.0f}원<extra>사망보험금</extra>")
    p <- add_lines(p, x = cv$ages, y = cv$premium, name = "누적납입보험료",
                    line = list(color = GOLD, dash = "dash", width = 2.2),
                    hovertemplate = "%{x}세: %{y:,.0f}원<extra>누적납입보험료</extra>")

    shapes <- list()
    if (!is.na(cv$breakeven)) shapes <- list(list(type = "line", x0 = cv$breakeven, x1 = cv$breakeven, y0 = 0, y1 = y_max,
                                                    line = list(color = GOLD, dash = "dot", width = 1.4)))

    layout(p,
           xaxis = list(title = "가정 사망 연령", gridcolor = CARD_BD, zeroline = FALSE),
           yaxis = list(title = "금액(원)", gridcolor = CARD_BD, zeroline = FALSE),
           shapes = shapes,
           paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
           font = list(color = INK, family = "Noto Sans KR, sans-serif"),
           legend = list(orientation = "h", y = -0.2),
           margin = list(t = 10))
  })

  output$li_table <- renderDT({ make_life_table_dt() })

  ## ---------- 연금 탭 ----------

  observeEvent(input$an_reset_rate, {
    updateNumericInput(session, "an_i_rate", value = 7)
  })

  an_rate_vals <- reactive({
    to_rate_vals(safe_i_pct(input$an_i_rate))
  })

  # 이 탭에서 선택한 이자율로 D_x/M_x/N_x를 다시 계산 (선택된 성별만)
  an_act <- reactive({
    precompute(LIFE[[input$an_sex]], an_rate_vals()$v)
  })

  observeEvent(list(input$an_age, input$an_retire), {
    age <- input$an_age; retire <- input$an_retire
    if (is.na(age) || is.na(retire)) return(invisible())
    max_years <- max(1, retire - age)
    updateSliderInput(session, "an_pay", max = max_years, value = min(input$an_pay, max_years))
  })

  an_errors <- reactive({
    errs <- c()
    age <- input$an_age; retire <- input$an_retire; start_age <- input$an_start
    pay <- input$an_pay; benefit <- input$an_benefit

    # 나이가 비어있으면(입력창을 지우는 중 등) 이후 비교에서 전부 NA가 전파되어
    # if(NA)에서 에러가 나므로, 나이 문제는 여기서 바로 반환합니다.
    if (is.na(age) || age < 0 || age > 99) {
      return("나이는 0~99세 사이여야 합니다.")
    }
    if (is.na(retire) || retire <= age) {
      errs <- c(errs, "은퇴시점은 현재 나이보다 커야 합니다.")
    }
    if (is.na(start_age) || start_age <= age || start_age > MAX_AGE) {
      errs <- c(errs, sprintf("지급개시 나이는 %d~%d세 사이여야 합니다.", age + 1, MAX_AGE))
    }
    if (!is.na(retire) && !is.na(start_age) && start_age < retire) {
      errs <- c(errs, "지급개시 나이가 은퇴시점보다 이릅니다 — 값을 다시 확인해주세요.")
    }
    if (!is.na(retire) && !is.na(pay) && pay > (retire - age)) {
      errs <- c(errs, "납입만기(n)는 은퇴시점까지의 기간을 넘을 수 없습니다.")
    }
    if (is.na(benefit) || benefit <= 0) {
      errs <- c(errs, "연금 수령액은 0보다 커야 합니다.")
    }
    errs
  })

  output$an_alert <- renderUI({
    errs <- an_errors()
    if (length(errs) == 0) return(NULL)
    div(class = "alert alert-warning small mt-2 mb-0",
        tags$strong("입력값을 확인해주세요"),
        tags$ul(style = "margin-bottom:0; padding-left: 1.1rem;", lapply(errs, tags$li)))
  })

  an_results <- reactive({
    validate(need(length(an_errors()) == 0, "왼쪽 입력값을 확인해주세요."))
    pay_freq    <- as.numeric(input$an_payfreq)
    payout_freq <- as.numeric(input$an_payoutfreq)
    sex   <- input$an_sex
    defer <- input$an_start - input$an_age
    calc_annuity(an_act(), LIFE[[sex]], an_rate_vals()$v, input$an_age, defer, input$an_pay,
                 input$an_benefit * 10000, pay_freq, payout_freq)
  })

  output$an_result_boxes <- renderUI({
    res <- an_results()
    sex <- input$an_sex
    lbl <- freq_label(as.numeric(input$an_payfreq))
    value_box(
      title = paste0(sex_label(sex), " ", lbl, " 연금보험료"),
      value = tags$span(style = "font-size:1.6rem; font-weight:700;", fmt_won(res$per_payment)),
      showcase = tags$span(style = paste0("font-size:2.1rem; color:", NAVY, ";"), sex_icon(sex)),
      theme = value_box_theme(bg = "#FFFFFF", fg = NAVY),
      class = "metric-card"
    )
  })

  # 사이드바의 "연금 수령주기(연1회/매월)" 선택에 따라, 입력한 연액(b)이 실제로
  # 얼마씩(연 1회 또는 매월) 지급되는지 사이드바에서 바로 보여줍니다.
  output$an_payout_hint <- renderUI({
    b <- input$an_benefit
    freq <- suppressWarnings(as.numeric(input$an_payoutfreq))
    if (is.na(b) || b <= 0 || is.na(freq)) return(NULL)
    won <- b * 10000
    txt <- if (freq == 1) {
      paste0("연 1회 수령 선택 — 매년 ", fmt_won(won), "씩 받습니다.")
    } else {
      paste0("매월 수령 선택 — 매달 ", fmt_won(round(won / 12)), "씩 받습니다. (연 ", fmt_won(won), " 기준 1/12)")
    }
    div(class = "payout-hint", txt)
  })

  an_curve <- reactive({
    res <- an_results()
    age <- input$an_age; n_pay <- input$an_pay; defer <- input$an_start - input$an_age
    annual_benefit_won <- input$an_benefit * 10000

    ages <- (age + 1):MAX_AGE
    years_paid <- pmin(pmax(ages - age, 0), n_pay)
    years_received <- pmax(ages - (age + defer), 0)
    benefit_received <- years_received * annual_benefit_won
    premium <- years_paid * res$annual

    idx <- which(benefit_received >= premium & benefit_received > 0)
    be <- if (length(idx) == 0) NA_integer_ else ages[min(idx)]
    list(ages = ages, benefit_received = benefit_received, premium = premium, breakeven = be)
  })

  output$an_metric_strip <- renderUI({
    res <- an_results(); cv <- an_curve()
    total_paid <- res$annual * input$an_pay
    div(class = "metric-strip",
        div(class = "metric-chip accent",
            span(class = "metric-chip-label", "원금회수 연령"),
            span(class = "metric-chip-value",
                 if (is.na(cv$breakeven)) "생애 내 미도달" else paste0(cv$breakeven, "세"))),
        div(class = "metric-chip",
            span(class = "metric-chip-label", paste0(input$an_pay, "년간 총 납입액")),
            span(class = "metric-chip-value", fmt_won(total_paid))),
        div(class = "metric-chip",
            span(class = "metric-chip-label", "연금 수령액(연액)"),
            span(class = "metric-chip-value", fmt_won(input$an_benefit * 10000)))
    )
  })

  output$an_plot <- renderPlotly({
    cv <- an_curve()
    y_max <- max(cv$benefit_received, cv$premium, na.rm = TRUE)

    p <- plot_ly()
    p <- add_lines(p, x = cv$ages, y = cv$benefit_received, name = "누적연금수령액",
                    line = list(color = NAVY, width = 2.2),
                    hovertemplate = "%{x}세: %{y:,.0f}원<extra>누적연금수령액</extra>")
    p <- add_lines(p, x = cv$ages, y = cv$premium, name = "누적납입액",
                    line = list(color = GOLD, dash = "dash", width = 2.2),
                    hovertemplate = "%{x}세: %{y:,.0f}원<extra>누적납입액</extra>")

    shapes <- list()
    if (!is.na(cv$breakeven)) shapes <- list(list(type = "line", x0 = cv$breakeven, x1 = cv$breakeven, y0 = 0, y1 = y_max,
                                                    line = list(color = GOLD, dash = "dot", width = 1.4)))

    layout(p,
           xaxis = list(title = "나이", gridcolor = CARD_BD, zeroline = FALSE),
           yaxis = list(title = "금액(원)", gridcolor = CARD_BD, zeroline = FALSE),
           shapes = shapes,
           paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
           font = list(color = INK, family = "Noto Sans KR, sans-serif"),
           legend = list(orientation = "h", y = -0.2),
           margin = list(t = 10))
  })

  output$an_table <- renderDT({ make_life_table_dt() })
}

rsconnect::deployApp(
  ".",
  appFiles = c("app.R", "data/life_table.csv"),
  forceUpdate = TRUE
)

shinyApp(ui, server)
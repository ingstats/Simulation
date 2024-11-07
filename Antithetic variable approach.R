# <Variance Reduction Techniques>
# Method 1: Antithetic variable approach
#   
#   X₁, X₂: 평균이  theta인 random variable (θ = E[X])
# 
# Var( (X₁ + X₂) / 2 ) = (1/4)[Var(X₁) + Var(X₂) + 2Cov(X₁, X₂)]
# * Cov(X₁, X₂) < 0 일 때 분산 감소
# * 두 rv가 음의 상관관계를 가지도록 배열
# X₁ = h(U₁, ..., Uₘ)
# X₂ = h(1 - U₁, ..., 1 - Uₘ)
# 
# - Probs
# 1. h가 각 좌표에 대해 단조함수일 때 Cov(X₁, X₂) < 0
# ⇒ θ 추정량인 (X₁ + X₂)/2 의 분산 감소
# 2. 균일난수 재활용 → 계산시간 감소
# (두 번째 난수 집합을 새로 생성할 필요 X)
# 
# ___________________________________________________________________
# 
# - Structure function (구조 함수)
#   φ(S₁, ..., Sₙ) = {
#     1, Sᵢ = 1
#     0, otherwise
#   }
# 
# S: state vector 
# Sᵢ = {
#   1, if component i works
#   0, otherwise
# }
# 
# (1) Series structure (직렬)
# φ(S₁, ..., Sₙ) = Min{Sᵢ}
# → 모든 component 작동 시 system 작동.
# 
# (2) Parallel structure (병렬)
# φ(S₁, ..., Sₙ) = Max{Sᵢ}
# → component 1개 이상 작동 시 system 작동.
# 
# (3) k-of-n system.
# φ(S₁, ..., Sₙ) = {
#   1, k개 이상 작동 시
#   0, otherwise
# }
# 
# (4) Bridge structure.
# φ(S₁, S₂, S₃, S₄, S₅) = Max(S₁S₃S₅, S₂S₃S₄, S₁S₄, S₂S₅)
# 
# 
# - Reliability function (신뢰도 함수)
#   Let pᵢ = P(Sᵢ = 1)
# r(p₁, ..., pₙ) = P{φ(S₁, ..., Sₙ) = 1}
# = E[φ(S₁, ..., Sₙ)]
# 
# * 직렬, 병렬 시스템의 신뢰도 함수는 구할 수 있음
# * k-of-n, bridge 시스템은 변형 → antithetic variable approach 적용
# 
# ___________________________________________________________________
# 
# * r(p₁, ..., pₘ) = E[φ(S₁, ..., Sₘ)]를 antithetic variable approach를 적용해서 보다 효율적으로 추정하는 방법:
#   
#   [Step 1]
# Sᵢ = {
#   1, Uᵢ < pᵢ
#   0, otherwise
# }
# X₁ = φ(S₁, ..., Sₙ)
# = h(U₁, ..., Uₘ)
# 
# [STEP 2]   
# Sᵢ = {
#   1, 1 - Uᵢ < pᵢ
#   0, otherwise
# }
# X₂ = φ(S₁, ..., Sₙ)
# = h(1 - U₁, ..., 1 - Uₘ)
# 
# [STEP 3]
# STEP 1과 STEP 2를 n/2번 반복해서 얻은 X₁, X₂, ..., Xₙ을 이용해서
# r(p₁, ..., pₘ) = E[φ]를 추정.
# 
# ___________________________________________________________________
# bridge system의 reliability function을 모의실험으로 추정
# antithetic variable approach 적용 여부에 따른 차이?
ni = 1000
p = rep(0.5, 5) # Si=1일 확률
phi = function(s) max(s[1]*s[3]*s[5], s[2]*s[3]*s[4], s[1]*s[4], s[2]*s[3]) 
# phi: bridge structure
x = w = numeric(ni)

for (i in seq(1,ni,2)){
  u = runif(5)
  # 대조변수 사용
  s = as.numeric(u<p); x[i] = phi(s)
  s = as.numeric(1-u<p); x[i+1] = phi(s)
  # 대조변수 사용 안 함
  w[i] = x[i]; w[i+1] = phi(as.numeric(runif(5)<p))
}

# 대조변수 이용한 추정량 분산
y = numeric(0.5*ni)
for (i in 1:(0.5*ni)) y[i] = (x[2*i-1]+x[2*i])/2
c(mean(x), sd(y)/sqrt(0.5*ni)) # 대조변수 썼을 때
c(mean(w), sd(w)/sqrt(ni)) # 대조변수 안 썼을 때

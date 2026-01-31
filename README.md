r
# ============================================
# Kriging Prediction Output Handling
# مدیریت خروجی پیش‌بینی کریجینگ
# ============================================

# ایجاد لیست نتایج با مقدار پیش‌بینی شده y
res <- list(y = f)

# بررسی آیا انحراف معیار (s) یا بهبود مورد انتظار (ei) مورد نیاز است
if (any(object$target %in% c("s", "ei"))) {
  
  # محاسبه وارون ماتریس PsiB با کنترل خطا
  Psinv <- try(solve.default(PsiB), TRUE)
  if (class(Psinv)[1] == "try-error") {
    Psinv <- ginv(PsiB)  # استفاده از وارون تعمیم یافته در صورت خطا
  }
  
  # محاسبه مربع انحراف معیار
  SSqr <- SigmaSqr * (1 - diag(psi %*% (Psinv %*% t(psi))))
  s <- sqrt(abs(SSqr))  # انحراف معیار
  res$s <- s
  
  # اگر بهبود مورد انتظار (EI) مورد نیاز است
  if (any(object$target == "ei")) {
    res$ei <- expectedImprovement(f, s, object$min)
  }
}

# اگر بازگرداندن ماتریس همبستگی متقاطع درخواست شده است
if (object$returnCrossCor) {
  res$psi <- psi
}

# بازگرداندن نتایج نهایی
res

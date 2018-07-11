
    val fa = curryT(toLocalDate)
    val now   = LocalDateTime.now
    val mapFrom  = mapL[String,LocalDateTime](fa, { d => min(now,d).following24Hours })_
    val mapTo = mapR[String,LocalDateTime]({ d => min(now,d).prior24Hours }, fa)_
    
    val (left, right) = map2(from, to, fa)
      .map(ordered)
      .orElse(mapFrom(from))
      .orElse(mapTo(to))
      .getOrElse(now.prior24Hours -> now)

      (clamp(???,???,left).toDate, clamp(???,???,right).toDate)

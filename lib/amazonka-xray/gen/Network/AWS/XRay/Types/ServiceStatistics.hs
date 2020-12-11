-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ServiceStatistics
  ( ServiceStatistics (..),

    -- * Smart constructor
    mkServiceStatistics,

    -- * Lenses
    ssFaultStatistics,
    ssOKCount,
    ssTotalResponseTime,
    ssErrorStatistics,
    ssTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultStatistics

-- | Response statistics for a service.
--
-- /See:/ 'mkServiceStatistics' smart constructor.
data ServiceStatistics = ServiceStatistics'
  { faultStatistics ::
      Lude.Maybe FaultStatistics,
    okCount :: Lude.Maybe Lude.Integer,
    totalResponseTime :: Lude.Maybe Lude.Double,
    errorStatistics :: Lude.Maybe ErrorStatistics,
    totalCount :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceStatistics' with the minimum fields required to make a request.
--
-- * 'errorStatistics' - Information about requests that failed with a 4xx Client Error status code.
-- * 'faultStatistics' - Information about requests that failed with a 5xx Server Error status code.
-- * 'okCount' - The number of requests that completed with a 2xx Success status code.
-- * 'totalCount' - The total number of completed requests.
-- * 'totalResponseTime' - The aggregate response time of completed requests.
mkServiceStatistics ::
  ServiceStatistics
mkServiceStatistics =
  ServiceStatistics'
    { faultStatistics = Lude.Nothing,
      okCount = Lude.Nothing,
      totalResponseTime = Lude.Nothing,
      errorStatistics = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | Information about requests that failed with a 5xx Server Error status code.
--
-- /Note:/ Consider using 'faultStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssFaultStatistics :: Lens.Lens' ServiceStatistics (Lude.Maybe FaultStatistics)
ssFaultStatistics = Lens.lens (faultStatistics :: ServiceStatistics -> Lude.Maybe FaultStatistics) (\s a -> s {faultStatistics = a} :: ServiceStatistics)
{-# DEPRECATED ssFaultStatistics "Use generic-lens or generic-optics with 'faultStatistics' instead." #-}

-- | The number of requests that completed with a 2xx Success status code.
--
-- /Note:/ Consider using 'okCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOKCount :: Lens.Lens' ServiceStatistics (Lude.Maybe Lude.Integer)
ssOKCount = Lens.lens (okCount :: ServiceStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {okCount = a} :: ServiceStatistics)
{-# DEPRECATED ssOKCount "Use generic-lens or generic-optics with 'okCount' instead." #-}

-- | The aggregate response time of completed requests.
--
-- /Note:/ Consider using 'totalResponseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTotalResponseTime :: Lens.Lens' ServiceStatistics (Lude.Maybe Lude.Double)
ssTotalResponseTime = Lens.lens (totalResponseTime :: ServiceStatistics -> Lude.Maybe Lude.Double) (\s a -> s {totalResponseTime = a} :: ServiceStatistics)
{-# DEPRECATED ssTotalResponseTime "Use generic-lens or generic-optics with 'totalResponseTime' instead." #-}

-- | Information about requests that failed with a 4xx Client Error status code.
--
-- /Note:/ Consider using 'errorStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssErrorStatistics :: Lens.Lens' ServiceStatistics (Lude.Maybe ErrorStatistics)
ssErrorStatistics = Lens.lens (errorStatistics :: ServiceStatistics -> Lude.Maybe ErrorStatistics) (\s a -> s {errorStatistics = a} :: ServiceStatistics)
{-# DEPRECATED ssErrorStatistics "Use generic-lens or generic-optics with 'errorStatistics' instead." #-}

-- | The total number of completed requests.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTotalCount :: Lens.Lens' ServiceStatistics (Lude.Maybe Lude.Integer)
ssTotalCount = Lens.lens (totalCount :: ServiceStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalCount = a} :: ServiceStatistics)
{-# DEPRECATED ssTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON ServiceStatistics where
  parseJSON =
    Lude.withObject
      "ServiceStatistics"
      ( \x ->
          ServiceStatistics'
            Lude.<$> (x Lude..:? "FaultStatistics")
            Lude.<*> (x Lude..:? "OkCount")
            Lude.<*> (x Lude..:? "TotalResponseTime")
            Lude.<*> (x Lude..:? "ErrorStatistics")
            Lude.<*> (x Lude..:? "TotalCount")
      )

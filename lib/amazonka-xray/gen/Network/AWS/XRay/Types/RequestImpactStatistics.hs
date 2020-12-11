-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.RequestImpactStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.RequestImpactStatistics
  ( RequestImpactStatistics (..),

    -- * Smart constructor
    mkRequestImpactStatistics,

    -- * Lenses
    risOKCount,
    risFaultCount,
    risTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statistics that describe how the incident has impacted a service.
--
-- /See:/ 'mkRequestImpactStatistics' smart constructor.
data RequestImpactStatistics = RequestImpactStatistics'
  { okCount ::
      Lude.Maybe Lude.Integer,
    faultCount :: Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'RequestImpactStatistics' with the minimum fields required to make a request.
--
-- * 'faultCount' - The number of requests that have resulted in a fault,
-- * 'okCount' - The number of successful requests.
-- * 'totalCount' - The total number of requests to the service.
mkRequestImpactStatistics ::
  RequestImpactStatistics
mkRequestImpactStatistics =
  RequestImpactStatistics'
    { okCount = Lude.Nothing,
      faultCount = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | The number of successful requests.
--
-- /Note:/ Consider using 'okCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risOKCount :: Lens.Lens' RequestImpactStatistics (Lude.Maybe Lude.Integer)
risOKCount = Lens.lens (okCount :: RequestImpactStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {okCount = a} :: RequestImpactStatistics)
{-# DEPRECATED risOKCount "Use generic-lens or generic-optics with 'okCount' instead." #-}

-- | The number of requests that have resulted in a fault,
--
-- /Note:/ Consider using 'faultCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risFaultCount :: Lens.Lens' RequestImpactStatistics (Lude.Maybe Lude.Integer)
risFaultCount = Lens.lens (faultCount :: RequestImpactStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {faultCount = a} :: RequestImpactStatistics)
{-# DEPRECATED risFaultCount "Use generic-lens or generic-optics with 'faultCount' instead." #-}

-- | The total number of requests to the service.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risTotalCount :: Lens.Lens' RequestImpactStatistics (Lude.Maybe Lude.Integer)
risTotalCount = Lens.lens (totalCount :: RequestImpactStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalCount = a} :: RequestImpactStatistics)
{-# DEPRECATED risTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON RequestImpactStatistics where
  parseJSON =
    Lude.withObject
      "RequestImpactStatistics"
      ( \x ->
          RequestImpactStatistics'
            Lude.<$> (x Lude..:? "OkCount")
            Lude.<*> (x Lude..:? "FaultCount")
            Lude.<*> (x Lude..:? "TotalCount")
      )

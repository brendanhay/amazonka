{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.RequestImpactStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.RequestImpactStatistics
  ( RequestImpactStatistics (..)
  -- * Smart constructor
  , mkRequestImpactStatistics
  -- * Lenses
  , risFaultCount
  , risOkCount
  , risTotalCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Statistics that describe how the incident has impacted a service.
--
-- /See:/ 'mkRequestImpactStatistics' smart constructor.
data RequestImpactStatistics = RequestImpactStatistics'
  { faultCount :: Core.Maybe Core.Integer
    -- ^ The number of requests that have resulted in a fault,
  , okCount :: Core.Maybe Core.Integer
    -- ^ The number of successful requests.
  , totalCount :: Core.Maybe Core.Integer
    -- ^ The total number of requests to the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestImpactStatistics' value with any optional fields omitted.
mkRequestImpactStatistics
    :: RequestImpactStatistics
mkRequestImpactStatistics
  = RequestImpactStatistics'{faultCount = Core.Nothing,
                             okCount = Core.Nothing, totalCount = Core.Nothing}

-- | The number of requests that have resulted in a fault,
--
-- /Note:/ Consider using 'faultCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risFaultCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
risFaultCount = Lens.field @"faultCount"
{-# INLINEABLE risFaultCount #-}
{-# DEPRECATED faultCount "Use generic-lens or generic-optics with 'faultCount' instead"  #-}

-- | The number of successful requests.
--
-- /Note:/ Consider using 'okCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risOkCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
risOkCount = Lens.field @"okCount"
{-# INLINEABLE risOkCount #-}
{-# DEPRECATED okCount "Use generic-lens or generic-optics with 'okCount' instead"  #-}

-- | The total number of requests to the service.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
risTotalCount :: Lens.Lens' RequestImpactStatistics (Core.Maybe Core.Integer)
risTotalCount = Lens.field @"totalCount"
{-# INLINEABLE risTotalCount #-}
{-# DEPRECATED totalCount "Use generic-lens or generic-optics with 'totalCount' instead"  #-}

instance Core.FromJSON RequestImpactStatistics where
        parseJSON
          = Core.withObject "RequestImpactStatistics" Core.$
              \ x ->
                RequestImpactStatistics' Core.<$>
                  (x Core..:? "FaultCount") Core.<*> x Core..:? "OkCount" Core.<*>
                    x Core..:? "TotalCount"

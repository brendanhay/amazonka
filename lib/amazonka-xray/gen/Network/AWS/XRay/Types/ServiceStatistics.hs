{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ServiceStatistics
  ( ServiceStatistics (..)
  -- * Smart constructor
  , mkServiceStatistics
  -- * Lenses
  , ssErrorStatistics
  , ssFaultStatistics
  , ssOkCount
  , ssTotalCount
  , ssTotalResponseTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ErrorStatistics as Types
import qualified Network.AWS.XRay.Types.FaultStatistics as Types

-- | Response statistics for a service.
--
-- /See:/ 'mkServiceStatistics' smart constructor.
data ServiceStatistics = ServiceStatistics'
  { errorStatistics :: Core.Maybe Types.ErrorStatistics
    -- ^ Information about requests that failed with a 4xx Client Error status code.
  , faultStatistics :: Core.Maybe Types.FaultStatistics
    -- ^ Information about requests that failed with a 5xx Server Error status code.
  , okCount :: Core.Maybe Core.Integer
    -- ^ The number of requests that completed with a 2xx Success status code.
  , totalCount :: Core.Maybe Core.Integer
    -- ^ The total number of completed requests.
  , totalResponseTime :: Core.Maybe Core.Double
    -- ^ The aggregate response time of completed requests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceStatistics' value with any optional fields omitted.
mkServiceStatistics
    :: ServiceStatistics
mkServiceStatistics
  = ServiceStatistics'{errorStatistics = Core.Nothing,
                       faultStatistics = Core.Nothing, okCount = Core.Nothing,
                       totalCount = Core.Nothing, totalResponseTime = Core.Nothing}

-- | Information about requests that failed with a 4xx Client Error status code.
--
-- /Note:/ Consider using 'errorStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssErrorStatistics :: Lens.Lens' ServiceStatistics (Core.Maybe Types.ErrorStatistics)
ssErrorStatistics = Lens.field @"errorStatistics"
{-# INLINEABLE ssErrorStatistics #-}
{-# DEPRECATED errorStatistics "Use generic-lens or generic-optics with 'errorStatistics' instead"  #-}

-- | Information about requests that failed with a 5xx Server Error status code.
--
-- /Note:/ Consider using 'faultStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssFaultStatistics :: Lens.Lens' ServiceStatistics (Core.Maybe Types.FaultStatistics)
ssFaultStatistics = Lens.field @"faultStatistics"
{-# INLINEABLE ssFaultStatistics #-}
{-# DEPRECATED faultStatistics "Use generic-lens or generic-optics with 'faultStatistics' instead"  #-}

-- | The number of requests that completed with a 2xx Success status code.
--
-- /Note:/ Consider using 'okCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOkCount :: Lens.Lens' ServiceStatistics (Core.Maybe Core.Integer)
ssOkCount = Lens.field @"okCount"
{-# INLINEABLE ssOkCount #-}
{-# DEPRECATED okCount "Use generic-lens or generic-optics with 'okCount' instead"  #-}

-- | The total number of completed requests.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTotalCount :: Lens.Lens' ServiceStatistics (Core.Maybe Core.Integer)
ssTotalCount = Lens.field @"totalCount"
{-# INLINEABLE ssTotalCount #-}
{-# DEPRECATED totalCount "Use generic-lens or generic-optics with 'totalCount' instead"  #-}

-- | The aggregate response time of completed requests.
--
-- /Note:/ Consider using 'totalResponseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTotalResponseTime :: Lens.Lens' ServiceStatistics (Core.Maybe Core.Double)
ssTotalResponseTime = Lens.field @"totalResponseTime"
{-# INLINEABLE ssTotalResponseTime #-}
{-# DEPRECATED totalResponseTime "Use generic-lens or generic-optics with 'totalResponseTime' instead"  #-}

instance Core.FromJSON ServiceStatistics where
        parseJSON
          = Core.withObject "ServiceStatistics" Core.$
              \ x ->
                ServiceStatistics' Core.<$>
                  (x Core..:? "ErrorStatistics") Core.<*>
                    x Core..:? "FaultStatistics"
                    Core.<*> x Core..:? "OkCount"
                    Core.<*> x Core..:? "TotalCount"
                    Core.<*> x Core..:? "TotalResponseTime"

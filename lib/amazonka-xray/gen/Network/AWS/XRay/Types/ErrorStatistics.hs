{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ErrorStatistics
  ( ErrorStatistics (..)
  -- * Smart constructor
  , mkErrorStatistics
  -- * Lenses
  , eOtherCount
  , eThrottleCount
  , eTotalCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about requests that failed with a 4xx Client Error status code.
--
-- /See:/ 'mkErrorStatistics' smart constructor.
data ErrorStatistics = ErrorStatistics'
  { otherCount :: Core.Maybe Core.Integer
    -- ^ The number of requests that failed with untracked 4xx Client Error status codes.
  , throttleCount :: Core.Maybe Core.Integer
    -- ^ The number of requests that failed with a 419 throttling status code.
  , totalCount :: Core.Maybe Core.Integer
    -- ^ The total number of requests that failed with a 4xx Client Error status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorStatistics' value with any optional fields omitted.
mkErrorStatistics
    :: ErrorStatistics
mkErrorStatistics
  = ErrorStatistics'{otherCount = Core.Nothing,
                     throttleCount = Core.Nothing, totalCount = Core.Nothing}

-- | The number of requests that failed with untracked 4xx Client Error status codes.
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOtherCount :: Lens.Lens' ErrorStatistics (Core.Maybe Core.Integer)
eOtherCount = Lens.field @"otherCount"
{-# INLINEABLE eOtherCount #-}
{-# DEPRECATED otherCount "Use generic-lens or generic-optics with 'otherCount' instead"  #-}

-- | The number of requests that failed with a 419 throttling status code.
--
-- /Note:/ Consider using 'throttleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eThrottleCount :: Lens.Lens' ErrorStatistics (Core.Maybe Core.Integer)
eThrottleCount = Lens.field @"throttleCount"
{-# INLINEABLE eThrottleCount #-}
{-# DEPRECATED throttleCount "Use generic-lens or generic-optics with 'throttleCount' instead"  #-}

-- | The total number of requests that failed with a 4xx Client Error status code.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTotalCount :: Lens.Lens' ErrorStatistics (Core.Maybe Core.Integer)
eTotalCount = Lens.field @"totalCount"
{-# INLINEABLE eTotalCount #-}
{-# DEPRECATED totalCount "Use generic-lens or generic-optics with 'totalCount' instead"  #-}

instance Core.FromJSON ErrorStatistics where
        parseJSON
          = Core.withObject "ErrorStatistics" Core.$
              \ x ->
                ErrorStatistics' Core.<$>
                  (x Core..:? "OtherCount") Core.<*> x Core..:? "ThrottleCount"
                    Core.<*> x Core..:? "TotalCount"

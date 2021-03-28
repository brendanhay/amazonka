{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.ThrottleSettings
  ( ThrottleSettings (..)
  -- * Smart constructor
  , mkThrottleSettings
  -- * Lenses
  , tsBurstLimit
  , tsRateLimit
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The API request rate limits.
--
-- /See:/ 'mkThrottleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { burstLimit :: Core.Maybe Core.Int
    -- ^ The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
  , rateLimit :: Core.Maybe Core.Double
    -- ^ The API request steady-state rate limit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThrottleSettings' value with any optional fields omitted.
mkThrottleSettings
    :: ThrottleSettings
mkThrottleSettings
  = ThrottleSettings'{burstLimit = Core.Nothing,
                      rateLimit = Core.Nothing}

-- | The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
--
-- /Note:/ Consider using 'burstLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsBurstLimit :: Lens.Lens' ThrottleSettings (Core.Maybe Core.Int)
tsBurstLimit = Lens.field @"burstLimit"
{-# INLINEABLE tsBurstLimit #-}
{-# DEPRECATED burstLimit "Use generic-lens or generic-optics with 'burstLimit' instead"  #-}

-- | The API request steady-state rate limit.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRateLimit :: Lens.Lens' ThrottleSettings (Core.Maybe Core.Double)
tsRateLimit = Lens.field @"rateLimit"
{-# INLINEABLE tsRateLimit #-}
{-# DEPRECATED rateLimit "Use generic-lens or generic-optics with 'rateLimit' instead"  #-}

instance Core.FromJSON ThrottleSettings where
        toJSON ThrottleSettings{..}
          = Core.object
              (Core.catMaybes
                 [("burstLimit" Core..=) Core.<$> burstLimit,
                  ("rateLimit" Core..=) Core.<$> rateLimit])

instance Core.FromJSON ThrottleSettings where
        parseJSON
          = Core.withObject "ThrottleSettings" Core.$
              \ x ->
                ThrottleSettings' Core.<$>
                  (x Core..:? "burstLimit") Core.<*> x Core..:? "rateLimit"

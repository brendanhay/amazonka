{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
  ( ContainerServiceHealthCheckConfig (..)
  -- * Smart constructor
  , mkContainerServiceHealthCheckConfig
  -- * Lenses
  , cshccHealthyThreshold
  , cshccIntervalSeconds
  , cshccPath
  , cshccSuccessCodes
  , cshccTimeoutSeconds
  , cshccUnhealthyThreshold
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the health check configuration of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceHealthCheckConfig' smart constructor.
data ContainerServiceHealthCheckConfig = ContainerServiceHealthCheckConfig'
  { healthyThreshold :: Core.Maybe Core.Int
    -- ^ The number of consecutive health checks successes required before moving the container to the @Healthy@ state.
  , intervalSeconds :: Core.Maybe Core.Int
    -- ^ The approximate interval, in seconds, between health checks of an individual container. You may specify between 5 and 300 seconds.
  , path :: Core.Maybe Core.Text
    -- ^ The path on the container on which to perform the health check.
  , successCodes :: Core.Maybe Core.Text
    -- ^ The HTTP codes to use when checking for a successful response from a container. You can specify values between 200 and 499.
  , timeoutSeconds :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, during which no response means a failed health check. You may specify between 2 and 60 seconds.
  , unhealthyThreshold :: Core.Maybe Core.Int
    -- ^ The number of consecutive health check failures required before moving the container to the @Unhealthy@ state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerServiceHealthCheckConfig' value with any optional fields omitted.
mkContainerServiceHealthCheckConfig
    :: ContainerServiceHealthCheckConfig
mkContainerServiceHealthCheckConfig
  = ContainerServiceHealthCheckConfig'{healthyThreshold =
                                         Core.Nothing,
                                       intervalSeconds = Core.Nothing, path = Core.Nothing,
                                       successCodes = Core.Nothing, timeoutSeconds = Core.Nothing,
                                       unhealthyThreshold = Core.Nothing}

-- | The number of consecutive health checks successes required before moving the container to the @Healthy@ state.
--
-- /Note:/ Consider using 'healthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccHealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
cshccHealthyThreshold = Lens.field @"healthyThreshold"
{-# INLINEABLE cshccHealthyThreshold #-}
{-# DEPRECATED healthyThreshold "Use generic-lens or generic-optics with 'healthyThreshold' instead"  #-}

-- | The approximate interval, in seconds, between health checks of an individual container. You may specify between 5 and 300 seconds.
--
-- /Note:/ Consider using 'intervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccIntervalSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
cshccIntervalSeconds = Lens.field @"intervalSeconds"
{-# INLINEABLE cshccIntervalSeconds #-}
{-# DEPRECATED intervalSeconds "Use generic-lens or generic-optics with 'intervalSeconds' instead"  #-}

-- | The path on the container on which to perform the health check.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccPath :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Text)
cshccPath = Lens.field @"path"
{-# INLINEABLE cshccPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The HTTP codes to use when checking for a successful response from a container. You can specify values between 200 and 499.
--
-- /Note:/ Consider using 'successCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccSuccessCodes :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Text)
cshccSuccessCodes = Lens.field @"successCodes"
{-# INLINEABLE cshccSuccessCodes #-}
{-# DEPRECATED successCodes "Use generic-lens or generic-optics with 'successCodes' instead"  #-}

-- | The amount of time, in seconds, during which no response means a failed health check. You may specify between 2 and 60 seconds.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccTimeoutSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
cshccTimeoutSeconds = Lens.field @"timeoutSeconds"
{-# INLINEABLE cshccTimeoutSeconds #-}
{-# DEPRECATED timeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead"  #-}

-- | The number of consecutive health check failures required before moving the container to the @Unhealthy@ state.
--
-- /Note:/ Consider using 'unhealthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccUnhealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
cshccUnhealthyThreshold = Lens.field @"unhealthyThreshold"
{-# INLINEABLE cshccUnhealthyThreshold #-}
{-# DEPRECATED unhealthyThreshold "Use generic-lens or generic-optics with 'unhealthyThreshold' instead"  #-}

instance Core.FromJSON ContainerServiceHealthCheckConfig where
        toJSON ContainerServiceHealthCheckConfig{..}
          = Core.object
              (Core.catMaybes
                 [("healthyThreshold" Core..=) Core.<$> healthyThreshold,
                  ("intervalSeconds" Core..=) Core.<$> intervalSeconds,
                  ("path" Core..=) Core.<$> path,
                  ("successCodes" Core..=) Core.<$> successCodes,
                  ("timeoutSeconds" Core..=) Core.<$> timeoutSeconds,
                  ("unhealthyThreshold" Core..=) Core.<$> unhealthyThreshold])

instance Core.FromJSON ContainerServiceHealthCheckConfig where
        parseJSON
          = Core.withObject "ContainerServiceHealthCheckConfig" Core.$
              \ x ->
                ContainerServiceHealthCheckConfig' Core.<$>
                  (x Core..:? "healthyThreshold") Core.<*>
                    x Core..:? "intervalSeconds"
                    Core.<*> x Core..:? "path"
                    Core.<*> x Core..:? "successCodes"
                    Core.<*> x Core..:? "timeoutSeconds"
                    Core.<*> x Core..:? "unhealthyThreshold"

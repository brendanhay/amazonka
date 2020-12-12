{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
  ( ContainerServiceHealthCheckConfig (..),

    -- * Smart constructor
    mkContainerServiceHealthCheckConfig,

    -- * Lenses
    cshccHealthyThreshold,
    cshccPath,
    cshccSuccessCodes,
    cshccIntervalSeconds,
    cshccTimeoutSeconds,
    cshccUnhealthyThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the health check configuration of an Amazon Lightsail container service.
--
-- /See:/ 'mkContainerServiceHealthCheckConfig' smart constructor.
data ContainerServiceHealthCheckConfig = ContainerServiceHealthCheckConfig'
  { healthyThreshold ::
      Lude.Maybe Lude.Int,
    path ::
      Lude.Maybe Lude.Text,
    successCodes ::
      Lude.Maybe Lude.Text,
    intervalSeconds ::
      Lude.Maybe Lude.Int,
    timeoutSeconds ::
      Lude.Maybe Lude.Int,
    unhealthyThreshold ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServiceHealthCheckConfig' with the minimum fields required to make a request.
--
-- * 'healthyThreshold' - The number of consecutive health checks successes required before moving the container to the @Healthy@ state.
-- * 'intervalSeconds' - The approximate interval, in seconds, between health checks of an individual container. You may specify between 5 and 300 seconds.
-- * 'path' - The path on the container on which to perform the health check.
-- * 'successCodes' - The HTTP codes to use when checking for a successful response from a container. You can specify values between 200 and 499.
-- * 'timeoutSeconds' - The amount of time, in seconds, during which no response means a failed health check. You may specify between 2 and 60 seconds.
-- * 'unhealthyThreshold' - The number of consecutive health check failures required before moving the container to the @Unhealthy@ state.
mkContainerServiceHealthCheckConfig ::
  ContainerServiceHealthCheckConfig
mkContainerServiceHealthCheckConfig =
  ContainerServiceHealthCheckConfig'
    { healthyThreshold =
        Lude.Nothing,
      path = Lude.Nothing,
      successCodes = Lude.Nothing,
      intervalSeconds = Lude.Nothing,
      timeoutSeconds = Lude.Nothing,
      unhealthyThreshold = Lude.Nothing
    }

-- | The number of consecutive health checks successes required before moving the container to the @Healthy@ state.
--
-- /Note:/ Consider using 'healthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccHealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Int)
cshccHealthyThreshold = Lens.lens (healthyThreshold :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Int) (\s a -> s {healthyThreshold = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccHealthyThreshold "Use generic-lens or generic-optics with 'healthyThreshold' instead." #-}

-- | The path on the container on which to perform the health check.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccPath :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Text)
cshccPath = Lens.lens (path :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The HTTP codes to use when checking for a successful response from a container. You can specify values between 200 and 499.
--
-- /Note:/ Consider using 'successCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccSuccessCodes :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Text)
cshccSuccessCodes = Lens.lens (successCodes :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Text) (\s a -> s {successCodes = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccSuccessCodes "Use generic-lens or generic-optics with 'successCodes' instead." #-}

-- | The approximate interval, in seconds, between health checks of an individual container. You may specify between 5 and 300 seconds.
--
-- /Note:/ Consider using 'intervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccIntervalSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Int)
cshccIntervalSeconds = Lens.lens (intervalSeconds :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Int) (\s a -> s {intervalSeconds = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccIntervalSeconds "Use generic-lens or generic-optics with 'intervalSeconds' instead." #-}

-- | The amount of time, in seconds, during which no response means a failed health check. You may specify between 2 and 60 seconds.
--
-- /Note:/ Consider using 'timeoutSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccTimeoutSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Int)
cshccTimeoutSeconds = Lens.lens (timeoutSeconds :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Int) (\s a -> s {timeoutSeconds = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccTimeoutSeconds "Use generic-lens or generic-optics with 'timeoutSeconds' instead." #-}

-- | The number of consecutive health check failures required before moving the container to the @Unhealthy@ state.
--
-- /Note:/ Consider using 'unhealthyThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cshccUnhealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Lude.Maybe Lude.Int)
cshccUnhealthyThreshold = Lens.lens (unhealthyThreshold :: ContainerServiceHealthCheckConfig -> Lude.Maybe Lude.Int) (\s a -> s {unhealthyThreshold = a} :: ContainerServiceHealthCheckConfig)
{-# DEPRECATED cshccUnhealthyThreshold "Use generic-lens or generic-optics with 'unhealthyThreshold' instead." #-}

instance Lude.FromJSON ContainerServiceHealthCheckConfig where
  parseJSON =
    Lude.withObject
      "ContainerServiceHealthCheckConfig"
      ( \x ->
          ContainerServiceHealthCheckConfig'
            Lude.<$> (x Lude..:? "healthyThreshold")
            Lude.<*> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "successCodes")
            Lude.<*> (x Lude..:? "intervalSeconds")
            Lude.<*> (x Lude..:? "timeoutSeconds")
            Lude.<*> (x Lude..:? "unhealthyThreshold")
      )

instance Lude.ToJSON ContainerServiceHealthCheckConfig where
  toJSON ContainerServiceHealthCheckConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("healthyThreshold" Lude..=) Lude.<$> healthyThreshold,
            ("path" Lude..=) Lude.<$> path,
            ("successCodes" Lude..=) Lude.<$> successCodes,
            ("intervalSeconds" Lude..=) Lude.<$> intervalSeconds,
            ("timeoutSeconds" Lude..=) Lude.<$> timeoutSeconds,
            ("unhealthyThreshold" Lude..=) Lude.<$> unhealthyThreshold
          ]
      )

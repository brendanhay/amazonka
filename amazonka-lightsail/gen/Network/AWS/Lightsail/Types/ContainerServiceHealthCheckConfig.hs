{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the health check configuration of an Amazon Lightsail
-- container service.
--
-- /See:/ 'newContainerServiceHealthCheckConfig' smart constructor.
data ContainerServiceHealthCheckConfig = ContainerServiceHealthCheckConfig'
  { -- | The approximate interval, in seconds, between health checks of an
    -- individual container. You can specify between 5 and 300 seconds. The
    -- default value is @5@.
    intervalSeconds :: Core.Maybe Core.Int,
    -- | The number of consecutive health checks successes required before moving
    -- the container to the @Healthy@ state. The default value is @2@.
    healthyThreshold :: Core.Maybe Core.Int,
    -- | The number of consecutive health check failures required before moving
    -- the container to the @Unhealthy@ state. The default value is @2@.
    unhealthyThreshold :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check. You can specify between 2 and 60 seconds. The default
    -- value is @2@.
    timeoutSeconds :: Core.Maybe Core.Int,
    -- | The path on the container on which to perform the health check. The
    -- default value is @\/@.
    path :: Core.Maybe Core.Text,
    -- | The HTTP codes to use when checking for a successful response from a
    -- container. You can specify values between 200 and 499.
    successCodes :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContainerServiceHealthCheckConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intervalSeconds', 'containerServiceHealthCheckConfig_intervalSeconds' - The approximate interval, in seconds, between health checks of an
-- individual container. You can specify between 5 and 300 seconds. The
-- default value is @5@.
--
-- 'healthyThreshold', 'containerServiceHealthCheckConfig_healthyThreshold' - The number of consecutive health checks successes required before moving
-- the container to the @Healthy@ state. The default value is @2@.
--
-- 'unhealthyThreshold', 'containerServiceHealthCheckConfig_unhealthyThreshold' - The number of consecutive health check failures required before moving
-- the container to the @Unhealthy@ state. The default value is @2@.
--
-- 'timeoutSeconds', 'containerServiceHealthCheckConfig_timeoutSeconds' - The amount of time, in seconds, during which no response means a failed
-- health check. You can specify between 2 and 60 seconds. The default
-- value is @2@.
--
-- 'path', 'containerServiceHealthCheckConfig_path' - The path on the container on which to perform the health check. The
-- default value is @\/@.
--
-- 'successCodes', 'containerServiceHealthCheckConfig_successCodes' - The HTTP codes to use when checking for a successful response from a
-- container. You can specify values between 200 and 499.
newContainerServiceHealthCheckConfig ::
  ContainerServiceHealthCheckConfig
newContainerServiceHealthCheckConfig =
  ContainerServiceHealthCheckConfig'
    { intervalSeconds =
        Core.Nothing,
      healthyThreshold = Core.Nothing,
      unhealthyThreshold = Core.Nothing,
      timeoutSeconds = Core.Nothing,
      path = Core.Nothing,
      successCodes = Core.Nothing
    }

-- | The approximate interval, in seconds, between health checks of an
-- individual container. You can specify between 5 and 300 seconds. The
-- default value is @5@.
containerServiceHealthCheckConfig_intervalSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
containerServiceHealthCheckConfig_intervalSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {intervalSeconds} -> intervalSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {intervalSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The number of consecutive health checks successes required before moving
-- the container to the @Healthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_healthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
containerServiceHealthCheckConfig_healthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {healthyThreshold} -> healthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {healthyThreshold = a} :: ContainerServiceHealthCheckConfig)

-- | The number of consecutive health check failures required before moving
-- the container to the @Unhealthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_unhealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
containerServiceHealthCheckConfig_unhealthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {unhealthyThreshold} -> unhealthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {unhealthyThreshold = a} :: ContainerServiceHealthCheckConfig)

-- | The amount of time, in seconds, during which no response means a failed
-- health check. You can specify between 2 and 60 seconds. The default
-- value is @2@.
containerServiceHealthCheckConfig_timeoutSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Int)
containerServiceHealthCheckConfig_timeoutSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {timeoutSeconds} -> timeoutSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {timeoutSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The path on the container on which to perform the health check. The
-- default value is @\/@.
containerServiceHealthCheckConfig_path :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Text)
containerServiceHealthCheckConfig_path = Lens.lens (\ContainerServiceHealthCheckConfig' {path} -> path) (\s@ContainerServiceHealthCheckConfig' {} a -> s {path = a} :: ContainerServiceHealthCheckConfig)

-- | The HTTP codes to use when checking for a successful response from a
-- container. You can specify values between 200 and 499.
containerServiceHealthCheckConfig_successCodes :: Lens.Lens' ContainerServiceHealthCheckConfig (Core.Maybe Core.Text)
containerServiceHealthCheckConfig_successCodes = Lens.lens (\ContainerServiceHealthCheckConfig' {successCodes} -> successCodes) (\s@ContainerServiceHealthCheckConfig' {} a -> s {successCodes = a} :: ContainerServiceHealthCheckConfig)

instance
  Core.FromJSON
    ContainerServiceHealthCheckConfig
  where
  parseJSON =
    Core.withObject
      "ContainerServiceHealthCheckConfig"
      ( \x ->
          ContainerServiceHealthCheckConfig'
            Core.<$> (x Core..:? "intervalSeconds")
            Core.<*> (x Core..:? "healthyThreshold")
            Core.<*> (x Core..:? "unhealthyThreshold")
            Core.<*> (x Core..:? "timeoutSeconds")
            Core.<*> (x Core..:? "path")
            Core.<*> (x Core..:? "successCodes")
      )

instance
  Core.Hashable
    ContainerServiceHealthCheckConfig

instance
  Core.NFData
    ContainerServiceHealthCheckConfig

instance
  Core.ToJSON
    ContainerServiceHealthCheckConfig
  where
  toJSON ContainerServiceHealthCheckConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("intervalSeconds" Core..=)
              Core.<$> intervalSeconds,
            ("healthyThreshold" Core..=)
              Core.<$> healthyThreshold,
            ("unhealthyThreshold" Core..=)
              Core.<$> unhealthyThreshold,
            ("timeoutSeconds" Core..=) Core.<$> timeoutSeconds,
            ("path" Core..=) Core.<$> path,
            ("successCodes" Core..=) Core.<$> successCodes
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the health check configuration of an Amazon Lightsail
-- container service.
--
-- /See:/ 'newContainerServiceHealthCheckConfig' smart constructor.
data ContainerServiceHealthCheckConfig = ContainerServiceHealthCheckConfig'
  { -- | The approximate interval, in seconds, between health checks of an
    -- individual container. You can specify between 5 and 300 seconds. The
    -- default value is @5@.
    intervalSeconds :: Prelude.Maybe Prelude.Int,
    -- | The number of consecutive health checks successes required before moving
    -- the container to the @Healthy@ state. The default value is @2@.
    healthyThreshold :: Prelude.Maybe Prelude.Int,
    -- | The number of consecutive health check failures required before moving
    -- the container to the @Unhealthy@ state. The default value is @2@.
    unhealthyThreshold :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check. You can specify between 2 and 60 seconds. The default
    -- value is @2@.
    timeoutSeconds :: Prelude.Maybe Prelude.Int,
    -- | The path on the container on which to perform the health check. The
    -- default value is @\/@.
    path :: Prelude.Maybe Prelude.Text,
    -- | The HTTP codes to use when checking for a successful response from a
    -- container. You can specify values between 200 and 499.
    successCodes :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      healthyThreshold = Prelude.Nothing,
      unhealthyThreshold = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      path = Prelude.Nothing,
      successCodes = Prelude.Nothing
    }

-- | The approximate interval, in seconds, between health checks of an
-- individual container. You can specify between 5 and 300 seconds. The
-- default value is @5@.
containerServiceHealthCheckConfig_intervalSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_intervalSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {intervalSeconds} -> intervalSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {intervalSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The number of consecutive health checks successes required before moving
-- the container to the @Healthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_healthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_healthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {healthyThreshold} -> healthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {healthyThreshold = a} :: ContainerServiceHealthCheckConfig)

-- | The number of consecutive health check failures required before moving
-- the container to the @Unhealthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_unhealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_unhealthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {unhealthyThreshold} -> unhealthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {unhealthyThreshold = a} :: ContainerServiceHealthCheckConfig)

-- | The amount of time, in seconds, during which no response means a failed
-- health check. You can specify between 2 and 60 seconds. The default
-- value is @2@.
containerServiceHealthCheckConfig_timeoutSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_timeoutSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {timeoutSeconds} -> timeoutSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {timeoutSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The path on the container on which to perform the health check. The
-- default value is @\/@.
containerServiceHealthCheckConfig_path :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Text)
containerServiceHealthCheckConfig_path = Lens.lens (\ContainerServiceHealthCheckConfig' {path} -> path) (\s@ContainerServiceHealthCheckConfig' {} a -> s {path = a} :: ContainerServiceHealthCheckConfig)

-- | The HTTP codes to use when checking for a successful response from a
-- container. You can specify values between 200 and 499.
containerServiceHealthCheckConfig_successCodes :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Text)
containerServiceHealthCheckConfig_successCodes = Lens.lens (\ContainerServiceHealthCheckConfig' {successCodes} -> successCodes) (\s@ContainerServiceHealthCheckConfig' {} a -> s {successCodes = a} :: ContainerServiceHealthCheckConfig)

instance
  Prelude.FromJSON
    ContainerServiceHealthCheckConfig
  where
  parseJSON =
    Prelude.withObject
      "ContainerServiceHealthCheckConfig"
      ( \x ->
          ContainerServiceHealthCheckConfig'
            Prelude.<$> (x Prelude..:? "intervalSeconds")
            Prelude.<*> (x Prelude..:? "healthyThreshold")
            Prelude.<*> (x Prelude..:? "unhealthyThreshold")
            Prelude.<*> (x Prelude..:? "timeoutSeconds")
            Prelude.<*> (x Prelude..:? "path")
            Prelude.<*> (x Prelude..:? "successCodes")
      )

instance
  Prelude.Hashable
    ContainerServiceHealthCheckConfig

instance
  Prelude.NFData
    ContainerServiceHealthCheckConfig

instance
  Prelude.ToJSON
    ContainerServiceHealthCheckConfig
  where
  toJSON ContainerServiceHealthCheckConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("intervalSeconds" Prelude..=)
              Prelude.<$> intervalSeconds,
            ("healthyThreshold" Prelude..=)
              Prelude.<$> healthyThreshold,
            ("unhealthyThreshold" Prelude..=)
              Prelude.<$> unhealthyThreshold,
            ("timeoutSeconds" Prelude..=)
              Prelude.<$> timeoutSeconds,
            ("path" Prelude..=) Prelude.<$> path,
            ("successCodes" Prelude..=)
              Prelude.<$> successCodes
          ]
      )

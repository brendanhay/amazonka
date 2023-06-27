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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceHealthCheckConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceHealthCheckConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the health check configuration of an Amazon Lightsail
-- container service.
--
-- /See:/ 'newContainerServiceHealthCheckConfig' smart constructor.
data ContainerServiceHealthCheckConfig = ContainerServiceHealthCheckConfig'
  { -- | The number of consecutive health checks successes required before moving
    -- the container to the @Healthy@ state. The default value is @2@.
    healthyThreshold :: Prelude.Maybe Prelude.Int,
    -- | The approximate interval, in seconds, between health checks of an
    -- individual container. You can specify between 5 and 300 seconds. The
    -- default value is @5@.
    intervalSeconds :: Prelude.Maybe Prelude.Int,
    -- | The path on the container on which to perform the health check. The
    -- default value is @\/@.
    path :: Prelude.Maybe Prelude.Text,
    -- | The HTTP codes to use when checking for a successful response from a
    -- container. You can specify values between @200@ and @499@. You can
    -- specify multiple values (for example, @200,202@) or a range of values
    -- (for example, @200-299@).
    successCodes :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, during which no response means a failed
    -- health check. You can specify between 2 and 60 seconds. The default
    -- value is @2@.
    timeoutSeconds :: Prelude.Maybe Prelude.Int,
    -- | The number of consecutive health check failures required before moving
    -- the container to the @Unhealthy@ state. The default value is @2@.
    unhealthyThreshold :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceHealthCheckConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthyThreshold', 'containerServiceHealthCheckConfig_healthyThreshold' - The number of consecutive health checks successes required before moving
-- the container to the @Healthy@ state. The default value is @2@.
--
-- 'intervalSeconds', 'containerServiceHealthCheckConfig_intervalSeconds' - The approximate interval, in seconds, between health checks of an
-- individual container. You can specify between 5 and 300 seconds. The
-- default value is @5@.
--
-- 'path', 'containerServiceHealthCheckConfig_path' - The path on the container on which to perform the health check. The
-- default value is @\/@.
--
-- 'successCodes', 'containerServiceHealthCheckConfig_successCodes' - The HTTP codes to use when checking for a successful response from a
-- container. You can specify values between @200@ and @499@. You can
-- specify multiple values (for example, @200,202@) or a range of values
-- (for example, @200-299@).
--
-- 'timeoutSeconds', 'containerServiceHealthCheckConfig_timeoutSeconds' - The amount of time, in seconds, during which no response means a failed
-- health check. You can specify between 2 and 60 seconds. The default
-- value is @2@.
--
-- 'unhealthyThreshold', 'containerServiceHealthCheckConfig_unhealthyThreshold' - The number of consecutive health check failures required before moving
-- the container to the @Unhealthy@ state. The default value is @2@.
newContainerServiceHealthCheckConfig ::
  ContainerServiceHealthCheckConfig
newContainerServiceHealthCheckConfig =
  ContainerServiceHealthCheckConfig'
    { healthyThreshold =
        Prelude.Nothing,
      intervalSeconds = Prelude.Nothing,
      path = Prelude.Nothing,
      successCodes = Prelude.Nothing,
      timeoutSeconds = Prelude.Nothing,
      unhealthyThreshold = Prelude.Nothing
    }

-- | The number of consecutive health checks successes required before moving
-- the container to the @Healthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_healthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_healthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {healthyThreshold} -> healthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {healthyThreshold = a} :: ContainerServiceHealthCheckConfig)

-- | The approximate interval, in seconds, between health checks of an
-- individual container. You can specify between 5 and 300 seconds. The
-- default value is @5@.
containerServiceHealthCheckConfig_intervalSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_intervalSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {intervalSeconds} -> intervalSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {intervalSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The path on the container on which to perform the health check. The
-- default value is @\/@.
containerServiceHealthCheckConfig_path :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Text)
containerServiceHealthCheckConfig_path = Lens.lens (\ContainerServiceHealthCheckConfig' {path} -> path) (\s@ContainerServiceHealthCheckConfig' {} a -> s {path = a} :: ContainerServiceHealthCheckConfig)

-- | The HTTP codes to use when checking for a successful response from a
-- container. You can specify values between @200@ and @499@. You can
-- specify multiple values (for example, @200,202@) or a range of values
-- (for example, @200-299@).
containerServiceHealthCheckConfig_successCodes :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Text)
containerServiceHealthCheckConfig_successCodes = Lens.lens (\ContainerServiceHealthCheckConfig' {successCodes} -> successCodes) (\s@ContainerServiceHealthCheckConfig' {} a -> s {successCodes = a} :: ContainerServiceHealthCheckConfig)

-- | The amount of time, in seconds, during which no response means a failed
-- health check. You can specify between 2 and 60 seconds. The default
-- value is @2@.
containerServiceHealthCheckConfig_timeoutSeconds :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_timeoutSeconds = Lens.lens (\ContainerServiceHealthCheckConfig' {timeoutSeconds} -> timeoutSeconds) (\s@ContainerServiceHealthCheckConfig' {} a -> s {timeoutSeconds = a} :: ContainerServiceHealthCheckConfig)

-- | The number of consecutive health check failures required before moving
-- the container to the @Unhealthy@ state. The default value is @2@.
containerServiceHealthCheckConfig_unhealthyThreshold :: Lens.Lens' ContainerServiceHealthCheckConfig (Prelude.Maybe Prelude.Int)
containerServiceHealthCheckConfig_unhealthyThreshold = Lens.lens (\ContainerServiceHealthCheckConfig' {unhealthyThreshold} -> unhealthyThreshold) (\s@ContainerServiceHealthCheckConfig' {} a -> s {unhealthyThreshold = a} :: ContainerServiceHealthCheckConfig)

instance
  Data.FromJSON
    ContainerServiceHealthCheckConfig
  where
  parseJSON =
    Data.withObject
      "ContainerServiceHealthCheckConfig"
      ( \x ->
          ContainerServiceHealthCheckConfig'
            Prelude.<$> (x Data..:? "healthyThreshold")
            Prelude.<*> (x Data..:? "intervalSeconds")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "successCodes")
            Prelude.<*> (x Data..:? "timeoutSeconds")
            Prelude.<*> (x Data..:? "unhealthyThreshold")
      )

instance
  Prelude.Hashable
    ContainerServiceHealthCheckConfig
  where
  hashWithSalt
    _salt
    ContainerServiceHealthCheckConfig' {..} =
      _salt
        `Prelude.hashWithSalt` healthyThreshold
        `Prelude.hashWithSalt` intervalSeconds
        `Prelude.hashWithSalt` path
        `Prelude.hashWithSalt` successCodes
        `Prelude.hashWithSalt` timeoutSeconds
        `Prelude.hashWithSalt` unhealthyThreshold

instance
  Prelude.NFData
    ContainerServiceHealthCheckConfig
  where
  rnf ContainerServiceHealthCheckConfig' {..} =
    Prelude.rnf healthyThreshold
      `Prelude.seq` Prelude.rnf intervalSeconds
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf successCodes
      `Prelude.seq` Prelude.rnf timeoutSeconds
      `Prelude.seq` Prelude.rnf unhealthyThreshold

instance
  Data.ToJSON
    ContainerServiceHealthCheckConfig
  where
  toJSON ContainerServiceHealthCheckConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("healthyThreshold" Data..=)
              Prelude.<$> healthyThreshold,
            ("intervalSeconds" Data..=)
              Prelude.<$> intervalSeconds,
            ("path" Data..=) Prelude.<$> path,
            ("successCodes" Data..=) Prelude.<$> successCodes,
            ("timeoutSeconds" Data..=)
              Prelude.<$> timeoutSeconds,
            ("unhealthyThreshold" Data..=)
              Prelude.<$> unhealthyThreshold
          ]
      )

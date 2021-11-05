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
-- Module      : Amazonka.AppRunner.Types.HealthCheckConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.HealthCheckConfiguration where

import Amazonka.AppRunner.Types.HealthCheckProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the settings for the health check that App Runner performs to
-- monitor the health of a service.
--
-- /See:/ 'newHealthCheckConfiguration' smart constructor.
data HealthCheckConfiguration = HealthCheckConfiguration'
  { -- | The number of consecutive checks that must succeed before App Runner
    -- decides that the service is healthy.
    --
    -- Default: @1@
    healthyThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The URL that health check requests are sent to.
    --
    -- @Path@ is only applicable when you set @Protocol@ to @HTTP@.
    --
    -- Default: @\"\/\"@
    path :: Prelude.Maybe Prelude.Text,
    -- | The IP protocol that App Runner uses to perform health checks for your
    -- service.
    --
    -- If you set @Protocol@ to @HTTP@, App Runner sends health check requests
    -- to the HTTP path specified by @Path@.
    --
    -- Default: @TCP@
    protocol :: Prelude.Maybe HealthCheckProtocol,
    -- | The time interval, in seconds, between health checks.
    --
    -- Default: @5@
    interval :: Prelude.Maybe Prelude.Natural,
    -- | The time, in seconds, to wait for a health check response before
    -- deciding it failed.
    --
    -- Default: @2@
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of consecutive checks that must fail before App Runner
    -- decides that the service is unhealthy.
    --
    -- Default: @5@
    unhealthyThreshold :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthyThreshold', 'healthCheckConfiguration_healthyThreshold' - The number of consecutive checks that must succeed before App Runner
-- decides that the service is healthy.
--
-- Default: @1@
--
-- 'path', 'healthCheckConfiguration_path' - The URL that health check requests are sent to.
--
-- @Path@ is only applicable when you set @Protocol@ to @HTTP@.
--
-- Default: @\"\/\"@
--
-- 'protocol', 'healthCheckConfiguration_protocol' - The IP protocol that App Runner uses to perform health checks for your
-- service.
--
-- If you set @Protocol@ to @HTTP@, App Runner sends health check requests
-- to the HTTP path specified by @Path@.
--
-- Default: @TCP@
--
-- 'interval', 'healthCheckConfiguration_interval' - The time interval, in seconds, between health checks.
--
-- Default: @5@
--
-- 'timeout', 'healthCheckConfiguration_timeout' - The time, in seconds, to wait for a health check response before
-- deciding it failed.
--
-- Default: @2@
--
-- 'unhealthyThreshold', 'healthCheckConfiguration_unhealthyThreshold' - The number of consecutive checks that must fail before App Runner
-- decides that the service is unhealthy.
--
-- Default: @5@
newHealthCheckConfiguration ::
  HealthCheckConfiguration
newHealthCheckConfiguration =
  HealthCheckConfiguration'
    { healthyThreshold =
        Prelude.Nothing,
      path = Prelude.Nothing,
      protocol = Prelude.Nothing,
      interval = Prelude.Nothing,
      timeout = Prelude.Nothing,
      unhealthyThreshold = Prelude.Nothing
    }

-- | The number of consecutive checks that must succeed before App Runner
-- decides that the service is healthy.
--
-- Default: @1@
healthCheckConfiguration_healthyThreshold :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe Prelude.Natural)
healthCheckConfiguration_healthyThreshold = Lens.lens (\HealthCheckConfiguration' {healthyThreshold} -> healthyThreshold) (\s@HealthCheckConfiguration' {} a -> s {healthyThreshold = a} :: HealthCheckConfiguration)

-- | The URL that health check requests are sent to.
--
-- @Path@ is only applicable when you set @Protocol@ to @HTTP@.
--
-- Default: @\"\/\"@
healthCheckConfiguration_path :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe Prelude.Text)
healthCheckConfiguration_path = Lens.lens (\HealthCheckConfiguration' {path} -> path) (\s@HealthCheckConfiguration' {} a -> s {path = a} :: HealthCheckConfiguration)

-- | The IP protocol that App Runner uses to perform health checks for your
-- service.
--
-- If you set @Protocol@ to @HTTP@, App Runner sends health check requests
-- to the HTTP path specified by @Path@.
--
-- Default: @TCP@
healthCheckConfiguration_protocol :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe HealthCheckProtocol)
healthCheckConfiguration_protocol = Lens.lens (\HealthCheckConfiguration' {protocol} -> protocol) (\s@HealthCheckConfiguration' {} a -> s {protocol = a} :: HealthCheckConfiguration)

-- | The time interval, in seconds, between health checks.
--
-- Default: @5@
healthCheckConfiguration_interval :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe Prelude.Natural)
healthCheckConfiguration_interval = Lens.lens (\HealthCheckConfiguration' {interval} -> interval) (\s@HealthCheckConfiguration' {} a -> s {interval = a} :: HealthCheckConfiguration)

-- | The time, in seconds, to wait for a health check response before
-- deciding it failed.
--
-- Default: @2@
healthCheckConfiguration_timeout :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe Prelude.Natural)
healthCheckConfiguration_timeout = Lens.lens (\HealthCheckConfiguration' {timeout} -> timeout) (\s@HealthCheckConfiguration' {} a -> s {timeout = a} :: HealthCheckConfiguration)

-- | The number of consecutive checks that must fail before App Runner
-- decides that the service is unhealthy.
--
-- Default: @5@
healthCheckConfiguration_unhealthyThreshold :: Lens.Lens' HealthCheckConfiguration (Prelude.Maybe Prelude.Natural)
healthCheckConfiguration_unhealthyThreshold = Lens.lens (\HealthCheckConfiguration' {unhealthyThreshold} -> unhealthyThreshold) (\s@HealthCheckConfiguration' {} a -> s {unhealthyThreshold = a} :: HealthCheckConfiguration)

instance Core.FromJSON HealthCheckConfiguration where
  parseJSON =
    Core.withObject
      "HealthCheckConfiguration"
      ( \x ->
          HealthCheckConfiguration'
            Prelude.<$> (x Core..:? "HealthyThreshold")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "Timeout")
            Prelude.<*> (x Core..:? "UnhealthyThreshold")
      )

instance Prelude.Hashable HealthCheckConfiguration

instance Prelude.NFData HealthCheckConfiguration

instance Core.ToJSON HealthCheckConfiguration where
  toJSON HealthCheckConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HealthyThreshold" Core..=)
              Prelude.<$> healthyThreshold,
            ("Path" Core..=) Prelude.<$> path,
            ("Protocol" Core..=) Prelude.<$> protocol,
            ("Interval" Core..=) Prelude.<$> interval,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("UnhealthyThreshold" Core..=)
              Prelude.<$> unhealthyThreshold
          ]
      )

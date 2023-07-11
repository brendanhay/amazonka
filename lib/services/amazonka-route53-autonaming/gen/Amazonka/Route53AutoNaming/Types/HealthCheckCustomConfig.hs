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
-- Module      : Amazonka.Route53AutoNaming.Types.HealthCheckCustomConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.HealthCheckCustomConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about an optional custom health
-- check. A custom health check, which requires that you use a third-party
-- health checker to evaluate the health of your resources, is useful in
-- the following circumstances:
--
-- -   You can\'t use a health check that\'s defined by @HealthCheckConfig@
--     because the resource isn\'t available over the internet. For
--     example, you can use a custom health check when the instance is in
--     an Amazon VPC. (To check the health of resources in a VPC, the
--     health checker must also be in the VPC.)
--
-- -   You want to use a third-party health checker regardless of where
--     your resources are located.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- To change the status of a custom health check, submit an
-- @UpdateInstanceCustomHealthStatus@ request. Cloud Map doesn\'t monitor
-- the status of the resource, it just keeps a record of the status
-- specified in the most recent @UpdateInstanceCustomHealthStatus@ request.
--
-- Here\'s how custom health checks work:
--
-- 1.  You create a service.
--
-- 2.  You register an instance.
--
-- 3.  You configure a third-party health checker to monitor the resource
--     that\'s associated with the new instance.
--
--     Cloud Map doesn\'t check the health of the resource directly.
--
-- 4.  The third-party health-checker determines that the resource is
--     unhealthy and notifies your application.
--
-- 5.  Your application submits an @UpdateInstanceCustomHealthStatus@
--     request.
--
-- 6.  Cloud Map waits for 30 seconds.
--
-- 7.  If another @UpdateInstanceCustomHealthStatus@ request doesn\'t
--     arrive during that time to change the status back to healthy, Cloud
--     Map stops routing traffic to the resource.
--
-- /See:/ 'newHealthCheckCustomConfig' smart constructor.
data HealthCheckCustomConfig = HealthCheckCustomConfig'
  { -- | This parameter is no longer supported and is always set to 1. Cloud Map
    -- waits for approximately 30 seconds after receiving an
    -- @UpdateInstanceCustomHealthStatus@ request before changing the status of
    -- the service instance.
    --
    -- The number of 30-second intervals that you want Cloud Map to wait after
    -- receiving an @UpdateInstanceCustomHealthStatus@ request before it
    -- changes the health status of a service instance.
    --
    -- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
    -- request with the same value before 30 seconds has passed doesn\'t
    -- accelerate the change. Cloud Map still waits @30@ seconds after the
    -- first request to make the change.
    failureThreshold :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckCustomConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureThreshold', 'healthCheckCustomConfig_failureThreshold' - This parameter is no longer supported and is always set to 1. Cloud Map
-- waits for approximately 30 seconds after receiving an
-- @UpdateInstanceCustomHealthStatus@ request before changing the status of
-- the service instance.
--
-- The number of 30-second intervals that you want Cloud Map to wait after
-- receiving an @UpdateInstanceCustomHealthStatus@ request before it
-- changes the health status of a service instance.
--
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
-- request with the same value before 30 seconds has passed doesn\'t
-- accelerate the change. Cloud Map still waits @30@ seconds after the
-- first request to make the change.
newHealthCheckCustomConfig ::
  HealthCheckCustomConfig
newHealthCheckCustomConfig =
  HealthCheckCustomConfig'
    { failureThreshold =
        Prelude.Nothing
    }

-- | This parameter is no longer supported and is always set to 1. Cloud Map
-- waits for approximately 30 seconds after receiving an
-- @UpdateInstanceCustomHealthStatus@ request before changing the status of
-- the service instance.
--
-- The number of 30-second intervals that you want Cloud Map to wait after
-- receiving an @UpdateInstanceCustomHealthStatus@ request before it
-- changes the health status of a service instance.
--
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
-- request with the same value before 30 seconds has passed doesn\'t
-- accelerate the change. Cloud Map still waits @30@ seconds after the
-- first request to make the change.
healthCheckCustomConfig_failureThreshold :: Lens.Lens' HealthCheckCustomConfig (Prelude.Maybe Prelude.Natural)
healthCheckCustomConfig_failureThreshold = Lens.lens (\HealthCheckCustomConfig' {failureThreshold} -> failureThreshold) (\s@HealthCheckCustomConfig' {} a -> s {failureThreshold = a} :: HealthCheckCustomConfig)

instance Data.FromJSON HealthCheckCustomConfig where
  parseJSON =
    Data.withObject
      "HealthCheckCustomConfig"
      ( \x ->
          HealthCheckCustomConfig'
            Prelude.<$> (x Data..:? "FailureThreshold")
      )

instance Prelude.Hashable HealthCheckCustomConfig where
  hashWithSalt _salt HealthCheckCustomConfig' {..} =
    _salt `Prelude.hashWithSalt` failureThreshold

instance Prelude.NFData HealthCheckCustomConfig where
  rnf HealthCheckCustomConfig' {..} =
    Prelude.rnf failureThreshold

instance Data.ToJSON HealthCheckCustomConfig where
  toJSON HealthCheckCustomConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FailureThreshold" Data..=)
              Prelude.<$> failureThreshold
          ]
      )

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
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains information about an optional custom health
-- check. A custom health check, which requires that you use a third-party
-- health checker to evaluate the health of your resources, is useful in
-- the following circumstances:
--
-- -   You can\'t use a health check that is defined by @HealthCheckConfig@
--     because the resource isn\'t available over the internet. For
--     example, you can use a custom health check when the instance is in
--     an Amazon VPC. (To check the health of resources in a VPC, the
--     health checker must also be in the VPC.)
--
-- -   You want to use a third-party health checker regardless of where
--     your resources are.
--
-- If you specify a health check configuration, you can specify either
-- @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
--
-- To change the status of a custom health check, submit an
-- @UpdateInstanceCustomHealthStatus@ request. AWS Cloud Map doesn\'t
-- monitor the status of the resource, it just keeps a record of the status
-- specified in the most recent @UpdateInstanceCustomHealthStatus@ request.
--
-- Here\'s how custom health checks work:
--
-- 1.  You create a service and specify a value for @FailureThreshold@.
--
--     The failure threshold indicates the number of 30-second intervals
--     you want AWS Cloud Map to wait between the time that your
--     application sends an
--     <https://docs.aws.amazon.com/cloud-map/latest/api/API_UpdateInstanceCustomHealthStatus.html UpdateInstanceCustomHealthStatus>
--     request and the time that AWS Cloud Map stops routing internet
--     traffic to the corresponding resource.
--
-- 2.  You register an instance.
--
-- 3.  You configure a third-party health checker to monitor the resource
--     that is associated with the new instance.
--
--     AWS Cloud Map doesn\'t check the health of the resource directly.
--
-- 4.  The third-party health-checker determines that the resource is
--     unhealthy and notifies your application.
--
-- 5.  Your application submits an @UpdateInstanceCustomHealthStatus@
--     request.
--
-- 6.  AWS Cloud Map waits for (@FailureThreshold@ x 30) seconds.
--
-- 7.  If another @UpdateInstanceCustomHealthStatus@ request doesn\'t
--     arrive during that time to change the status back to healthy, AWS
--     Cloud Map stops routing traffic to the resource.
--
-- /See:/ 'newHealthCheckCustomConfig' smart constructor.
data HealthCheckCustomConfig = HealthCheckCustomConfig'
  { -- | This parameter has been deprecated and is always set to 1. AWS Cloud Map
    -- waits for approximately 30 seconds after receiving an
    -- @UpdateInstanceCustomHealthStatus@ request before changing the status of
    -- the service instance.
    --
    -- The number of 30-second intervals that you want AWS Cloud Map to wait
    -- after receiving an @UpdateInstanceCustomHealthStatus@ request before it
    -- changes the health status of a service instance.
    --
    -- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
    -- request with the same value before 30 seconds has passed doesn\'t
    -- accelerate the change. AWS Cloud Map still waits @30@ seconds after the
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
-- 'failureThreshold', 'healthCheckCustomConfig_failureThreshold' - This parameter has been deprecated and is always set to 1. AWS Cloud Map
-- waits for approximately 30 seconds after receiving an
-- @UpdateInstanceCustomHealthStatus@ request before changing the status of
-- the service instance.
--
-- The number of 30-second intervals that you want AWS Cloud Map to wait
-- after receiving an @UpdateInstanceCustomHealthStatus@ request before it
-- changes the health status of a service instance.
--
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
-- request with the same value before 30 seconds has passed doesn\'t
-- accelerate the change. AWS Cloud Map still waits @30@ seconds after the
-- first request to make the change.
newHealthCheckCustomConfig ::
  HealthCheckCustomConfig
newHealthCheckCustomConfig =
  HealthCheckCustomConfig'
    { failureThreshold =
        Prelude.Nothing
    }

-- | This parameter has been deprecated and is always set to 1. AWS Cloud Map
-- waits for approximately 30 seconds after receiving an
-- @UpdateInstanceCustomHealthStatus@ request before changing the status of
-- the service instance.
--
-- The number of 30-second intervals that you want AWS Cloud Map to wait
-- after receiving an @UpdateInstanceCustomHealthStatus@ request before it
-- changes the health status of a service instance.
--
-- Sending a second or subsequent @UpdateInstanceCustomHealthStatus@
-- request with the same value before 30 seconds has passed doesn\'t
-- accelerate the change. AWS Cloud Map still waits @30@ seconds after the
-- first request to make the change.
healthCheckCustomConfig_failureThreshold :: Lens.Lens' HealthCheckCustomConfig (Prelude.Maybe Prelude.Natural)
healthCheckCustomConfig_failureThreshold = Lens.lens (\HealthCheckCustomConfig' {failureThreshold} -> failureThreshold) (\s@HealthCheckCustomConfig' {} a -> s {failureThreshold = a} :: HealthCheckCustomConfig)

instance Core.FromJSON HealthCheckCustomConfig where
  parseJSON =
    Core.withObject
      "HealthCheckCustomConfig"
      ( \x ->
          HealthCheckCustomConfig'
            Prelude.<$> (x Core..:? "FailureThreshold")
      )

instance Prelude.Hashable HealthCheckCustomConfig

instance Prelude.NFData HealthCheckCustomConfig

instance Core.ToJSON HealthCheckCustomConfig where
  toJSON HealthCheckCustomConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FailureThreshold" Core..=)
              Prelude.<$> failureThreshold
          ]
      )

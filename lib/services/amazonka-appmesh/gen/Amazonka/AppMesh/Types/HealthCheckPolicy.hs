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
-- Module      : Amazonka.AppMesh.Types.HealthCheckPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HealthCheckPolicy where

import Amazonka.AppMesh.Types.PortProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the health check policy for a virtual node\'s
-- listener.
--
-- /See:/ 'newHealthCheckPolicy' smart constructor.
data HealthCheckPolicy = HealthCheckPolicy'
  { -- | The destination port for the health check request. This port must match
    -- the port defined in the PortMapping for the listener.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The destination path for the health check request. This value is only
    -- used if the specified protocol is HTTP or HTTP\/2. For any other
    -- protocol, this value is ignored.
    path :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive successful health checks that must occur
    -- before declaring listener healthy.
    healthyThreshold :: Prelude.Natural,
    -- | The time period in milliseconds between each health check execution.
    intervalMillis :: Prelude.Natural,
    -- | The protocol for the health check request. If you specify @grpc@, then
    -- your service must conform to the
    -- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
    protocol :: PortProtocol,
    -- | The amount of time to wait when receiving a response from the health
    -- check, in milliseconds.
    timeoutMillis :: Prelude.Natural,
    -- | The number of consecutive failed health checks that must occur before
    -- declaring a virtual node unhealthy.
    unhealthyThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HealthCheckPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'healthCheckPolicy_port' - The destination port for the health check request. This port must match
-- the port defined in the PortMapping for the listener.
--
-- 'path', 'healthCheckPolicy_path' - The destination path for the health check request. This value is only
-- used if the specified protocol is HTTP or HTTP\/2. For any other
-- protocol, this value is ignored.
--
-- 'healthyThreshold', 'healthCheckPolicy_healthyThreshold' - The number of consecutive successful health checks that must occur
-- before declaring listener healthy.
--
-- 'intervalMillis', 'healthCheckPolicy_intervalMillis' - The time period in milliseconds between each health check execution.
--
-- 'protocol', 'healthCheckPolicy_protocol' - The protocol for the health check request. If you specify @grpc@, then
-- your service must conform to the
-- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
--
-- 'timeoutMillis', 'healthCheckPolicy_timeoutMillis' - The amount of time to wait when receiving a response from the health
-- check, in milliseconds.
--
-- 'unhealthyThreshold', 'healthCheckPolicy_unhealthyThreshold' - The number of consecutive failed health checks that must occur before
-- declaring a virtual node unhealthy.
newHealthCheckPolicy ::
  -- | 'healthyThreshold'
  Prelude.Natural ->
  -- | 'intervalMillis'
  Prelude.Natural ->
  -- | 'protocol'
  PortProtocol ->
  -- | 'timeoutMillis'
  Prelude.Natural ->
  -- | 'unhealthyThreshold'
  Prelude.Natural ->
  HealthCheckPolicy
newHealthCheckPolicy
  pHealthyThreshold_
  pIntervalMillis_
  pProtocol_
  pTimeoutMillis_
  pUnhealthyThreshold_ =
    HealthCheckPolicy'
      { port = Prelude.Nothing,
        path = Prelude.Nothing,
        healthyThreshold = pHealthyThreshold_,
        intervalMillis = pIntervalMillis_,
        protocol = pProtocol_,
        timeoutMillis = pTimeoutMillis_,
        unhealthyThreshold = pUnhealthyThreshold_
      }

-- | The destination port for the health check request. This port must match
-- the port defined in the PortMapping for the listener.
healthCheckPolicy_port :: Lens.Lens' HealthCheckPolicy (Prelude.Maybe Prelude.Natural)
healthCheckPolicy_port = Lens.lens (\HealthCheckPolicy' {port} -> port) (\s@HealthCheckPolicy' {} a -> s {port = a} :: HealthCheckPolicy)

-- | The destination path for the health check request. This value is only
-- used if the specified protocol is HTTP or HTTP\/2. For any other
-- protocol, this value is ignored.
healthCheckPolicy_path :: Lens.Lens' HealthCheckPolicy (Prelude.Maybe Prelude.Text)
healthCheckPolicy_path = Lens.lens (\HealthCheckPolicy' {path} -> path) (\s@HealthCheckPolicy' {} a -> s {path = a} :: HealthCheckPolicy)

-- | The number of consecutive successful health checks that must occur
-- before declaring listener healthy.
healthCheckPolicy_healthyThreshold :: Lens.Lens' HealthCheckPolicy Prelude.Natural
healthCheckPolicy_healthyThreshold = Lens.lens (\HealthCheckPolicy' {healthyThreshold} -> healthyThreshold) (\s@HealthCheckPolicy' {} a -> s {healthyThreshold = a} :: HealthCheckPolicy)

-- | The time period in milliseconds between each health check execution.
healthCheckPolicy_intervalMillis :: Lens.Lens' HealthCheckPolicy Prelude.Natural
healthCheckPolicy_intervalMillis = Lens.lens (\HealthCheckPolicy' {intervalMillis} -> intervalMillis) (\s@HealthCheckPolicy' {} a -> s {intervalMillis = a} :: HealthCheckPolicy)

-- | The protocol for the health check request. If you specify @grpc@, then
-- your service must conform to the
-- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
healthCheckPolicy_protocol :: Lens.Lens' HealthCheckPolicy PortProtocol
healthCheckPolicy_protocol = Lens.lens (\HealthCheckPolicy' {protocol} -> protocol) (\s@HealthCheckPolicy' {} a -> s {protocol = a} :: HealthCheckPolicy)

-- | The amount of time to wait when receiving a response from the health
-- check, in milliseconds.
healthCheckPolicy_timeoutMillis :: Lens.Lens' HealthCheckPolicy Prelude.Natural
healthCheckPolicy_timeoutMillis = Lens.lens (\HealthCheckPolicy' {timeoutMillis} -> timeoutMillis) (\s@HealthCheckPolicy' {} a -> s {timeoutMillis = a} :: HealthCheckPolicy)

-- | The number of consecutive failed health checks that must occur before
-- declaring a virtual node unhealthy.
healthCheckPolicy_unhealthyThreshold :: Lens.Lens' HealthCheckPolicy Prelude.Natural
healthCheckPolicy_unhealthyThreshold = Lens.lens (\HealthCheckPolicy' {unhealthyThreshold} -> unhealthyThreshold) (\s@HealthCheckPolicy' {} a -> s {unhealthyThreshold = a} :: HealthCheckPolicy)

instance Core.FromJSON HealthCheckPolicy where
  parseJSON =
    Core.withObject
      "HealthCheckPolicy"
      ( \x ->
          HealthCheckPolicy'
            Prelude.<$> (x Core..:? "port")
            Prelude.<*> (x Core..:? "path")
            Prelude.<*> (x Core..: "healthyThreshold")
            Prelude.<*> (x Core..: "intervalMillis")
            Prelude.<*> (x Core..: "protocol")
            Prelude.<*> (x Core..: "timeoutMillis")
            Prelude.<*> (x Core..: "unhealthyThreshold")
      )

instance Prelude.Hashable HealthCheckPolicy where
  hashWithSalt _salt HealthCheckPolicy' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` healthyThreshold
      `Prelude.hashWithSalt` intervalMillis
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` timeoutMillis
      `Prelude.hashWithSalt` unhealthyThreshold

instance Prelude.NFData HealthCheckPolicy where
  rnf HealthCheckPolicy' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf healthyThreshold
      `Prelude.seq` Prelude.rnf intervalMillis
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf timeoutMillis
      `Prelude.seq` Prelude.rnf unhealthyThreshold

instance Core.ToJSON HealthCheckPolicy where
  toJSON HealthCheckPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("port" Core..=) Prelude.<$> port,
            ("path" Core..=) Prelude.<$> path,
            Prelude.Just
              ("healthyThreshold" Core..= healthyThreshold),
            Prelude.Just
              ("intervalMillis" Core..= intervalMillis),
            Prelude.Just ("protocol" Core..= protocol),
            Prelude.Just ("timeoutMillis" Core..= timeoutMillis),
            Prelude.Just
              ("unhealthyThreshold" Core..= unhealthyThreshold)
          ]
      )

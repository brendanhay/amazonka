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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayHealthCheckPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayHealthCheckPolicy where

import Amazonka.AppMesh.Types.VirtualGatewayPortProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the health check policy for a virtual
-- gateway\'s listener.
--
-- /See:/ 'newVirtualGatewayHealthCheckPolicy' smart constructor.
data VirtualGatewayHealthCheckPolicy = VirtualGatewayHealthCheckPolicy'
  { -- | The destination port for the health check request. This port must match
    -- the port defined in the PortMapping for the listener.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The destination path for the health check request. This value is only
    -- used if the specified protocol is HTTP or HTTP\/2. For any other
    -- protocol, this value is ignored.
    path :: Prelude.Maybe Prelude.Text,
    -- | The number of consecutive successful health checks that must occur
    -- before declaring the listener healthy.
    healthyThreshold :: Prelude.Natural,
    -- | The time period in milliseconds between each health check execution.
    intervalMillis :: Prelude.Natural,
    -- | The protocol for the health check request. If you specify @grpc@, then
    -- your service must conform to the
    -- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
    protocol :: VirtualGatewayPortProtocol,
    -- | The amount of time to wait when receiving a response from the health
    -- check, in milliseconds.
    timeoutMillis :: Prelude.Natural,
    -- | The number of consecutive failed health checks that must occur before
    -- declaring a virtual gateway unhealthy.
    unhealthyThreshold :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayHealthCheckPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'virtualGatewayHealthCheckPolicy_port' - The destination port for the health check request. This port must match
-- the port defined in the PortMapping for the listener.
--
-- 'path', 'virtualGatewayHealthCheckPolicy_path' - The destination path for the health check request. This value is only
-- used if the specified protocol is HTTP or HTTP\/2. For any other
-- protocol, this value is ignored.
--
-- 'healthyThreshold', 'virtualGatewayHealthCheckPolicy_healthyThreshold' - The number of consecutive successful health checks that must occur
-- before declaring the listener healthy.
--
-- 'intervalMillis', 'virtualGatewayHealthCheckPolicy_intervalMillis' - The time period in milliseconds between each health check execution.
--
-- 'protocol', 'virtualGatewayHealthCheckPolicy_protocol' - The protocol for the health check request. If you specify @grpc@, then
-- your service must conform to the
-- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
--
-- 'timeoutMillis', 'virtualGatewayHealthCheckPolicy_timeoutMillis' - The amount of time to wait when receiving a response from the health
-- check, in milliseconds.
--
-- 'unhealthyThreshold', 'virtualGatewayHealthCheckPolicy_unhealthyThreshold' - The number of consecutive failed health checks that must occur before
-- declaring a virtual gateway unhealthy.
newVirtualGatewayHealthCheckPolicy ::
  -- | 'healthyThreshold'
  Prelude.Natural ->
  -- | 'intervalMillis'
  Prelude.Natural ->
  -- | 'protocol'
  VirtualGatewayPortProtocol ->
  -- | 'timeoutMillis'
  Prelude.Natural ->
  -- | 'unhealthyThreshold'
  Prelude.Natural ->
  VirtualGatewayHealthCheckPolicy
newVirtualGatewayHealthCheckPolicy
  pHealthyThreshold_
  pIntervalMillis_
  pProtocol_
  pTimeoutMillis_
  pUnhealthyThreshold_ =
    VirtualGatewayHealthCheckPolicy'
      { port =
          Prelude.Nothing,
        path = Prelude.Nothing,
        healthyThreshold = pHealthyThreshold_,
        intervalMillis = pIntervalMillis_,
        protocol = pProtocol_,
        timeoutMillis = pTimeoutMillis_,
        unhealthyThreshold = pUnhealthyThreshold_
      }

-- | The destination port for the health check request. This port must match
-- the port defined in the PortMapping for the listener.
virtualGatewayHealthCheckPolicy_port :: Lens.Lens' VirtualGatewayHealthCheckPolicy (Prelude.Maybe Prelude.Natural)
virtualGatewayHealthCheckPolicy_port = Lens.lens (\VirtualGatewayHealthCheckPolicy' {port} -> port) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {port = a} :: VirtualGatewayHealthCheckPolicy)

-- | The destination path for the health check request. This value is only
-- used if the specified protocol is HTTP or HTTP\/2. For any other
-- protocol, this value is ignored.
virtualGatewayHealthCheckPolicy_path :: Lens.Lens' VirtualGatewayHealthCheckPolicy (Prelude.Maybe Prelude.Text)
virtualGatewayHealthCheckPolicy_path = Lens.lens (\VirtualGatewayHealthCheckPolicy' {path} -> path) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {path = a} :: VirtualGatewayHealthCheckPolicy)

-- | The number of consecutive successful health checks that must occur
-- before declaring the listener healthy.
virtualGatewayHealthCheckPolicy_healthyThreshold :: Lens.Lens' VirtualGatewayHealthCheckPolicy Prelude.Natural
virtualGatewayHealthCheckPolicy_healthyThreshold = Lens.lens (\VirtualGatewayHealthCheckPolicy' {healthyThreshold} -> healthyThreshold) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {healthyThreshold = a} :: VirtualGatewayHealthCheckPolicy)

-- | The time period in milliseconds between each health check execution.
virtualGatewayHealthCheckPolicy_intervalMillis :: Lens.Lens' VirtualGatewayHealthCheckPolicy Prelude.Natural
virtualGatewayHealthCheckPolicy_intervalMillis = Lens.lens (\VirtualGatewayHealthCheckPolicy' {intervalMillis} -> intervalMillis) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {intervalMillis = a} :: VirtualGatewayHealthCheckPolicy)

-- | The protocol for the health check request. If you specify @grpc@, then
-- your service must conform to the
-- <https://github.com/grpc/grpc/blob/master/doc/health-checking.md GRPC Health Checking Protocol>.
virtualGatewayHealthCheckPolicy_protocol :: Lens.Lens' VirtualGatewayHealthCheckPolicy VirtualGatewayPortProtocol
virtualGatewayHealthCheckPolicy_protocol = Lens.lens (\VirtualGatewayHealthCheckPolicy' {protocol} -> protocol) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {protocol = a} :: VirtualGatewayHealthCheckPolicy)

-- | The amount of time to wait when receiving a response from the health
-- check, in milliseconds.
virtualGatewayHealthCheckPolicy_timeoutMillis :: Lens.Lens' VirtualGatewayHealthCheckPolicy Prelude.Natural
virtualGatewayHealthCheckPolicy_timeoutMillis = Lens.lens (\VirtualGatewayHealthCheckPolicy' {timeoutMillis} -> timeoutMillis) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {timeoutMillis = a} :: VirtualGatewayHealthCheckPolicy)

-- | The number of consecutive failed health checks that must occur before
-- declaring a virtual gateway unhealthy.
virtualGatewayHealthCheckPolicy_unhealthyThreshold :: Lens.Lens' VirtualGatewayHealthCheckPolicy Prelude.Natural
virtualGatewayHealthCheckPolicy_unhealthyThreshold = Lens.lens (\VirtualGatewayHealthCheckPolicy' {unhealthyThreshold} -> unhealthyThreshold) (\s@VirtualGatewayHealthCheckPolicy' {} a -> s {unhealthyThreshold = a} :: VirtualGatewayHealthCheckPolicy)

instance
  Core.FromJSON
    VirtualGatewayHealthCheckPolicy
  where
  parseJSON =
    Core.withObject
      "VirtualGatewayHealthCheckPolicy"
      ( \x ->
          VirtualGatewayHealthCheckPolicy'
            Prelude.<$> (x Core..:? "port")
            Prelude.<*> (x Core..:? "path")
            Prelude.<*> (x Core..: "healthyThreshold")
            Prelude.<*> (x Core..: "intervalMillis")
            Prelude.<*> (x Core..: "protocol")
            Prelude.<*> (x Core..: "timeoutMillis")
            Prelude.<*> (x Core..: "unhealthyThreshold")
      )

instance
  Prelude.Hashable
    VirtualGatewayHealthCheckPolicy
  where
  hashWithSalt
    _salt
    VirtualGatewayHealthCheckPolicy' {..} =
      _salt `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` path
        `Prelude.hashWithSalt` healthyThreshold
        `Prelude.hashWithSalt` intervalMillis
        `Prelude.hashWithSalt` protocol
        `Prelude.hashWithSalt` timeoutMillis
        `Prelude.hashWithSalt` unhealthyThreshold

instance
  Prelude.NFData
    VirtualGatewayHealthCheckPolicy
  where
  rnf VirtualGatewayHealthCheckPolicy' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf healthyThreshold
      `Prelude.seq` Prelude.rnf intervalMillis
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf timeoutMillis
      `Prelude.seq` Prelude.rnf unhealthyThreshold

instance Core.ToJSON VirtualGatewayHealthCheckPolicy where
  toJSON VirtualGatewayHealthCheckPolicy' {..} =
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

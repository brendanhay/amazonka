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
-- Module      : Network.AWS.EC2.Types.VgwTelemetry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VgwTelemetry where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TelemetryStatus
import qualified Network.AWS.Lens as Lens

-- | Describes telemetry for a VPN tunnel.
--
-- /See:/ 'newVgwTelemetry' smart constructor.
data VgwTelemetry = VgwTelemetry'
  { -- | If an error occurs, a description of the error.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the VPN tunnel.
    status :: Core.Maybe TelemetryStatus,
    -- | The number of accepted routes.
    acceptedRouteCount :: Core.Maybe Core.Int,
    -- | The date and time of the last change in status.
    lastStatusChange :: Core.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
    certificateArn :: Core.Maybe Core.Text,
    -- | The Internet-routable IP address of the virtual private gateway\'s
    -- outside interface.
    outsideIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VgwTelemetry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'vgwTelemetry_statusMessage' - If an error occurs, a description of the error.
--
-- 'status', 'vgwTelemetry_status' - The status of the VPN tunnel.
--
-- 'acceptedRouteCount', 'vgwTelemetry_acceptedRouteCount' - The number of accepted routes.
--
-- 'lastStatusChange', 'vgwTelemetry_lastStatusChange' - The date and time of the last change in status.
--
-- 'certificateArn', 'vgwTelemetry_certificateArn' - The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
--
-- 'outsideIpAddress', 'vgwTelemetry_outsideIpAddress' - The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
newVgwTelemetry ::
  VgwTelemetry
newVgwTelemetry =
  VgwTelemetry'
    { statusMessage = Core.Nothing,
      status = Core.Nothing,
      acceptedRouteCount = Core.Nothing,
      lastStatusChange = Core.Nothing,
      certificateArn = Core.Nothing,
      outsideIpAddress = Core.Nothing
    }

-- | If an error occurs, a description of the error.
vgwTelemetry_statusMessage :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vgwTelemetry_statusMessage = Lens.lens (\VgwTelemetry' {statusMessage} -> statusMessage) (\s@VgwTelemetry' {} a -> s {statusMessage = a} :: VgwTelemetry)

-- | The status of the VPN tunnel.
vgwTelemetry_status :: Lens.Lens' VgwTelemetry (Core.Maybe TelemetryStatus)
vgwTelemetry_status = Lens.lens (\VgwTelemetry' {status} -> status) (\s@VgwTelemetry' {} a -> s {status = a} :: VgwTelemetry)

-- | The number of accepted routes.
vgwTelemetry_acceptedRouteCount :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Int)
vgwTelemetry_acceptedRouteCount = Lens.lens (\VgwTelemetry' {acceptedRouteCount} -> acceptedRouteCount) (\s@VgwTelemetry' {} a -> s {acceptedRouteCount = a} :: VgwTelemetry)

-- | The date and time of the last change in status.
vgwTelemetry_lastStatusChange :: Lens.Lens' VgwTelemetry (Core.Maybe Core.UTCTime)
vgwTelemetry_lastStatusChange = Lens.lens (\VgwTelemetry' {lastStatusChange} -> lastStatusChange) (\s@VgwTelemetry' {} a -> s {lastStatusChange = a} :: VgwTelemetry) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
vgwTelemetry_certificateArn :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vgwTelemetry_certificateArn = Lens.lens (\VgwTelemetry' {certificateArn} -> certificateArn) (\s@VgwTelemetry' {} a -> s {certificateArn = a} :: VgwTelemetry)

-- | The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
vgwTelemetry_outsideIpAddress :: Lens.Lens' VgwTelemetry (Core.Maybe Core.Text)
vgwTelemetry_outsideIpAddress = Lens.lens (\VgwTelemetry' {outsideIpAddress} -> outsideIpAddress) (\s@VgwTelemetry' {} a -> s {outsideIpAddress = a} :: VgwTelemetry)

instance Core.FromXML VgwTelemetry where
  parseXML x =
    VgwTelemetry'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "acceptedRouteCount")
      Core.<*> (x Core..@? "lastStatusChange")
      Core.<*> (x Core..@? "certificateArn")
      Core.<*> (x Core..@? "outsideIpAddress")

instance Core.Hashable VgwTelemetry

instance Core.NFData VgwTelemetry

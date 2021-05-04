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
-- Module      : Network.AWS.EC2.Types.VgwTelemetry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VgwTelemetry where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.TelemetryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes telemetry for a VPN tunnel.
--
-- /See:/ 'newVgwTelemetry' smart constructor.
data VgwTelemetry = VgwTelemetry'
  { -- | If an error occurs, a description of the error.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPN tunnel.
    status :: Prelude.Maybe TelemetryStatus,
    -- | The number of accepted routes.
    acceptedRouteCount :: Prelude.Maybe Prelude.Int,
    -- | The date and time of the last change in status.
    lastStatusChange :: Prelude.Maybe Prelude.ISO8601,
    -- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The Internet-routable IP address of the virtual private gateway\'s
    -- outside interface.
    outsideIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { statusMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      acceptedRouteCount = Prelude.Nothing,
      lastStatusChange = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      outsideIpAddress = Prelude.Nothing
    }

-- | If an error occurs, a description of the error.
vgwTelemetry_statusMessage :: Lens.Lens' VgwTelemetry (Prelude.Maybe Prelude.Text)
vgwTelemetry_statusMessage = Lens.lens (\VgwTelemetry' {statusMessage} -> statusMessage) (\s@VgwTelemetry' {} a -> s {statusMessage = a} :: VgwTelemetry)

-- | The status of the VPN tunnel.
vgwTelemetry_status :: Lens.Lens' VgwTelemetry (Prelude.Maybe TelemetryStatus)
vgwTelemetry_status = Lens.lens (\VgwTelemetry' {status} -> status) (\s@VgwTelemetry' {} a -> s {status = a} :: VgwTelemetry)

-- | The number of accepted routes.
vgwTelemetry_acceptedRouteCount :: Lens.Lens' VgwTelemetry (Prelude.Maybe Prelude.Int)
vgwTelemetry_acceptedRouteCount = Lens.lens (\VgwTelemetry' {acceptedRouteCount} -> acceptedRouteCount) (\s@VgwTelemetry' {} a -> s {acceptedRouteCount = a} :: VgwTelemetry)

-- | The date and time of the last change in status.
vgwTelemetry_lastStatusChange :: Lens.Lens' VgwTelemetry (Prelude.Maybe Prelude.UTCTime)
vgwTelemetry_lastStatusChange = Lens.lens (\VgwTelemetry' {lastStatusChange} -> lastStatusChange) (\s@VgwTelemetry' {} a -> s {lastStatusChange = a} :: VgwTelemetry) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the VPN tunnel endpoint certificate.
vgwTelemetry_certificateArn :: Lens.Lens' VgwTelemetry (Prelude.Maybe Prelude.Text)
vgwTelemetry_certificateArn = Lens.lens (\VgwTelemetry' {certificateArn} -> certificateArn) (\s@VgwTelemetry' {} a -> s {certificateArn = a} :: VgwTelemetry)

-- | The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
vgwTelemetry_outsideIpAddress :: Lens.Lens' VgwTelemetry (Prelude.Maybe Prelude.Text)
vgwTelemetry_outsideIpAddress = Lens.lens (\VgwTelemetry' {outsideIpAddress} -> outsideIpAddress) (\s@VgwTelemetry' {} a -> s {outsideIpAddress = a} :: VgwTelemetry)

instance Prelude.FromXML VgwTelemetry where
  parseXML x =
    VgwTelemetry'
      Prelude.<$> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "acceptedRouteCount")
      Prelude.<*> (x Prelude..@? "lastStatusChange")
      Prelude.<*> (x Prelude..@? "certificateArn")
      Prelude.<*> (x Prelude..@? "outsideIpAddress")

instance Prelude.Hashable VgwTelemetry

instance Prelude.NFData VgwTelemetry

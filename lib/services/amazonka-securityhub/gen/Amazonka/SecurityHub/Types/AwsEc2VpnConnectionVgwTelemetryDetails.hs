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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpnConnectionVgwTelemetryDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpnConnectionVgwTelemetryDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the VPN tunnel.
--
-- /See:/ 'newAwsEc2VpnConnectionVgwTelemetryDetails' smart constructor.
data AwsEc2VpnConnectionVgwTelemetryDetails = AwsEc2VpnConnectionVgwTelemetryDetails'
  { -- | The number of accepted routes.
    acceptedRouteCount :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the VPN tunnel endpoint certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the last change in status.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastStatusChange :: Prelude.Maybe Prelude.Text,
    -- | The Internet-routable IP address of the virtual private gateway\'s
    -- outside interface.
    outsideIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPN tunnel. Valid values are @DOWN@ or @UP@.
    status :: Prelude.Maybe Prelude.Text,
    -- | If an error occurs, a description of the error.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpnConnectionVgwTelemetryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptedRouteCount', 'awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount' - The number of accepted routes.
--
-- 'certificateArn', 'awsEc2VpnConnectionVgwTelemetryDetails_certificateArn' - The ARN of the VPN tunnel endpoint certificate.
--
-- 'lastStatusChange', 'awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange' - The date and time of the last change in status.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'outsideIpAddress', 'awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress' - The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
--
-- 'status', 'awsEc2VpnConnectionVgwTelemetryDetails_status' - The status of the VPN tunnel. Valid values are @DOWN@ or @UP@.
--
-- 'statusMessage', 'awsEc2VpnConnectionVgwTelemetryDetails_statusMessage' - If an error occurs, a description of the error.
newAwsEc2VpnConnectionVgwTelemetryDetails ::
  AwsEc2VpnConnectionVgwTelemetryDetails
newAwsEc2VpnConnectionVgwTelemetryDetails =
  AwsEc2VpnConnectionVgwTelemetryDetails'
    { acceptedRouteCount =
        Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      lastStatusChange = Prelude.Nothing,
      outsideIpAddress = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The number of accepted routes.
awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Int)
awsEc2VpnConnectionVgwTelemetryDetails_acceptedRouteCount = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {acceptedRouteCount} -> acceptedRouteCount) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {acceptedRouteCount = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

-- | The ARN of the VPN tunnel endpoint certificate.
awsEc2VpnConnectionVgwTelemetryDetails_certificateArn :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionVgwTelemetryDetails_certificateArn = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {certificateArn} -> certificateArn) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {certificateArn = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

-- | The date and time of the last change in status.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionVgwTelemetryDetails_lastStatusChange = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {lastStatusChange} -> lastStatusChange) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {lastStatusChange = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

-- | The Internet-routable IP address of the virtual private gateway\'s
-- outside interface.
awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionVgwTelemetryDetails_outsideIpAddress = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {outsideIpAddress} -> outsideIpAddress) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {outsideIpAddress = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

-- | The status of the VPN tunnel. Valid values are @DOWN@ or @UP@.
awsEc2VpnConnectionVgwTelemetryDetails_status :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionVgwTelemetryDetails_status = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {status} -> status) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {status = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

-- | If an error occurs, a description of the error.
awsEc2VpnConnectionVgwTelemetryDetails_statusMessage :: Lens.Lens' AwsEc2VpnConnectionVgwTelemetryDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionVgwTelemetryDetails_statusMessage = Lens.lens (\AwsEc2VpnConnectionVgwTelemetryDetails' {statusMessage} -> statusMessage) (\s@AwsEc2VpnConnectionVgwTelemetryDetails' {} a -> s {statusMessage = a} :: AwsEc2VpnConnectionVgwTelemetryDetails)

instance
  Data.FromJSON
    AwsEc2VpnConnectionVgwTelemetryDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpnConnectionVgwTelemetryDetails"
      ( \x ->
          AwsEc2VpnConnectionVgwTelemetryDetails'
            Prelude.<$> (x Data..:? "AcceptedRouteCount")
            Prelude.<*> (x Data..:? "CertificateArn")
            Prelude.<*> (x Data..:? "LastStatusChange")
            Prelude.<*> (x Data..:? "OutsideIpAddress")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance
  Prelude.Hashable
    AwsEc2VpnConnectionVgwTelemetryDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpnConnectionVgwTelemetryDetails' {..} =
      _salt
        `Prelude.hashWithSalt` acceptedRouteCount
        `Prelude.hashWithSalt` certificateArn
        `Prelude.hashWithSalt` lastStatusChange
        `Prelude.hashWithSalt` outsideIpAddress
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusMessage

instance
  Prelude.NFData
    AwsEc2VpnConnectionVgwTelemetryDetails
  where
  rnf AwsEc2VpnConnectionVgwTelemetryDetails' {..} =
    Prelude.rnf acceptedRouteCount
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf lastStatusChange
      `Prelude.seq` Prelude.rnf outsideIpAddress
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage

instance
  Data.ToJSON
    AwsEc2VpnConnectionVgwTelemetryDetails
  where
  toJSON AwsEc2VpnConnectionVgwTelemetryDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptedRouteCount" Data..=)
              Prelude.<$> acceptedRouteCount,
            ("CertificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("LastStatusChange" Data..=)
              Prelude.<$> lastStatusChange,
            ("OutsideIpAddress" Data..=)
              Prelude.<$> outsideIpAddress,
            ("Status" Data..=) Prelude.<$> status,
            ("StatusMessage" Data..=) Prelude.<$> statusMessage
          ]
      )

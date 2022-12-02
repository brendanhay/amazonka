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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpnConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionRoutesDetails
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionVgwTelemetryDetails

-- | Details about an Amazon EC2 VPN connection.
--
-- /See:/ 'newAwsEc2VpnConnectionDetails' smart constructor.
data AwsEc2VpnConnectionDetails = AwsEc2VpnConnectionDetails'
  { -- | The type of VPN connection.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the transit gateway that is associated with the VPN
    -- connection.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The configuration information for the VPN connection\'s customer
    -- gateway, in the native XML format.
    customerGatewayConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPN connection.
    vpnConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the VPN connection. Valid values are as follows:
    --
    -- -   @available@
    --
    -- -   @deleted@
    --
    -- -   @deleting@
    --
    -- -   @pending@
    state :: Prelude.Maybe Prelude.Text,
    -- | The VPN connection options.
    options :: Prelude.Maybe AwsEc2VpnConnectionOptionsDetails,
    -- | The identifier of the customer gateway that is at your end of the VPN
    -- connection.
    customerGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the virtual private gateway that is at the Amazon Web
    -- Services side of the VPN connection.
    vpnGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The category of the VPN connection. @VPN@ indicates an Amazon Web
    -- Services VPN connection. @VPN-Classic@ indicates an Amazon Web Services
    -- Classic VPN connection.
    category :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPN tunnel.
    vgwTelemetry :: Prelude.Maybe [AwsEc2VpnConnectionVgwTelemetryDetails],
    -- | The static routes that are associated with the VPN connection.
    routes :: Prelude.Maybe [AwsEc2VpnConnectionRoutesDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpnConnectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsEc2VpnConnectionDetails_type' - The type of VPN connection.
--
-- 'transitGatewayId', 'awsEc2VpnConnectionDetails_transitGatewayId' - The identifier of the transit gateway that is associated with the VPN
-- connection.
--
-- 'customerGatewayConfiguration', 'awsEc2VpnConnectionDetails_customerGatewayConfiguration' - The configuration information for the VPN connection\'s customer
-- gateway, in the native XML format.
--
-- 'vpnConnectionId', 'awsEc2VpnConnectionDetails_vpnConnectionId' - The identifier of the VPN connection.
--
-- 'state', 'awsEc2VpnConnectionDetails_state' - The current state of the VPN connection. Valid values are as follows:
--
-- -   @available@
--
-- -   @deleted@
--
-- -   @deleting@
--
-- -   @pending@
--
-- 'options', 'awsEc2VpnConnectionDetails_options' - The VPN connection options.
--
-- 'customerGatewayId', 'awsEc2VpnConnectionDetails_customerGatewayId' - The identifier of the customer gateway that is at your end of the VPN
-- connection.
--
-- 'vpnGatewayId', 'awsEc2VpnConnectionDetails_vpnGatewayId' - The identifier of the virtual private gateway that is at the Amazon Web
-- Services side of the VPN connection.
--
-- 'category', 'awsEc2VpnConnectionDetails_category' - The category of the VPN connection. @VPN@ indicates an Amazon Web
-- Services VPN connection. @VPN-Classic@ indicates an Amazon Web Services
-- Classic VPN connection.
--
-- 'vgwTelemetry', 'awsEc2VpnConnectionDetails_vgwTelemetry' - Information about the VPN tunnel.
--
-- 'routes', 'awsEc2VpnConnectionDetails_routes' - The static routes that are associated with the VPN connection.
newAwsEc2VpnConnectionDetails ::
  AwsEc2VpnConnectionDetails
newAwsEc2VpnConnectionDetails =
  AwsEc2VpnConnectionDetails'
    { type' =
        Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      customerGatewayConfiguration = Prelude.Nothing,
      vpnConnectionId = Prelude.Nothing,
      state = Prelude.Nothing,
      options = Prelude.Nothing,
      customerGatewayId = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing,
      category = Prelude.Nothing,
      vgwTelemetry = Prelude.Nothing,
      routes = Prelude.Nothing
    }

-- | The type of VPN connection.
awsEc2VpnConnectionDetails_type :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_type = Lens.lens (\AwsEc2VpnConnectionDetails' {type'} -> type') (\s@AwsEc2VpnConnectionDetails' {} a -> s {type' = a} :: AwsEc2VpnConnectionDetails)

-- | The identifier of the transit gateway that is associated with the VPN
-- connection.
awsEc2VpnConnectionDetails_transitGatewayId :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_transitGatewayId = Lens.lens (\AwsEc2VpnConnectionDetails' {transitGatewayId} -> transitGatewayId) (\s@AwsEc2VpnConnectionDetails' {} a -> s {transitGatewayId = a} :: AwsEc2VpnConnectionDetails)

-- | The configuration information for the VPN connection\'s customer
-- gateway, in the native XML format.
awsEc2VpnConnectionDetails_customerGatewayConfiguration :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_customerGatewayConfiguration = Lens.lens (\AwsEc2VpnConnectionDetails' {customerGatewayConfiguration} -> customerGatewayConfiguration) (\s@AwsEc2VpnConnectionDetails' {} a -> s {customerGatewayConfiguration = a} :: AwsEc2VpnConnectionDetails)

-- | The identifier of the VPN connection.
awsEc2VpnConnectionDetails_vpnConnectionId :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_vpnConnectionId = Lens.lens (\AwsEc2VpnConnectionDetails' {vpnConnectionId} -> vpnConnectionId) (\s@AwsEc2VpnConnectionDetails' {} a -> s {vpnConnectionId = a} :: AwsEc2VpnConnectionDetails)

-- | The current state of the VPN connection. Valid values are as follows:
--
-- -   @available@
--
-- -   @deleted@
--
-- -   @deleting@
--
-- -   @pending@
awsEc2VpnConnectionDetails_state :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_state = Lens.lens (\AwsEc2VpnConnectionDetails' {state} -> state) (\s@AwsEc2VpnConnectionDetails' {} a -> s {state = a} :: AwsEc2VpnConnectionDetails)

-- | The VPN connection options.
awsEc2VpnConnectionDetails_options :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe AwsEc2VpnConnectionOptionsDetails)
awsEc2VpnConnectionDetails_options = Lens.lens (\AwsEc2VpnConnectionDetails' {options} -> options) (\s@AwsEc2VpnConnectionDetails' {} a -> s {options = a} :: AwsEc2VpnConnectionDetails)

-- | The identifier of the customer gateway that is at your end of the VPN
-- connection.
awsEc2VpnConnectionDetails_customerGatewayId :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_customerGatewayId = Lens.lens (\AwsEc2VpnConnectionDetails' {customerGatewayId} -> customerGatewayId) (\s@AwsEc2VpnConnectionDetails' {} a -> s {customerGatewayId = a} :: AwsEc2VpnConnectionDetails)

-- | The identifier of the virtual private gateway that is at the Amazon Web
-- Services side of the VPN connection.
awsEc2VpnConnectionDetails_vpnGatewayId :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_vpnGatewayId = Lens.lens (\AwsEc2VpnConnectionDetails' {vpnGatewayId} -> vpnGatewayId) (\s@AwsEc2VpnConnectionDetails' {} a -> s {vpnGatewayId = a} :: AwsEc2VpnConnectionDetails)

-- | The category of the VPN connection. @VPN@ indicates an Amazon Web
-- Services VPN connection. @VPN-Classic@ indicates an Amazon Web Services
-- Classic VPN connection.
awsEc2VpnConnectionDetails_category :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionDetails_category = Lens.lens (\AwsEc2VpnConnectionDetails' {category} -> category) (\s@AwsEc2VpnConnectionDetails' {} a -> s {category = a} :: AwsEc2VpnConnectionDetails)

-- | Information about the VPN tunnel.
awsEc2VpnConnectionDetails_vgwTelemetry :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe [AwsEc2VpnConnectionVgwTelemetryDetails])
awsEc2VpnConnectionDetails_vgwTelemetry = Lens.lens (\AwsEc2VpnConnectionDetails' {vgwTelemetry} -> vgwTelemetry) (\s@AwsEc2VpnConnectionDetails' {} a -> s {vgwTelemetry = a} :: AwsEc2VpnConnectionDetails) Prelude.. Lens.mapping Lens.coerced

-- | The static routes that are associated with the VPN connection.
awsEc2VpnConnectionDetails_routes :: Lens.Lens' AwsEc2VpnConnectionDetails (Prelude.Maybe [AwsEc2VpnConnectionRoutesDetails])
awsEc2VpnConnectionDetails_routes = Lens.lens (\AwsEc2VpnConnectionDetails' {routes} -> routes) (\s@AwsEc2VpnConnectionDetails' {} a -> s {routes = a} :: AwsEc2VpnConnectionDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsEc2VpnConnectionDetails where
  parseJSON =
    Data.withObject
      "AwsEc2VpnConnectionDetails"
      ( \x ->
          AwsEc2VpnConnectionDetails'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "TransitGatewayId")
            Prelude.<*> (x Data..:? "CustomerGatewayConfiguration")
            Prelude.<*> (x Data..:? "VpnConnectionId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "CustomerGatewayId")
            Prelude.<*> (x Data..:? "VpnGatewayId")
            Prelude.<*> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "VgwTelemetry" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Routes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsEc2VpnConnectionDetails where
  hashWithSalt _salt AwsEc2VpnConnectionDetails' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` customerGatewayConfiguration
      `Prelude.hashWithSalt` vpnConnectionId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` customerGatewayId
      `Prelude.hashWithSalt` vpnGatewayId
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` vgwTelemetry
      `Prelude.hashWithSalt` routes

instance Prelude.NFData AwsEc2VpnConnectionDetails where
  rnf AwsEc2VpnConnectionDetails' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf customerGatewayConfiguration
      `Prelude.seq` Prelude.rnf vpnConnectionId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf customerGatewayId
      `Prelude.seq` Prelude.rnf vpnGatewayId
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf vgwTelemetry
      `Prelude.seq` Prelude.rnf routes

instance Data.ToJSON AwsEc2VpnConnectionDetails where
  toJSON AwsEc2VpnConnectionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("TransitGatewayId" Data..=)
              Prelude.<$> transitGatewayId,
            ("CustomerGatewayConfiguration" Data..=)
              Prelude.<$> customerGatewayConfiguration,
            ("VpnConnectionId" Data..=)
              Prelude.<$> vpnConnectionId,
            ("State" Data..=) Prelude.<$> state,
            ("Options" Data..=) Prelude.<$> options,
            ("CustomerGatewayId" Data..=)
              Prelude.<$> customerGatewayId,
            ("VpnGatewayId" Data..=) Prelude.<$> vpnGatewayId,
            ("Category" Data..=) Prelude.<$> category,
            ("VgwTelemetry" Data..=) Prelude.<$> vgwTelemetry,
            ("Routes" Data..=) Prelude.<$> routes
          ]
      )

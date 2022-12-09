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
-- Module      : Amazonka.NetworkManager.Types.CustomerGatewayAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CustomerGatewayAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.CustomerGatewayAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes the association between a customer gateway, a device, and a
-- link.
--
-- /See:/ 'newCustomerGatewayAssociation' smart constructor.
data CustomerGatewayAssociation = CustomerGatewayAssociation'
  { -- | The Amazon Resource Name (ARN) of the customer gateway.
    customerGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The association state.
    state :: Prelude.Maybe CustomerGatewayAssociationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayArn', 'customerGatewayAssociation_customerGatewayArn' - The Amazon Resource Name (ARN) of the customer gateway.
--
-- 'deviceId', 'customerGatewayAssociation_deviceId' - The ID of the device.
--
-- 'globalNetworkId', 'customerGatewayAssociation_globalNetworkId' - The ID of the global network.
--
-- 'linkId', 'customerGatewayAssociation_linkId' - The ID of the link.
--
-- 'state', 'customerGatewayAssociation_state' - The association state.
newCustomerGatewayAssociation ::
  CustomerGatewayAssociation
newCustomerGatewayAssociation =
  CustomerGatewayAssociation'
    { customerGatewayArn =
        Prelude.Nothing,
      deviceId = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      linkId = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the customer gateway.
customerGatewayAssociation_customerGatewayArn :: Lens.Lens' CustomerGatewayAssociation (Prelude.Maybe Prelude.Text)
customerGatewayAssociation_customerGatewayArn = Lens.lens (\CustomerGatewayAssociation' {customerGatewayArn} -> customerGatewayArn) (\s@CustomerGatewayAssociation' {} a -> s {customerGatewayArn = a} :: CustomerGatewayAssociation)

-- | The ID of the device.
customerGatewayAssociation_deviceId :: Lens.Lens' CustomerGatewayAssociation (Prelude.Maybe Prelude.Text)
customerGatewayAssociation_deviceId = Lens.lens (\CustomerGatewayAssociation' {deviceId} -> deviceId) (\s@CustomerGatewayAssociation' {} a -> s {deviceId = a} :: CustomerGatewayAssociation)

-- | The ID of the global network.
customerGatewayAssociation_globalNetworkId :: Lens.Lens' CustomerGatewayAssociation (Prelude.Maybe Prelude.Text)
customerGatewayAssociation_globalNetworkId = Lens.lens (\CustomerGatewayAssociation' {globalNetworkId} -> globalNetworkId) (\s@CustomerGatewayAssociation' {} a -> s {globalNetworkId = a} :: CustomerGatewayAssociation)

-- | The ID of the link.
customerGatewayAssociation_linkId :: Lens.Lens' CustomerGatewayAssociation (Prelude.Maybe Prelude.Text)
customerGatewayAssociation_linkId = Lens.lens (\CustomerGatewayAssociation' {linkId} -> linkId) (\s@CustomerGatewayAssociation' {} a -> s {linkId = a} :: CustomerGatewayAssociation)

-- | The association state.
customerGatewayAssociation_state :: Lens.Lens' CustomerGatewayAssociation (Prelude.Maybe CustomerGatewayAssociationState)
customerGatewayAssociation_state = Lens.lens (\CustomerGatewayAssociation' {state} -> state) (\s@CustomerGatewayAssociation' {} a -> s {state = a} :: CustomerGatewayAssociation)

instance Data.FromJSON CustomerGatewayAssociation where
  parseJSON =
    Data.withObject
      "CustomerGatewayAssociation"
      ( \x ->
          CustomerGatewayAssociation'
            Prelude.<$> (x Data..:? "CustomerGatewayArn")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "LinkId")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable CustomerGatewayAssociation where
  hashWithSalt _salt CustomerGatewayAssociation' {..} =
    _salt `Prelude.hashWithSalt` customerGatewayArn
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` state

instance Prelude.NFData CustomerGatewayAssociation where
  rnf CustomerGatewayAssociation' {..} =
    Prelude.rnf customerGatewayArn
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf state

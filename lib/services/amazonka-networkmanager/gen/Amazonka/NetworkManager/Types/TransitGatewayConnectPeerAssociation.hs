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
-- Module      : Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.TransitGatewayConnectPeerAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway Connect peer association.
--
-- /See:/ 'newTransitGatewayConnectPeerAssociation' smart constructor.
data TransitGatewayConnectPeerAssociation = TransitGatewayConnectPeerAssociation'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The state of the association.
    state :: Prelude.Maybe TransitGatewayConnectPeerAssociationState,
    -- | The Amazon Resource Name (ARN) of the transit gateway Connect peer.
    transitGatewayConnectPeerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConnectPeerAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'transitGatewayConnectPeerAssociation_globalNetworkId' - The ID of the global network.
--
-- 'linkId', 'transitGatewayConnectPeerAssociation_linkId' - The ID of the link.
--
-- 'deviceId', 'transitGatewayConnectPeerAssociation_deviceId' - The ID of the device.
--
-- 'state', 'transitGatewayConnectPeerAssociation_state' - The state of the association.
--
-- 'transitGatewayConnectPeerArn', 'transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn' - The Amazon Resource Name (ARN) of the transit gateway Connect peer.
newTransitGatewayConnectPeerAssociation ::
  TransitGatewayConnectPeerAssociation
newTransitGatewayConnectPeerAssociation =
  TransitGatewayConnectPeerAssociation'
    { globalNetworkId =
        Prelude.Nothing,
      linkId = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayConnectPeerArn =
        Prelude.Nothing
    }

-- | The ID of the global network.
transitGatewayConnectPeerAssociation_globalNetworkId :: Lens.Lens' TransitGatewayConnectPeerAssociation (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerAssociation_globalNetworkId = Lens.lens (\TransitGatewayConnectPeerAssociation' {globalNetworkId} -> globalNetworkId) (\s@TransitGatewayConnectPeerAssociation' {} a -> s {globalNetworkId = a} :: TransitGatewayConnectPeerAssociation)

-- | The ID of the link.
transitGatewayConnectPeerAssociation_linkId :: Lens.Lens' TransitGatewayConnectPeerAssociation (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerAssociation_linkId = Lens.lens (\TransitGatewayConnectPeerAssociation' {linkId} -> linkId) (\s@TransitGatewayConnectPeerAssociation' {} a -> s {linkId = a} :: TransitGatewayConnectPeerAssociation)

-- | The ID of the device.
transitGatewayConnectPeerAssociation_deviceId :: Lens.Lens' TransitGatewayConnectPeerAssociation (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerAssociation_deviceId = Lens.lens (\TransitGatewayConnectPeerAssociation' {deviceId} -> deviceId) (\s@TransitGatewayConnectPeerAssociation' {} a -> s {deviceId = a} :: TransitGatewayConnectPeerAssociation)

-- | The state of the association.
transitGatewayConnectPeerAssociation_state :: Lens.Lens' TransitGatewayConnectPeerAssociation (Prelude.Maybe TransitGatewayConnectPeerAssociationState)
transitGatewayConnectPeerAssociation_state = Lens.lens (\TransitGatewayConnectPeerAssociation' {state} -> state) (\s@TransitGatewayConnectPeerAssociation' {} a -> s {state = a} :: TransitGatewayConnectPeerAssociation)

-- | The Amazon Resource Name (ARN) of the transit gateway Connect peer.
transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn :: Lens.Lens' TransitGatewayConnectPeerAssociation (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeerAssociation_transitGatewayConnectPeerArn = Lens.lens (\TransitGatewayConnectPeerAssociation' {transitGatewayConnectPeerArn} -> transitGatewayConnectPeerArn) (\s@TransitGatewayConnectPeerAssociation' {} a -> s {transitGatewayConnectPeerArn = a} :: TransitGatewayConnectPeerAssociation)

instance
  Core.FromJSON
    TransitGatewayConnectPeerAssociation
  where
  parseJSON =
    Core.withObject
      "TransitGatewayConnectPeerAssociation"
      ( \x ->
          TransitGatewayConnectPeerAssociation'
            Prelude.<$> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "LinkId")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "TransitGatewayConnectPeerArn")
      )

instance
  Prelude.Hashable
    TransitGatewayConnectPeerAssociation
  where
  hashWithSalt
    _salt
    TransitGatewayConnectPeerAssociation' {..} =
      _salt `Prelude.hashWithSalt` globalNetworkId
        `Prelude.hashWithSalt` linkId
        `Prelude.hashWithSalt` deviceId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` transitGatewayConnectPeerArn

instance
  Prelude.NFData
    TransitGatewayConnectPeerAssociation
  where
  rnf TransitGatewayConnectPeerAssociation' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeerArn

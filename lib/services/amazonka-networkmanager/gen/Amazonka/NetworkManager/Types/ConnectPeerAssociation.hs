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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ConnectPeerAssociationState
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network Connect peer association.
--
-- /See:/ 'newConnectPeerAssociation' smart constructor.
data ConnectPeerAssociation = ConnectPeerAssociation'
  { -- | The ID of the Connect peer.
    connectPeerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device to connect to.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The state of the Connect peer association.
    state :: Prelude.Maybe ConnectPeerAssociationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectPeerAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerId', 'connectPeerAssociation_connectPeerId' - The ID of the Connect peer.
--
-- 'deviceId', 'connectPeerAssociation_deviceId' - The ID of the device to connect to.
--
-- 'globalNetworkId', 'connectPeerAssociation_globalNetworkId' - The ID of the global network.
--
-- 'linkId', 'connectPeerAssociation_linkId' - The ID of the link.
--
-- 'state', 'connectPeerAssociation_state' - The state of the Connect peer association.
newConnectPeerAssociation ::
  ConnectPeerAssociation
newConnectPeerAssociation =
  ConnectPeerAssociation'
    { connectPeerId =
        Prelude.Nothing,
      deviceId = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      linkId = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The ID of the Connect peer.
connectPeerAssociation_connectPeerId :: Lens.Lens' ConnectPeerAssociation (Prelude.Maybe Prelude.Text)
connectPeerAssociation_connectPeerId = Lens.lens (\ConnectPeerAssociation' {connectPeerId} -> connectPeerId) (\s@ConnectPeerAssociation' {} a -> s {connectPeerId = a} :: ConnectPeerAssociation)

-- | The ID of the device to connect to.
connectPeerAssociation_deviceId :: Lens.Lens' ConnectPeerAssociation (Prelude.Maybe Prelude.Text)
connectPeerAssociation_deviceId = Lens.lens (\ConnectPeerAssociation' {deviceId} -> deviceId) (\s@ConnectPeerAssociation' {} a -> s {deviceId = a} :: ConnectPeerAssociation)

-- | The ID of the global network.
connectPeerAssociation_globalNetworkId :: Lens.Lens' ConnectPeerAssociation (Prelude.Maybe Prelude.Text)
connectPeerAssociation_globalNetworkId = Lens.lens (\ConnectPeerAssociation' {globalNetworkId} -> globalNetworkId) (\s@ConnectPeerAssociation' {} a -> s {globalNetworkId = a} :: ConnectPeerAssociation)

-- | The ID of the link.
connectPeerAssociation_linkId :: Lens.Lens' ConnectPeerAssociation (Prelude.Maybe Prelude.Text)
connectPeerAssociation_linkId = Lens.lens (\ConnectPeerAssociation' {linkId} -> linkId) (\s@ConnectPeerAssociation' {} a -> s {linkId = a} :: ConnectPeerAssociation)

-- | The state of the Connect peer association.
connectPeerAssociation_state :: Lens.Lens' ConnectPeerAssociation (Prelude.Maybe ConnectPeerAssociationState)
connectPeerAssociation_state = Lens.lens (\ConnectPeerAssociation' {state} -> state) (\s@ConnectPeerAssociation' {} a -> s {state = a} :: ConnectPeerAssociation)

instance Data.FromJSON ConnectPeerAssociation where
  parseJSON =
    Data.withObject
      "ConnectPeerAssociation"
      ( \x ->
          ConnectPeerAssociation'
            Prelude.<$> (x Data..:? "ConnectPeerId")
            Prelude.<*> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "LinkId")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ConnectPeerAssociation where
  hashWithSalt _salt ConnectPeerAssociation' {..} =
    _salt `Prelude.hashWithSalt` connectPeerId
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` state

instance Prelude.NFData ConnectPeerAssociation where
  rnf ConnectPeerAssociation' {..} =
    Prelude.rnf connectPeerId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf state

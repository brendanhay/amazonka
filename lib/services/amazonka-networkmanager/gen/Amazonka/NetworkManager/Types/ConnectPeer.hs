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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.ConnectPeerConfiguration
import Amazonka.NetworkManager.Types.ConnectPeerState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network Connect peer.
--
-- /See:/ 'newConnectPeer' smart constructor.
data ConnectPeer = ConnectPeer'
  { -- | The list of key-value tags associated with the Connect peer.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the attachment to connect.
    connectAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The configuration of the Connect peer.
    configuration :: Prelude.Maybe ConnectPeerConfiguration,
    -- | The state of the Connect peer.
    state :: Prelude.Maybe ConnectPeerState,
    -- | The Connect peer Regions where edges are located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Connect peer.
    connectPeerId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the Connect peer was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'connectPeer_tags' - The list of key-value tags associated with the Connect peer.
--
-- 'coreNetworkId', 'connectPeer_coreNetworkId' - The ID of a core network.
--
-- 'connectAttachmentId', 'connectPeer_connectAttachmentId' - The ID of the attachment to connect.
--
-- 'configuration', 'connectPeer_configuration' - The configuration of the Connect peer.
--
-- 'state', 'connectPeer_state' - The state of the Connect peer.
--
-- 'edgeLocation', 'connectPeer_edgeLocation' - The Connect peer Regions where edges are located.
--
-- 'connectPeerId', 'connectPeer_connectPeerId' - The ID of the Connect peer.
--
-- 'createdAt', 'connectPeer_createdAt' - The timestamp when the Connect peer was created.
newConnectPeer ::
  ConnectPeer
newConnectPeer =
  ConnectPeer'
    { tags = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      connectAttachmentId = Prelude.Nothing,
      configuration = Prelude.Nothing,
      state = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      connectPeerId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The list of key-value tags associated with the Connect peer.
connectPeer_tags :: Lens.Lens' ConnectPeer (Prelude.Maybe [Tag])
connectPeer_tags = Lens.lens (\ConnectPeer' {tags} -> tags) (\s@ConnectPeer' {} a -> s {tags = a} :: ConnectPeer) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a core network.
connectPeer_coreNetworkId :: Lens.Lens' ConnectPeer (Prelude.Maybe Prelude.Text)
connectPeer_coreNetworkId = Lens.lens (\ConnectPeer' {coreNetworkId} -> coreNetworkId) (\s@ConnectPeer' {} a -> s {coreNetworkId = a} :: ConnectPeer)

-- | The ID of the attachment to connect.
connectPeer_connectAttachmentId :: Lens.Lens' ConnectPeer (Prelude.Maybe Prelude.Text)
connectPeer_connectAttachmentId = Lens.lens (\ConnectPeer' {connectAttachmentId} -> connectAttachmentId) (\s@ConnectPeer' {} a -> s {connectAttachmentId = a} :: ConnectPeer)

-- | The configuration of the Connect peer.
connectPeer_configuration :: Lens.Lens' ConnectPeer (Prelude.Maybe ConnectPeerConfiguration)
connectPeer_configuration = Lens.lens (\ConnectPeer' {configuration} -> configuration) (\s@ConnectPeer' {} a -> s {configuration = a} :: ConnectPeer)

-- | The state of the Connect peer.
connectPeer_state :: Lens.Lens' ConnectPeer (Prelude.Maybe ConnectPeerState)
connectPeer_state = Lens.lens (\ConnectPeer' {state} -> state) (\s@ConnectPeer' {} a -> s {state = a} :: ConnectPeer)

-- | The Connect peer Regions where edges are located.
connectPeer_edgeLocation :: Lens.Lens' ConnectPeer (Prelude.Maybe Prelude.Text)
connectPeer_edgeLocation = Lens.lens (\ConnectPeer' {edgeLocation} -> edgeLocation) (\s@ConnectPeer' {} a -> s {edgeLocation = a} :: ConnectPeer)

-- | The ID of the Connect peer.
connectPeer_connectPeerId :: Lens.Lens' ConnectPeer (Prelude.Maybe Prelude.Text)
connectPeer_connectPeerId = Lens.lens (\ConnectPeer' {connectPeerId} -> connectPeerId) (\s@ConnectPeer' {} a -> s {connectPeerId = a} :: ConnectPeer)

-- | The timestamp when the Connect peer was created.
connectPeer_createdAt :: Lens.Lens' ConnectPeer (Prelude.Maybe Prelude.UTCTime)
connectPeer_createdAt = Lens.lens (\ConnectPeer' {createdAt} -> createdAt) (\s@ConnectPeer' {} a -> s {createdAt = a} :: ConnectPeer) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ConnectPeer where
  parseJSON =
    Core.withObject
      "ConnectPeer"
      ( \x ->
          ConnectPeer'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "ConnectAttachmentId")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "EdgeLocation")
            Prelude.<*> (x Core..:? "ConnectPeerId")
            Prelude.<*> (x Core..:? "CreatedAt")
      )

instance Prelude.Hashable ConnectPeer where
  hashWithSalt _salt ConnectPeer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` connectAttachmentId
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` connectPeerId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData ConnectPeer where
  rnf ConnectPeer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf connectAttachmentId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf connectPeerId
      `Prelude.seq` Prelude.rnf createdAt

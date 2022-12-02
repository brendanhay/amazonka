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
-- Module      : Amazonka.NetworkManager.Types.ConnectPeerSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ConnectPeerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ConnectPeerState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Summary description of a Connect peer.
--
-- /See:/ 'newConnectPeerSummary' smart constructor.
data ConnectPeerSummary = ConnectPeerSummary'
  { -- | The list of key-value tags associated with the Connect peer summary.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a Connect peer attachment.
    connectAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The Region where the edge is located.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The state of a Connect peer.
    connectPeerState :: Prelude.Maybe ConnectPeerState,
    -- | The ID of a Connect peer.
    connectPeerId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when a Connect peer was created.
    createdAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectPeerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'connectPeerSummary_tags' - The list of key-value tags associated with the Connect peer summary.
--
-- 'coreNetworkId', 'connectPeerSummary_coreNetworkId' - The ID of a core network.
--
-- 'connectAttachmentId', 'connectPeerSummary_connectAttachmentId' - The ID of a Connect peer attachment.
--
-- 'edgeLocation', 'connectPeerSummary_edgeLocation' - The Region where the edge is located.
--
-- 'connectPeerState', 'connectPeerSummary_connectPeerState' - The state of a Connect peer.
--
-- 'connectPeerId', 'connectPeerSummary_connectPeerId' - The ID of a Connect peer.
--
-- 'createdAt', 'connectPeerSummary_createdAt' - The timestamp when a Connect peer was created.
newConnectPeerSummary ::
  ConnectPeerSummary
newConnectPeerSummary =
  ConnectPeerSummary'
    { tags = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      connectAttachmentId = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      connectPeerState = Prelude.Nothing,
      connectPeerId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The list of key-value tags associated with the Connect peer summary.
connectPeerSummary_tags :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe [Tag])
connectPeerSummary_tags = Lens.lens (\ConnectPeerSummary' {tags} -> tags) (\s@ConnectPeerSummary' {} a -> s {tags = a} :: ConnectPeerSummary) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a core network.
connectPeerSummary_coreNetworkId :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe Prelude.Text)
connectPeerSummary_coreNetworkId = Lens.lens (\ConnectPeerSummary' {coreNetworkId} -> coreNetworkId) (\s@ConnectPeerSummary' {} a -> s {coreNetworkId = a} :: ConnectPeerSummary)

-- | The ID of a Connect peer attachment.
connectPeerSummary_connectAttachmentId :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe Prelude.Text)
connectPeerSummary_connectAttachmentId = Lens.lens (\ConnectPeerSummary' {connectAttachmentId} -> connectAttachmentId) (\s@ConnectPeerSummary' {} a -> s {connectAttachmentId = a} :: ConnectPeerSummary)

-- | The Region where the edge is located.
connectPeerSummary_edgeLocation :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe Prelude.Text)
connectPeerSummary_edgeLocation = Lens.lens (\ConnectPeerSummary' {edgeLocation} -> edgeLocation) (\s@ConnectPeerSummary' {} a -> s {edgeLocation = a} :: ConnectPeerSummary)

-- | The state of a Connect peer.
connectPeerSummary_connectPeerState :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe ConnectPeerState)
connectPeerSummary_connectPeerState = Lens.lens (\ConnectPeerSummary' {connectPeerState} -> connectPeerState) (\s@ConnectPeerSummary' {} a -> s {connectPeerState = a} :: ConnectPeerSummary)

-- | The ID of a Connect peer.
connectPeerSummary_connectPeerId :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe Prelude.Text)
connectPeerSummary_connectPeerId = Lens.lens (\ConnectPeerSummary' {connectPeerId} -> connectPeerId) (\s@ConnectPeerSummary' {} a -> s {connectPeerId = a} :: ConnectPeerSummary)

-- | The timestamp when a Connect peer was created.
connectPeerSummary_createdAt :: Lens.Lens' ConnectPeerSummary (Prelude.Maybe Prelude.UTCTime)
connectPeerSummary_createdAt = Lens.lens (\ConnectPeerSummary' {createdAt} -> createdAt) (\s@ConnectPeerSummary' {} a -> s {createdAt = a} :: ConnectPeerSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ConnectPeerSummary where
  parseJSON =
    Data.withObject
      "ConnectPeerSummary"
      ( \x ->
          ConnectPeerSummary'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "ConnectAttachmentId")
            Prelude.<*> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "ConnectPeerState")
            Prelude.<*> (x Data..:? "ConnectPeerId")
            Prelude.<*> (x Data..:? "CreatedAt")
      )

instance Prelude.Hashable ConnectPeerSummary where
  hashWithSalt _salt ConnectPeerSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` connectAttachmentId
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` connectPeerState
      `Prelude.hashWithSalt` connectPeerId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData ConnectPeerSummary where
  rnf ConnectPeerSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf connectAttachmentId
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf connectPeerState
      `Prelude.seq` Prelude.rnf connectPeerId
      `Prelude.seq` Prelude.rnf createdAt

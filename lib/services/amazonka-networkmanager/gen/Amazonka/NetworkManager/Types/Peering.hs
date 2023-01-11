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
-- Module      : Amazonka.NetworkManager.Types.Peering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Peering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.PeeringState
import Amazonka.NetworkManager.Types.PeeringType
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a peering connection.
--
-- /See:/ 'newPeering' smart constructor.
data Peering = Peering'
  { -- | The ARN of a core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the core network for the peering request.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the attachment peer was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The edge location for the peer.
    edgeLocation :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account owner.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the peering attachment.
    peeringId :: Prelude.Maybe Prelude.Text,
    -- | The type of peering. This will be @TRANSIT_GATEWAY@.
    peeringType :: Prelude.Maybe PeeringType,
    -- | The resource ARN of the peer.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the peering connection.
    state :: Prelude.Maybe PeeringState,
    -- | The list of key-value tags associated with the peering.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Peering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkArn', 'peering_coreNetworkArn' - The ARN of a core network.
--
-- 'coreNetworkId', 'peering_coreNetworkId' - The ID of the core network for the peering request.
--
-- 'createdAt', 'peering_createdAt' - The timestamp when the attachment peer was created.
--
-- 'edgeLocation', 'peering_edgeLocation' - The edge location for the peer.
--
-- 'ownerAccountId', 'peering_ownerAccountId' - The ID of the account owner.
--
-- 'peeringId', 'peering_peeringId' - The ID of the peering attachment.
--
-- 'peeringType', 'peering_peeringType' - The type of peering. This will be @TRANSIT_GATEWAY@.
--
-- 'resourceArn', 'peering_resourceArn' - The resource ARN of the peer.
--
-- 'state', 'peering_state' - The current state of the peering connection.
--
-- 'tags', 'peering_tags' - The list of key-value tags associated with the peering.
newPeering ::
  Peering
newPeering =
  Peering'
    { coreNetworkArn = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      edgeLocation = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      peeringId = Prelude.Nothing,
      peeringType = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of a core network.
peering_coreNetworkArn :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_coreNetworkArn = Lens.lens (\Peering' {coreNetworkArn} -> coreNetworkArn) (\s@Peering' {} a -> s {coreNetworkArn = a} :: Peering)

-- | The ID of the core network for the peering request.
peering_coreNetworkId :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_coreNetworkId = Lens.lens (\Peering' {coreNetworkId} -> coreNetworkId) (\s@Peering' {} a -> s {coreNetworkId = a} :: Peering)

-- | The timestamp when the attachment peer was created.
peering_createdAt :: Lens.Lens' Peering (Prelude.Maybe Prelude.UTCTime)
peering_createdAt = Lens.lens (\Peering' {createdAt} -> createdAt) (\s@Peering' {} a -> s {createdAt = a} :: Peering) Prelude.. Lens.mapping Data._Time

-- | The edge location for the peer.
peering_edgeLocation :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_edgeLocation = Lens.lens (\Peering' {edgeLocation} -> edgeLocation) (\s@Peering' {} a -> s {edgeLocation = a} :: Peering)

-- | The ID of the account owner.
peering_ownerAccountId :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_ownerAccountId = Lens.lens (\Peering' {ownerAccountId} -> ownerAccountId) (\s@Peering' {} a -> s {ownerAccountId = a} :: Peering)

-- | The ID of the peering attachment.
peering_peeringId :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_peeringId = Lens.lens (\Peering' {peeringId} -> peeringId) (\s@Peering' {} a -> s {peeringId = a} :: Peering)

-- | The type of peering. This will be @TRANSIT_GATEWAY@.
peering_peeringType :: Lens.Lens' Peering (Prelude.Maybe PeeringType)
peering_peeringType = Lens.lens (\Peering' {peeringType} -> peeringType) (\s@Peering' {} a -> s {peeringType = a} :: Peering)

-- | The resource ARN of the peer.
peering_resourceArn :: Lens.Lens' Peering (Prelude.Maybe Prelude.Text)
peering_resourceArn = Lens.lens (\Peering' {resourceArn} -> resourceArn) (\s@Peering' {} a -> s {resourceArn = a} :: Peering)

-- | The current state of the peering connection.
peering_state :: Lens.Lens' Peering (Prelude.Maybe PeeringState)
peering_state = Lens.lens (\Peering' {state} -> state) (\s@Peering' {} a -> s {state = a} :: Peering)

-- | The list of key-value tags associated with the peering.
peering_tags :: Lens.Lens' Peering (Prelude.Maybe [Tag])
peering_tags = Lens.lens (\Peering' {tags} -> tags) (\s@Peering' {} a -> s {tags = a} :: Peering) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Peering where
  parseJSON =
    Data.withObject
      "Peering"
      ( \x ->
          Peering'
            Prelude.<$> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "EdgeLocation")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "PeeringId")
            Prelude.<*> (x Data..:? "PeeringType")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Peering where
  hashWithSalt _salt Peering' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` edgeLocation
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` peeringId
      `Prelude.hashWithSalt` peeringType
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Peering where
  rnf Peering' {..} =
    Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf edgeLocation
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf peeringId
      `Prelude.seq` Prelude.rnf peeringType
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags

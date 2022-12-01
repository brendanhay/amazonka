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
-- Module      : Amazonka.NetworkManager.Types.CoreNetwork
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.CoreNetworkEdge
import Amazonka.NetworkManager.Types.CoreNetworkSegment
import Amazonka.NetworkManager.Types.CoreNetworkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network.
--
-- /See:/ 'newCoreNetwork' smart constructor.
data CoreNetwork = CoreNetwork'
  { -- | The ID of the global network that your core network is a part of.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value tags associated with a core network.
    tags :: Prelude.Maybe [Tag],
    -- | The edges within a core network.
    edges :: Prelude.Maybe [CoreNetworkEdge],
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The current state of a core network.
    state :: Prelude.Maybe CoreNetworkState,
    -- | The description of a core network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when a core network was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The segments within a core network.
    segments :: Prelude.Maybe [CoreNetworkSegment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'coreNetwork_globalNetworkId' - The ID of the global network that your core network is a part of.
--
-- 'tags', 'coreNetwork_tags' - The list of key-value tags associated with a core network.
--
-- 'edges', 'coreNetwork_edges' - The edges within a core network.
--
-- 'coreNetworkId', 'coreNetwork_coreNetworkId' - The ID of a core network.
--
-- 'state', 'coreNetwork_state' - The current state of a core network.
--
-- 'description', 'coreNetwork_description' - The description of a core network.
--
-- 'coreNetworkArn', 'coreNetwork_coreNetworkArn' - The ARN of a core network.
--
-- 'createdAt', 'coreNetwork_createdAt' - The timestamp when a core network was created.
--
-- 'segments', 'coreNetwork_segments' - The segments within a core network.
newCoreNetwork ::
  CoreNetwork
newCoreNetwork =
  CoreNetwork'
    { globalNetworkId = Prelude.Nothing,
      tags = Prelude.Nothing,
      edges = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      coreNetworkArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      segments = Prelude.Nothing
    }

-- | The ID of the global network that your core network is a part of.
coreNetwork_globalNetworkId :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_globalNetworkId = Lens.lens (\CoreNetwork' {globalNetworkId} -> globalNetworkId) (\s@CoreNetwork' {} a -> s {globalNetworkId = a} :: CoreNetwork)

-- | The list of key-value tags associated with a core network.
coreNetwork_tags :: Lens.Lens' CoreNetwork (Prelude.Maybe [Tag])
coreNetwork_tags = Lens.lens (\CoreNetwork' {tags} -> tags) (\s@CoreNetwork' {} a -> s {tags = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The edges within a core network.
coreNetwork_edges :: Lens.Lens' CoreNetwork (Prelude.Maybe [CoreNetworkEdge])
coreNetwork_edges = Lens.lens (\CoreNetwork' {edges} -> edges) (\s@CoreNetwork' {} a -> s {edges = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a core network.
coreNetwork_coreNetworkId :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_coreNetworkId = Lens.lens (\CoreNetwork' {coreNetworkId} -> coreNetworkId) (\s@CoreNetwork' {} a -> s {coreNetworkId = a} :: CoreNetwork)

-- | The current state of a core network.
coreNetwork_state :: Lens.Lens' CoreNetwork (Prelude.Maybe CoreNetworkState)
coreNetwork_state = Lens.lens (\CoreNetwork' {state} -> state) (\s@CoreNetwork' {} a -> s {state = a} :: CoreNetwork)

-- | The description of a core network.
coreNetwork_description :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_description = Lens.lens (\CoreNetwork' {description} -> description) (\s@CoreNetwork' {} a -> s {description = a} :: CoreNetwork)

-- | The ARN of a core network.
coreNetwork_coreNetworkArn :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_coreNetworkArn = Lens.lens (\CoreNetwork' {coreNetworkArn} -> coreNetworkArn) (\s@CoreNetwork' {} a -> s {coreNetworkArn = a} :: CoreNetwork)

-- | The timestamp when a core network was created.
coreNetwork_createdAt :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.UTCTime)
coreNetwork_createdAt = Lens.lens (\CoreNetwork' {createdAt} -> createdAt) (\s@CoreNetwork' {} a -> s {createdAt = a} :: CoreNetwork) Prelude.. Lens.mapping Core._Time

-- | The segments within a core network.
coreNetwork_segments :: Lens.Lens' CoreNetwork (Prelude.Maybe [CoreNetworkSegment])
coreNetwork_segments = Lens.lens (\CoreNetwork' {segments} -> segments) (\s@CoreNetwork' {} a -> s {segments = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CoreNetwork where
  parseJSON =
    Core.withObject
      "CoreNetwork"
      ( \x ->
          CoreNetwork'
            Prelude.<$> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Edges" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CoreNetworkArn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Segments" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CoreNetwork where
  hashWithSalt _salt CoreNetwork' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` edges
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` segments

instance Prelude.NFData CoreNetwork where
  rnf CoreNetwork' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf edges
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf segments

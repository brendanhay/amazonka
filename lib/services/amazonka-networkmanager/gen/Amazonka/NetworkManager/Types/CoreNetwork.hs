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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.CoreNetworkEdge
import Amazonka.NetworkManager.Types.CoreNetworkSegment
import Amazonka.NetworkManager.Types.CoreNetworkState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network.
--
-- /See:/ 'newCoreNetwork' smart constructor.
data CoreNetwork = CoreNetwork'
  { -- | The ARN of a core network.
    coreNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when a core network was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of a core network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The edges within a core network.
    edges :: Prelude.Maybe [CoreNetworkEdge],
    -- | The ID of the global network that your core network is a part of.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The segments within a core network.
    segments :: Prelude.Maybe [CoreNetworkSegment],
    -- | The current state of a core network.
    state :: Prelude.Maybe CoreNetworkState,
    -- | The list of key-value tags associated with a core network.
    tags :: Prelude.Maybe [Tag]
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
-- 'coreNetworkArn', 'coreNetwork_coreNetworkArn' - The ARN of a core network.
--
-- 'coreNetworkId', 'coreNetwork_coreNetworkId' - The ID of a core network.
--
-- 'createdAt', 'coreNetwork_createdAt' - The timestamp when a core network was created.
--
-- 'description', 'coreNetwork_description' - The description of a core network.
--
-- 'edges', 'coreNetwork_edges' - The edges within a core network.
--
-- 'globalNetworkId', 'coreNetwork_globalNetworkId' - The ID of the global network that your core network is a part of.
--
-- 'segments', 'coreNetwork_segments' - The segments within a core network.
--
-- 'state', 'coreNetwork_state' - The current state of a core network.
--
-- 'tags', 'coreNetwork_tags' - The list of key-value tags associated with a core network.
newCoreNetwork ::
  CoreNetwork
newCoreNetwork =
  CoreNetwork'
    { coreNetworkArn = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      edges = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      segments = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ARN of a core network.
coreNetwork_coreNetworkArn :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_coreNetworkArn = Lens.lens (\CoreNetwork' {coreNetworkArn} -> coreNetworkArn) (\s@CoreNetwork' {} a -> s {coreNetworkArn = a} :: CoreNetwork)

-- | The ID of a core network.
coreNetwork_coreNetworkId :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_coreNetworkId = Lens.lens (\CoreNetwork' {coreNetworkId} -> coreNetworkId) (\s@CoreNetwork' {} a -> s {coreNetworkId = a} :: CoreNetwork)

-- | The timestamp when a core network was created.
coreNetwork_createdAt :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.UTCTime)
coreNetwork_createdAt = Lens.lens (\CoreNetwork' {createdAt} -> createdAt) (\s@CoreNetwork' {} a -> s {createdAt = a} :: CoreNetwork) Prelude.. Lens.mapping Data._Time

-- | The description of a core network.
coreNetwork_description :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_description = Lens.lens (\CoreNetwork' {description} -> description) (\s@CoreNetwork' {} a -> s {description = a} :: CoreNetwork)

-- | The edges within a core network.
coreNetwork_edges :: Lens.Lens' CoreNetwork (Prelude.Maybe [CoreNetworkEdge])
coreNetwork_edges = Lens.lens (\CoreNetwork' {edges} -> edges) (\s@CoreNetwork' {} a -> s {edges = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the global network that your core network is a part of.
coreNetwork_globalNetworkId :: Lens.Lens' CoreNetwork (Prelude.Maybe Prelude.Text)
coreNetwork_globalNetworkId = Lens.lens (\CoreNetwork' {globalNetworkId} -> globalNetworkId) (\s@CoreNetwork' {} a -> s {globalNetworkId = a} :: CoreNetwork)

-- | The segments within a core network.
coreNetwork_segments :: Lens.Lens' CoreNetwork (Prelude.Maybe [CoreNetworkSegment])
coreNetwork_segments = Lens.lens (\CoreNetwork' {segments} -> segments) (\s@CoreNetwork' {} a -> s {segments = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The current state of a core network.
coreNetwork_state :: Lens.Lens' CoreNetwork (Prelude.Maybe CoreNetworkState)
coreNetwork_state = Lens.lens (\CoreNetwork' {state} -> state) (\s@CoreNetwork' {} a -> s {state = a} :: CoreNetwork)

-- | The list of key-value tags associated with a core network.
coreNetwork_tags :: Lens.Lens' CoreNetwork (Prelude.Maybe [Tag])
coreNetwork_tags = Lens.lens (\CoreNetwork' {tags} -> tags) (\s@CoreNetwork' {} a -> s {tags = a} :: CoreNetwork) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CoreNetwork where
  parseJSON =
    Data.withObject
      "CoreNetwork"
      ( \x ->
          CoreNetwork'
            Prelude.<$> (x Data..:? "CoreNetworkArn")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Edges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GlobalNetworkId")
            Prelude.<*> (x Data..:? "Segments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CoreNetwork where
  hashWithSalt _salt CoreNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` coreNetworkArn
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` edges
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` segments
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CoreNetwork where
  rnf CoreNetwork' {..} =
    Prelude.rnf coreNetworkArn
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf edges
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf segments
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags

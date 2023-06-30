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
-- Module      : Amazonka.MemoryDb.Types.Shard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Shard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.Node
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of nodes in a cluster. One node in the node
-- group is the read\/write primary node. All the other nodes are read-only
-- Replica nodes.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The name of the shard
    name :: Prelude.Maybe Prelude.Text,
    -- | A list containing information about individual nodes within the shard
    nodes :: Prelude.Maybe [Node],
    -- | The number of nodes in the shard
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The keyspace for this shard.
    slots :: Prelude.Maybe Prelude.Text,
    -- | The current state of this replication group - creating, available,
    -- modifying, deleting.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Shard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'shard_name' - The name of the shard
--
-- 'nodes', 'shard_nodes' - A list containing information about individual nodes within the shard
--
-- 'numberOfNodes', 'shard_numberOfNodes' - The number of nodes in the shard
--
-- 'slots', 'shard_slots' - The keyspace for this shard.
--
-- 'status', 'shard_status' - The current state of this replication group - creating, available,
-- modifying, deleting.
newShard ::
  Shard
newShard =
  Shard'
    { name = Prelude.Nothing,
      nodes = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      slots = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the shard
shard_name :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_name = Lens.lens (\Shard' {name} -> name) (\s@Shard' {} a -> s {name = a} :: Shard)

-- | A list containing information about individual nodes within the shard
shard_nodes :: Lens.Lens' Shard (Prelude.Maybe [Node])
shard_nodes = Lens.lens (\Shard' {nodes} -> nodes) (\s@Shard' {} a -> s {nodes = a} :: Shard) Prelude.. Lens.mapping Lens.coerced

-- | The number of nodes in the shard
shard_numberOfNodes :: Lens.Lens' Shard (Prelude.Maybe Prelude.Int)
shard_numberOfNodes = Lens.lens (\Shard' {numberOfNodes} -> numberOfNodes) (\s@Shard' {} a -> s {numberOfNodes = a} :: Shard)

-- | The keyspace for this shard.
shard_slots :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_slots = Lens.lens (\Shard' {slots} -> slots) (\s@Shard' {} a -> s {slots = a} :: Shard)

-- | The current state of this replication group - creating, available,
-- modifying, deleting.
shard_status :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_status = Lens.lens (\Shard' {status} -> status) (\s@Shard' {} a -> s {status = a} :: Shard)

instance Data.FromJSON Shard where
  parseJSON =
    Data.withObject
      "Shard"
      ( \x ->
          Shard'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Nodes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NumberOfNodes")
            Prelude.<*> (x Data..:? "Slots")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Shard where
  hashWithSalt _salt Shard' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nodes
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` slots
      `Prelude.hashWithSalt` status

instance Prelude.NFData Shard where
  rnf Shard' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf status

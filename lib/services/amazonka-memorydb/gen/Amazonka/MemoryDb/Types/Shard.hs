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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Shard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types.Node
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of nodes in a cluster. One node in the node
-- group is the read\/write primary node. All the other nodes are read-only
-- Replica nodes.
--
-- /See:/ 'newShard' smart constructor.
data Shard = Shard'
  { -- | The current state of this replication group - creating, available,
    -- modifying, deleting.
    status :: Prelude.Maybe Prelude.Text,
    -- | The keyspace for this shard.
    slots :: Prelude.Maybe Prelude.Text,
    -- | The number of nodes in the shard
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The name of the shard
    name :: Prelude.Maybe Prelude.Text,
    -- | A list containing information about individual nodes within the shard
    nodes :: Prelude.Maybe [Node]
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
-- 'status', 'shard_status' - The current state of this replication group - creating, available,
-- modifying, deleting.
--
-- 'slots', 'shard_slots' - The keyspace for this shard.
--
-- 'numberOfNodes', 'shard_numberOfNodes' - The number of nodes in the shard
--
-- 'name', 'shard_name' - The name of the shard
--
-- 'nodes', 'shard_nodes' - A list containing information about individual nodes within the shard
newShard ::
  Shard
newShard =
  Shard'
    { status = Prelude.Nothing,
      slots = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      name = Prelude.Nothing,
      nodes = Prelude.Nothing
    }

-- | The current state of this replication group - creating, available,
-- modifying, deleting.
shard_status :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_status = Lens.lens (\Shard' {status} -> status) (\s@Shard' {} a -> s {status = a} :: Shard)

-- | The keyspace for this shard.
shard_slots :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_slots = Lens.lens (\Shard' {slots} -> slots) (\s@Shard' {} a -> s {slots = a} :: Shard)

-- | The number of nodes in the shard
shard_numberOfNodes :: Lens.Lens' Shard (Prelude.Maybe Prelude.Int)
shard_numberOfNodes = Lens.lens (\Shard' {numberOfNodes} -> numberOfNodes) (\s@Shard' {} a -> s {numberOfNodes = a} :: Shard)

-- | The name of the shard
shard_name :: Lens.Lens' Shard (Prelude.Maybe Prelude.Text)
shard_name = Lens.lens (\Shard' {name} -> name) (\s@Shard' {} a -> s {name = a} :: Shard)

-- | A list containing information about individual nodes within the shard
shard_nodes :: Lens.Lens' Shard (Prelude.Maybe [Node])
shard_nodes = Lens.lens (\Shard' {nodes} -> nodes) (\s@Shard' {} a -> s {nodes = a} :: Shard) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Shard where
  parseJSON =
    Core.withObject
      "Shard"
      ( \x ->
          Shard'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Slots")
            Prelude.<*> (x Core..:? "NumberOfNodes")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Nodes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Shard where
  hashWithSalt salt' Shard' {..} =
    salt' `Prelude.hashWithSalt` nodes
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` slots
      `Prelude.hashWithSalt` status

instance Prelude.NFData Shard where
  rnf Shard' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf slots

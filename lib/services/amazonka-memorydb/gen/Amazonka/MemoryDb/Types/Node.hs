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
-- Module      : Amazonka.MemoryDb.Types.Node
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.Node where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types.Endpoint
import qualified Amazonka.Prelude as Prelude

-- | Represents an individual node within a cluster. Each node runs its own
-- instance of the cluster\'s protocol-compliant caching software.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The node identifier. A node name is a numeric identifier (0001, 0002,
    -- etc.). The combination of cluster name, shard name and node name
    -- uniquely identifies every node used in a customer\'s Amazon account.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update on the node
    status :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the node resides
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the node was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The hostname for connecting to this node.
    endpoint :: Prelude.Maybe Endpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'node_name' - The node identifier. A node name is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster name, shard name and node name
-- uniquely identifies every node used in a customer\'s Amazon account.
--
-- 'status', 'node_status' - The status of the service update on the node
--
-- 'availabilityZone', 'node_availabilityZone' - The Availability Zone in which the node resides
--
-- 'createTime', 'node_createTime' - The date and time when the node was created.
--
-- 'endpoint', 'node_endpoint' - The hostname for connecting to this node.
newNode ::
  Node
newNode =
  Node'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      createTime = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The node identifier. A node name is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster name, shard name and node name
-- uniquely identifies every node used in a customer\'s Amazon account.
node_name :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_name = Lens.lens (\Node' {name} -> name) (\s@Node' {} a -> s {name = a} :: Node)

-- | The status of the service update on the node
node_status :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_status = Lens.lens (\Node' {status} -> status) (\s@Node' {} a -> s {status = a} :: Node)

-- | The Availability Zone in which the node resides
node_availabilityZone :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_availabilityZone = Lens.lens (\Node' {availabilityZone} -> availabilityZone) (\s@Node' {} a -> s {availabilityZone = a} :: Node)

-- | The date and time when the node was created.
node_createTime :: Lens.Lens' Node (Prelude.Maybe Prelude.UTCTime)
node_createTime = Lens.lens (\Node' {createTime} -> createTime) (\s@Node' {} a -> s {createTime = a} :: Node) Prelude.. Lens.mapping Data._Time

-- | The hostname for connecting to this node.
node_endpoint :: Lens.Lens' Node (Prelude.Maybe Endpoint)
node_endpoint = Lens.lens (\Node' {endpoint} -> endpoint) (\s@Node' {} a -> s {endpoint = a} :: Node)

instance Data.FromJSON Node where
  parseJSON =
    Data.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "Endpoint")
      )

instance Prelude.Hashable Node where
  hashWithSalt _salt Node' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf endpoint

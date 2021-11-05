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
-- Module      : Network.AWS.MemoryDb.Types.Node
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types.Node where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MemoryDb.Types.Endpoint
import qualified Network.AWS.Prelude as Prelude

-- | Represents an individual node within a cluster. Each node runs its own
-- instance of the cluster\'s protocol-compliant caching software.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The status of the service update on the node
    status :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the node resides
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The node identifier. A node name is a numeric identifier (0001, 0002,
    -- etc.). The combination of cluster name, shard name and node name
    -- uniquely identifies every node used in a customer\'s Amazon account.
    name :: Prelude.Maybe Prelude.Text,
    -- | The hostname for connecting to this node.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The date and time when the node was created.
    createTime :: Prelude.Maybe Core.POSIX
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
-- 'status', 'node_status' - The status of the service update on the node
--
-- 'availabilityZone', 'node_availabilityZone' - The Availability Zone in which the node resides
--
-- 'name', 'node_name' - The node identifier. A node name is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster name, shard name and node name
-- uniquely identifies every node used in a customer\'s Amazon account.
--
-- 'endpoint', 'node_endpoint' - The hostname for connecting to this node.
--
-- 'createTime', 'node_createTime' - The date and time when the node was created.
newNode ::
  Node
newNode =
  Node'
    { status = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      name = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | The status of the service update on the node
node_status :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_status = Lens.lens (\Node' {status} -> status) (\s@Node' {} a -> s {status = a} :: Node)

-- | The Availability Zone in which the node resides
node_availabilityZone :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_availabilityZone = Lens.lens (\Node' {availabilityZone} -> availabilityZone) (\s@Node' {} a -> s {availabilityZone = a} :: Node)

-- | The node identifier. A node name is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster name, shard name and node name
-- uniquely identifies every node used in a customer\'s Amazon account.
node_name :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_name = Lens.lens (\Node' {name} -> name) (\s@Node' {} a -> s {name = a} :: Node)

-- | The hostname for connecting to this node.
node_endpoint :: Lens.Lens' Node (Prelude.Maybe Endpoint)
node_endpoint = Lens.lens (\Node' {endpoint} -> endpoint) (\s@Node' {} a -> s {endpoint = a} :: Node)

-- | The date and time when the node was created.
node_createTime :: Lens.Lens' Node (Prelude.Maybe Prelude.UTCTime)
node_createTime = Lens.lens (\Node' {createTime} -> createTime) (\s@Node' {} a -> s {createTime = a} :: Node) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "CreateTime")
      )

instance Prelude.Hashable Node

instance Prelude.NFData Node

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
-- Module      : Amazonka.DAX.Types.Node
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Node where

import qualified Amazonka.Core as Core
import Amazonka.DAX.Types.Endpoint
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an individual node within a DAX cluster.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The current status of the node. For example: @available@.
    nodeStatus :: Prelude.Maybe Prelude.Text,
    -- | The status of the parameter group associated with this node. For
    -- example, @in-sync@.
    parameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) in which the node has been deployed.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A system-generated identifier for the node.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the node, consisting of a DNS name and a port number.
    -- Client applications can connect directly to a node endpoint, if desired
    -- (as an alternative to allowing DAX client software to intelligently
    -- route requests and responses to nodes in the DAX cluster.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The date and time (in UNIX epoch format) when the node was launched.
    nodeCreateTime :: Prelude.Maybe Core.POSIX
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
-- 'nodeStatus', 'node_nodeStatus' - The current status of the node. For example: @available@.
--
-- 'parameterGroupStatus', 'node_parameterGroupStatus' - The status of the parameter group associated with this node. For
-- example, @in-sync@.
--
-- 'availabilityZone', 'node_availabilityZone' - The Availability Zone (AZ) in which the node has been deployed.
--
-- 'nodeId', 'node_nodeId' - A system-generated identifier for the node.
--
-- 'endpoint', 'node_endpoint' - The endpoint for the node, consisting of a DNS name and a port number.
-- Client applications can connect directly to a node endpoint, if desired
-- (as an alternative to allowing DAX client software to intelligently
-- route requests and responses to nodes in the DAX cluster.
--
-- 'nodeCreateTime', 'node_nodeCreateTime' - The date and time (in UNIX epoch format) when the node was launched.
newNode ::
  Node
newNode =
  Node'
    { nodeStatus = Prelude.Nothing,
      parameterGroupStatus = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      nodeId = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      nodeCreateTime = Prelude.Nothing
    }

-- | The current status of the node. For example: @available@.
node_nodeStatus :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_nodeStatus = Lens.lens (\Node' {nodeStatus} -> nodeStatus) (\s@Node' {} a -> s {nodeStatus = a} :: Node)

-- | The status of the parameter group associated with this node. For
-- example, @in-sync@.
node_parameterGroupStatus :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_parameterGroupStatus = Lens.lens (\Node' {parameterGroupStatus} -> parameterGroupStatus) (\s@Node' {} a -> s {parameterGroupStatus = a} :: Node)

-- | The Availability Zone (AZ) in which the node has been deployed.
node_availabilityZone :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_availabilityZone = Lens.lens (\Node' {availabilityZone} -> availabilityZone) (\s@Node' {} a -> s {availabilityZone = a} :: Node)

-- | A system-generated identifier for the node.
node_nodeId :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_nodeId = Lens.lens (\Node' {nodeId} -> nodeId) (\s@Node' {} a -> s {nodeId = a} :: Node)

-- | The endpoint for the node, consisting of a DNS name and a port number.
-- Client applications can connect directly to a node endpoint, if desired
-- (as an alternative to allowing DAX client software to intelligently
-- route requests and responses to nodes in the DAX cluster.
node_endpoint :: Lens.Lens' Node (Prelude.Maybe Endpoint)
node_endpoint = Lens.lens (\Node' {endpoint} -> endpoint) (\s@Node' {} a -> s {endpoint = a} :: Node)

-- | The date and time (in UNIX epoch format) when the node was launched.
node_nodeCreateTime :: Lens.Lens' Node (Prelude.Maybe Prelude.UTCTime)
node_nodeCreateTime = Lens.lens (\Node' {nodeCreateTime} -> nodeCreateTime) (\s@Node' {} a -> s {nodeCreateTime = a} :: Node) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Core..:? "NodeStatus")
            Prelude.<*> (x Core..:? "ParameterGroupStatus")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "NodeId")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "NodeCreateTime")
      )

instance Prelude.Hashable Node where
  hashWithSalt _salt Node' {..} =
    _salt `Prelude.hashWithSalt` nodeStatus
      `Prelude.hashWithSalt` parameterGroupStatus
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` nodeCreateTime

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf nodeStatus
      `Prelude.seq` Prelude.rnf parameterGroupStatus
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf nodeCreateTime

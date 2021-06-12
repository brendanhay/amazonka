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
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMember where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.Endpoint
import qualified Network.AWS.Lens as Lens

-- | Represents a single node within a node group (shard).
--
-- /See:/ 'newNodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { -- | The ID of the cluster to which the node belongs.
    cacheClusterId :: Core.Maybe Core.Text,
    -- | The name of the Availability Zone in which the node is located.
    preferredAvailabilityZone :: Core.Maybe Core.Text,
    -- | The information required for client programs to connect to a node for
    -- read operations. The read endpoint is only applicable on Redis (cluster
    -- mode disabled) clusters.
    readEndpoint :: Core.Maybe Endpoint,
    -- | The ID of the node within its cluster. A node ID is a numeric identifier
    -- (0001, 0002, etc.).
    cacheNodeId :: Core.Maybe Core.Text,
    -- | The outpost ARN of the node group member.
    preferredOutpostArn :: Core.Maybe Core.Text,
    -- | The role that is currently assigned to the node - @primary@ or
    -- @replica@. This member is only applicable for Redis (cluster mode
    -- disabled) replication groups.
    currentRole :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeGroupMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'nodeGroupMember_cacheClusterId' - The ID of the cluster to which the node belongs.
--
-- 'preferredAvailabilityZone', 'nodeGroupMember_preferredAvailabilityZone' - The name of the Availability Zone in which the node is located.
--
-- 'readEndpoint', 'nodeGroupMember_readEndpoint' - The information required for client programs to connect to a node for
-- read operations. The read endpoint is only applicable on Redis (cluster
-- mode disabled) clusters.
--
-- 'cacheNodeId', 'nodeGroupMember_cacheNodeId' - The ID of the node within its cluster. A node ID is a numeric identifier
-- (0001, 0002, etc.).
--
-- 'preferredOutpostArn', 'nodeGroupMember_preferredOutpostArn' - The outpost ARN of the node group member.
--
-- 'currentRole', 'nodeGroupMember_currentRole' - The role that is currently assigned to the node - @primary@ or
-- @replica@. This member is only applicable for Redis (cluster mode
-- disabled) replication groups.
newNodeGroupMember ::
  NodeGroupMember
newNodeGroupMember =
  NodeGroupMember'
    { cacheClusterId = Core.Nothing,
      preferredAvailabilityZone = Core.Nothing,
      readEndpoint = Core.Nothing,
      cacheNodeId = Core.Nothing,
      preferredOutpostArn = Core.Nothing,
      currentRole = Core.Nothing
    }

-- | The ID of the cluster to which the node belongs.
nodeGroupMember_cacheClusterId :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
nodeGroupMember_cacheClusterId = Lens.lens (\NodeGroupMember' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMember' {} a -> s {cacheClusterId = a} :: NodeGroupMember)

-- | The name of the Availability Zone in which the node is located.
nodeGroupMember_preferredAvailabilityZone :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
nodeGroupMember_preferredAvailabilityZone = Lens.lens (\NodeGroupMember' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@NodeGroupMember' {} a -> s {preferredAvailabilityZone = a} :: NodeGroupMember)

-- | The information required for client programs to connect to a node for
-- read operations. The read endpoint is only applicable on Redis (cluster
-- mode disabled) clusters.
nodeGroupMember_readEndpoint :: Lens.Lens' NodeGroupMember (Core.Maybe Endpoint)
nodeGroupMember_readEndpoint = Lens.lens (\NodeGroupMember' {readEndpoint} -> readEndpoint) (\s@NodeGroupMember' {} a -> s {readEndpoint = a} :: NodeGroupMember)

-- | The ID of the node within its cluster. A node ID is a numeric identifier
-- (0001, 0002, etc.).
nodeGroupMember_cacheNodeId :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
nodeGroupMember_cacheNodeId = Lens.lens (\NodeGroupMember' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMember' {} a -> s {cacheNodeId = a} :: NodeGroupMember)

-- | The outpost ARN of the node group member.
nodeGroupMember_preferredOutpostArn :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
nodeGroupMember_preferredOutpostArn = Lens.lens (\NodeGroupMember' {preferredOutpostArn} -> preferredOutpostArn) (\s@NodeGroupMember' {} a -> s {preferredOutpostArn = a} :: NodeGroupMember)

-- | The role that is currently assigned to the node - @primary@ or
-- @replica@. This member is only applicable for Redis (cluster mode
-- disabled) replication groups.
nodeGroupMember_currentRole :: Lens.Lens' NodeGroupMember (Core.Maybe Core.Text)
nodeGroupMember_currentRole = Lens.lens (\NodeGroupMember' {currentRole} -> currentRole) (\s@NodeGroupMember' {} a -> s {currentRole = a} :: NodeGroupMember)

instance Core.FromXML NodeGroupMember where
  parseXML x =
    NodeGroupMember'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "PreferredAvailabilityZone")
      Core.<*> (x Core..@? "ReadEndpoint")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "PreferredOutpostArn")
      Core.<*> (x Core..@? "CurrentRole")

instance Core.Hashable NodeGroupMember

instance Core.NFData NodeGroupMember

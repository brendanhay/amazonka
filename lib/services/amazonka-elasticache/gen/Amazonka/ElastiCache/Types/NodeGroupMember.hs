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
-- Module      : Amazonka.ElastiCache.Types.NodeGroupMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroupMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.Endpoint
import qualified Amazonka.Prelude as Prelude

-- | Represents a single node within a node group (shard).
--
-- /See:/ 'newNodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { -- | The name of the Availability Zone in which the node is located.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the cluster to which the node belongs.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The outpost ARN of the node group member.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the node within its cluster. A node ID is a numeric identifier
    -- (0001, 0002, etc.).
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The information required for client programs to connect to a node for
    -- read operations. The read endpoint is only applicable on Redis (cluster
    -- mode disabled) clusters.
    readEndpoint :: Prelude.Maybe Endpoint,
    -- | The role that is currently assigned to the node - @primary@ or
    -- @replica@. This member is only applicable for Redis (cluster mode
    -- disabled) replication groups.
    currentRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroupMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredAvailabilityZone', 'nodeGroupMember_preferredAvailabilityZone' - The name of the Availability Zone in which the node is located.
--
-- 'cacheClusterId', 'nodeGroupMember_cacheClusterId' - The ID of the cluster to which the node belongs.
--
-- 'preferredOutpostArn', 'nodeGroupMember_preferredOutpostArn' - The outpost ARN of the node group member.
--
-- 'cacheNodeId', 'nodeGroupMember_cacheNodeId' - The ID of the node within its cluster. A node ID is a numeric identifier
-- (0001, 0002, etc.).
--
-- 'readEndpoint', 'nodeGroupMember_readEndpoint' - The information required for client programs to connect to a node for
-- read operations. The read endpoint is only applicable on Redis (cluster
-- mode disabled) clusters.
--
-- 'currentRole', 'nodeGroupMember_currentRole' - The role that is currently assigned to the node - @primary@ or
-- @replica@. This member is only applicable for Redis (cluster mode
-- disabled) replication groups.
newNodeGroupMember ::
  NodeGroupMember
newNodeGroupMember =
  NodeGroupMember'
    { preferredAvailabilityZone =
        Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      readEndpoint = Prelude.Nothing,
      currentRole = Prelude.Nothing
    }

-- | The name of the Availability Zone in which the node is located.
nodeGroupMember_preferredAvailabilityZone :: Lens.Lens' NodeGroupMember (Prelude.Maybe Prelude.Text)
nodeGroupMember_preferredAvailabilityZone = Lens.lens (\NodeGroupMember' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@NodeGroupMember' {} a -> s {preferredAvailabilityZone = a} :: NodeGroupMember)

-- | The ID of the cluster to which the node belongs.
nodeGroupMember_cacheClusterId :: Lens.Lens' NodeGroupMember (Prelude.Maybe Prelude.Text)
nodeGroupMember_cacheClusterId = Lens.lens (\NodeGroupMember' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMember' {} a -> s {cacheClusterId = a} :: NodeGroupMember)

-- | The outpost ARN of the node group member.
nodeGroupMember_preferredOutpostArn :: Lens.Lens' NodeGroupMember (Prelude.Maybe Prelude.Text)
nodeGroupMember_preferredOutpostArn = Lens.lens (\NodeGroupMember' {preferredOutpostArn} -> preferredOutpostArn) (\s@NodeGroupMember' {} a -> s {preferredOutpostArn = a} :: NodeGroupMember)

-- | The ID of the node within its cluster. A node ID is a numeric identifier
-- (0001, 0002, etc.).
nodeGroupMember_cacheNodeId :: Lens.Lens' NodeGroupMember (Prelude.Maybe Prelude.Text)
nodeGroupMember_cacheNodeId = Lens.lens (\NodeGroupMember' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMember' {} a -> s {cacheNodeId = a} :: NodeGroupMember)

-- | The information required for client programs to connect to a node for
-- read operations. The read endpoint is only applicable on Redis (cluster
-- mode disabled) clusters.
nodeGroupMember_readEndpoint :: Lens.Lens' NodeGroupMember (Prelude.Maybe Endpoint)
nodeGroupMember_readEndpoint = Lens.lens (\NodeGroupMember' {readEndpoint} -> readEndpoint) (\s@NodeGroupMember' {} a -> s {readEndpoint = a} :: NodeGroupMember)

-- | The role that is currently assigned to the node - @primary@ or
-- @replica@. This member is only applicable for Redis (cluster mode
-- disabled) replication groups.
nodeGroupMember_currentRole :: Lens.Lens' NodeGroupMember (Prelude.Maybe Prelude.Text)
nodeGroupMember_currentRole = Lens.lens (\NodeGroupMember' {currentRole} -> currentRole) (\s@NodeGroupMember' {} a -> s {currentRole = a} :: NodeGroupMember)

instance Data.FromXML NodeGroupMember where
  parseXML x =
    NodeGroupMember'
      Prelude.<$> (x Data..@? "PreferredAvailabilityZone")
      Prelude.<*> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "PreferredOutpostArn")
      Prelude.<*> (x Data..@? "CacheNodeId")
      Prelude.<*> (x Data..@? "ReadEndpoint")
      Prelude.<*> (x Data..@? "CurrentRole")

instance Prelude.Hashable NodeGroupMember where
  hashWithSalt _salt NodeGroupMember' {..} =
    _salt
      `Prelude.hashWithSalt` preferredAvailabilityZone
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` preferredOutpostArn
      `Prelude.hashWithSalt` cacheNodeId
      `Prelude.hashWithSalt` readEndpoint
      `Prelude.hashWithSalt` currentRole

instance Prelude.NFData NodeGroupMember where
  rnf NodeGroupMember' {..} =
    Prelude.rnf preferredAvailabilityZone
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf preferredOutpostArn
      `Prelude.seq` Prelude.rnf cacheNodeId
      `Prelude.seq` Prelude.rnf readEndpoint
      `Prelude.seq` Prelude.rnf currentRole

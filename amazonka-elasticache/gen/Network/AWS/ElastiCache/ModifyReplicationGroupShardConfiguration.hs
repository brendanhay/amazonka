{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a replication group\'s shards (node groups) by allowing you to
-- add shards, remove shards, or rebalance the keyspaces among existing
-- shards.
module Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
  ( -- * Creating a Request
    ModifyReplicationGroupShardConfiguration (..),
    newModifyReplicationGroupShardConfiguration,

    -- * Request Lenses
    modifyReplicationGroupShardConfiguration_nodeGroupsToRetain,
    modifyReplicationGroupShardConfiguration_nodeGroupsToRemove,
    modifyReplicationGroupShardConfiguration_reshardingConfiguration,
    modifyReplicationGroupShardConfiguration_replicationGroupId,
    modifyReplicationGroupShardConfiguration_nodeGroupCount,
    modifyReplicationGroupShardConfiguration_applyImmediately,

    -- * Destructuring the Response
    ModifyReplicationGroupShardConfigurationResponse (..),
    newModifyReplicationGroupShardConfigurationResponse,

    -- * Response Lenses
    modifyReplicationGroupShardConfigurationResponse_replicationGroup,
    modifyReplicationGroupShardConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a @ModifyReplicationGroupShardConfiguration@
-- operation.
--
-- /See:/ 'newModifyReplicationGroupShardConfiguration' smart constructor.
data ModifyReplicationGroupShardConfiguration = ModifyReplicationGroupShardConfiguration'
  { -- | If the value of @NodeGroupCount@ is less than the current number of node
    -- groups (shards), then either @NodeGroupsToRemove@ or
    -- @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of
    -- @NodeGroupId@s to retain in the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups except
    -- those listed by @NodeGroupsToRetain@ from the cluster.
    nodeGroupsToRetain :: Prelude.Maybe [Prelude.Text],
    -- | If the value of @NodeGroupCount@ is less than the current number of node
    -- groups (shards), then either @NodeGroupsToRemove@ or
    -- @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of
    -- @NodeGroupId@s to remove from the cluster.
    --
    -- ElastiCache for Redis will attempt to remove all node groups listed by
    -- @NodeGroupsToRemove@ from the cluster.
    nodeGroupsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the preferred availability zones for each node group in the
    -- cluster. If the value of @NodeGroupCount@ is greater than the current
    -- number of node groups (shards), you can use this parameter to specify
    -- the preferred availability zones of the cluster\'s shards. If you omit
    -- this parameter ElastiCache selects availability zones for you.
    --
    -- You can specify this parameter only if the value of @NodeGroupCount@ is
    -- greater than the current number of node groups (shards).
    reshardingConfiguration :: Prelude.Maybe [ReshardingConfiguration],
    -- | The name of the Redis (cluster mode enabled) cluster (replication group)
    -- on which the shards are to be configured.
    replicationGroupId :: Prelude.Text,
    -- | The number of node groups (shards) that results from the modification of
    -- the shard configuration.
    nodeGroupCount :: Prelude.Int,
    -- | Indicates that the shard reconfiguration process begins immediately. At
    -- present, the only permitted value for this parameter is @true@.
    --
    -- Value: true
    applyImmediately :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationGroupShardConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeGroupsToRetain', 'modifyReplicationGroupShardConfiguration_nodeGroupsToRetain' - If the value of @NodeGroupCount@ is less than the current number of node
-- groups (shards), then either @NodeGroupsToRemove@ or
-- @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of
-- @NodeGroupId@s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except
-- those listed by @NodeGroupsToRetain@ from the cluster.
--
-- 'nodeGroupsToRemove', 'modifyReplicationGroupShardConfiguration_nodeGroupsToRemove' - If the value of @NodeGroupCount@ is less than the current number of node
-- groups (shards), then either @NodeGroupsToRemove@ or
-- @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of
-- @NodeGroupId@s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by
-- @NodeGroupsToRemove@ from the cluster.
--
-- 'reshardingConfiguration', 'modifyReplicationGroupShardConfiguration_reshardingConfiguration' - Specifies the preferred availability zones for each node group in the
-- cluster. If the value of @NodeGroupCount@ is greater than the current
-- number of node groups (shards), you can use this parameter to specify
-- the preferred availability zones of the cluster\'s shards. If you omit
-- this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is
-- greater than the current number of node groups (shards).
--
-- 'replicationGroupId', 'modifyReplicationGroupShardConfiguration_replicationGroupId' - The name of the Redis (cluster mode enabled) cluster (replication group)
-- on which the shards are to be configured.
--
-- 'nodeGroupCount', 'modifyReplicationGroupShardConfiguration_nodeGroupCount' - The number of node groups (shards) that results from the modification of
-- the shard configuration.
--
-- 'applyImmediately', 'modifyReplicationGroupShardConfiguration_applyImmediately' - Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is @true@.
--
-- Value: true
newModifyReplicationGroupShardConfiguration ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'nodeGroupCount'
  Prelude.Int ->
  -- | 'applyImmediately'
  Prelude.Bool ->
  ModifyReplicationGroupShardConfiguration
newModifyReplicationGroupShardConfiguration
  pReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    ModifyReplicationGroupShardConfiguration'
      { nodeGroupsToRetain =
          Prelude.Nothing,
        nodeGroupsToRemove =
          Prelude.Nothing,
        reshardingConfiguration =
          Prelude.Nothing,
        replicationGroupId =
          pReplicationGroupId_,
        nodeGroupCount = pNodeGroupCount_,
        applyImmediately =
          pApplyImmediately_
      }

-- | If the value of @NodeGroupCount@ is less than the current number of node
-- groups (shards), then either @NodeGroupsToRemove@ or
-- @NodeGroupsToRetain@ is required. @NodeGroupsToRetain@ is a list of
-- @NodeGroupId@s to retain in the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups except
-- those listed by @NodeGroupsToRetain@ from the cluster.
modifyReplicationGroupShardConfiguration_nodeGroupsToRetain :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Prelude.Maybe [Prelude.Text])
modifyReplicationGroupShardConfiguration_nodeGroupsToRetain = Lens.lens (\ModifyReplicationGroupShardConfiguration' {nodeGroupsToRetain} -> nodeGroupsToRetain) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {nodeGroupsToRetain = a} :: ModifyReplicationGroupShardConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | If the value of @NodeGroupCount@ is less than the current number of node
-- groups (shards), then either @NodeGroupsToRemove@ or
-- @NodeGroupsToRetain@ is required. @NodeGroupsToRemove@ is a list of
-- @NodeGroupId@s to remove from the cluster.
--
-- ElastiCache for Redis will attempt to remove all node groups listed by
-- @NodeGroupsToRemove@ from the cluster.
modifyReplicationGroupShardConfiguration_nodeGroupsToRemove :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Prelude.Maybe [Prelude.Text])
modifyReplicationGroupShardConfiguration_nodeGroupsToRemove = Lens.lens (\ModifyReplicationGroupShardConfiguration' {nodeGroupsToRemove} -> nodeGroupsToRemove) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {nodeGroupsToRemove = a} :: ModifyReplicationGroupShardConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the preferred availability zones for each node group in the
-- cluster. If the value of @NodeGroupCount@ is greater than the current
-- number of node groups (shards), you can use this parameter to specify
-- the preferred availability zones of the cluster\'s shards. If you omit
-- this parameter ElastiCache selects availability zones for you.
--
-- You can specify this parameter only if the value of @NodeGroupCount@ is
-- greater than the current number of node groups (shards).
modifyReplicationGroupShardConfiguration_reshardingConfiguration :: Lens.Lens' ModifyReplicationGroupShardConfiguration (Prelude.Maybe [ReshardingConfiguration])
modifyReplicationGroupShardConfiguration_reshardingConfiguration = Lens.lens (\ModifyReplicationGroupShardConfiguration' {reshardingConfiguration} -> reshardingConfiguration) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {reshardingConfiguration = a} :: ModifyReplicationGroupShardConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Redis (cluster mode enabled) cluster (replication group)
-- on which the shards are to be configured.
modifyReplicationGroupShardConfiguration_replicationGroupId :: Lens.Lens' ModifyReplicationGroupShardConfiguration Prelude.Text
modifyReplicationGroupShardConfiguration_replicationGroupId = Lens.lens (\ModifyReplicationGroupShardConfiguration' {replicationGroupId} -> replicationGroupId) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {replicationGroupId = a} :: ModifyReplicationGroupShardConfiguration)

-- | The number of node groups (shards) that results from the modification of
-- the shard configuration.
modifyReplicationGroupShardConfiguration_nodeGroupCount :: Lens.Lens' ModifyReplicationGroupShardConfiguration Prelude.Int
modifyReplicationGroupShardConfiguration_nodeGroupCount = Lens.lens (\ModifyReplicationGroupShardConfiguration' {nodeGroupCount} -> nodeGroupCount) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {nodeGroupCount = a} :: ModifyReplicationGroupShardConfiguration)

-- | Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is @true@.
--
-- Value: true
modifyReplicationGroupShardConfiguration_applyImmediately :: Lens.Lens' ModifyReplicationGroupShardConfiguration Prelude.Bool
modifyReplicationGroupShardConfiguration_applyImmediately = Lens.lens (\ModifyReplicationGroupShardConfiguration' {applyImmediately} -> applyImmediately) (\s@ModifyReplicationGroupShardConfiguration' {} a -> s {applyImmediately = a} :: ModifyReplicationGroupShardConfiguration)

instance
  Core.AWSRequest
    ModifyReplicationGroupShardConfiguration
  where
  type
    AWSResponse
      ModifyReplicationGroupShardConfiguration =
      ModifyReplicationGroupShardConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyReplicationGroupShardConfigurationResult"
      ( \s h x ->
          ModifyReplicationGroupShardConfigurationResponse'
            Prelude.<$> (x Core..@? "ReplicationGroup")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyReplicationGroupShardConfiguration

instance
  Prelude.NFData
    ModifyReplicationGroupShardConfiguration

instance
  Core.ToHeaders
    ModifyReplicationGroupShardConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ModifyReplicationGroupShardConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyReplicationGroupShardConfiguration
  where
  toQuery ModifyReplicationGroupShardConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyReplicationGroupShardConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "NodeGroupsToRetain"
          Core.=: Core.toQuery
            ( Core.toQueryList "NodeGroupToRetain"
                Prelude.<$> nodeGroupsToRetain
            ),
        "NodeGroupsToRemove"
          Core.=: Core.toQuery
            ( Core.toQueryList "NodeGroupToRemove"
                Prelude.<$> nodeGroupsToRemove
            ),
        "ReshardingConfiguration"
          Core.=: Core.toQuery
            ( Core.toQueryList "ReshardingConfiguration"
                Prelude.<$> reshardingConfiguration
            ),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "NodeGroupCount" Core.=: nodeGroupCount,
        "ApplyImmediately" Core.=: applyImmediately
      ]

-- | /See:/ 'newModifyReplicationGroupShardConfigurationResponse' smart constructor.
data ModifyReplicationGroupShardConfigurationResponse = ModifyReplicationGroupShardConfigurationResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationGroupShardConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'modifyReplicationGroupShardConfigurationResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyReplicationGroupShardConfigurationResponse_httpStatus' - The response's http status code.
newModifyReplicationGroupShardConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyReplicationGroupShardConfigurationResponse
newModifyReplicationGroupShardConfigurationResponse
  pHttpStatus_ =
    ModifyReplicationGroupShardConfigurationResponse'
      { replicationGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
modifyReplicationGroupShardConfigurationResponse_replicationGroup :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse (Prelude.Maybe ReplicationGroup)
modifyReplicationGroupShardConfigurationResponse_replicationGroup = Lens.lens (\ModifyReplicationGroupShardConfigurationResponse' {replicationGroup} -> replicationGroup) (\s@ModifyReplicationGroupShardConfigurationResponse' {} a -> s {replicationGroup = a} :: ModifyReplicationGroupShardConfigurationResponse)

-- | The response's http status code.
modifyReplicationGroupShardConfigurationResponse_httpStatus :: Lens.Lens' ModifyReplicationGroupShardConfigurationResponse Prelude.Int
modifyReplicationGroupShardConfigurationResponse_httpStatus = Lens.lens (\ModifyReplicationGroupShardConfigurationResponse' {httpStatus} -> httpStatus) (\s@ModifyReplicationGroupShardConfigurationResponse' {} a -> s {httpStatus = a} :: ModifyReplicationGroupShardConfigurationResponse)

instance
  Prelude.NFData
    ModifyReplicationGroupShardConfigurationResponse

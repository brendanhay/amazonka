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
-- Module      : Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the number of node groups in a Global Datastore
module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a Request
    DecreaseNodeGroupsInGlobalReplicationGroup (..),
    newDecreaseNodeGroupsInGlobalReplicationGroup,

    -- * Request Lenses
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove,
    decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain,
    decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId,
    decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount,
    decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately,

    -- * Destructuring the Response
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    newDecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup,
    decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDecreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroup = DecreaseNodeGroupsInGlobalReplicationGroup'
  { -- | If the value of NodeGroupCount is less than the current number of node
    -- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
    -- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
    -- the cluster. ElastiCache for Redis will attempt to remove all node
    -- groups listed by NodeGroupsToRemove from the cluster.
    globalNodeGroupsToRemove :: Core.Maybe [Core.Text],
    -- | If the value of NodeGroupCount is less than the current number of node
    -- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
    -- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
    -- the cluster. ElastiCache for Redis will attempt to remove all node
    -- groups listed by NodeGroupsToRemove from the cluster.
    globalNodeGroupsToRetain :: Core.Maybe [Core.Text],
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Text,
    -- | The number of node groups (shards) that results from the modification of
    -- the shard configuration
    nodeGroupCount :: Core.Int,
    -- | Indicates that the shard reconfiguration process begins immediately. At
    -- present, the only permitted value for this parameter is true.
    applyImmediately :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseNodeGroupsInGlobalReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNodeGroupsToRemove', 'decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove' - If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
-- the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by NodeGroupsToRemove from the cluster.
--
-- 'globalNodeGroupsToRetain', 'decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain' - If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
-- the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by NodeGroupsToRemove from the cluster.
--
-- 'globalReplicationGroupId', 'decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'nodeGroupCount', 'decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount' - The number of node groups (shards) that results from the modification of
-- the shard configuration
--
-- 'applyImmediately', 'decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately' - Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is true.
newDecreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Core.Text ->
  -- | 'nodeGroupCount'
  Core.Int ->
  -- | 'applyImmediately'
  Core.Bool ->
  DecreaseNodeGroupsInGlobalReplicationGroup
newDecreaseNodeGroupsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    DecreaseNodeGroupsInGlobalReplicationGroup'
      { globalNodeGroupsToRemove =
          Core.Nothing,
        globalNodeGroupsToRetain =
          Core.Nothing,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        nodeGroupCount =
          pNodeGroupCount_,
        applyImmediately =
          pApplyImmediately_
      }

-- | If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
-- the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by NodeGroupsToRemove from the cluster.
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Core.Maybe [Core.Text])
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRemove = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalNodeGroupsToRemove} -> globalNodeGroupsToRemove) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalNodeGroupsToRemove = a} :: DecreaseNodeGroupsInGlobalReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | If the value of NodeGroupCount is less than the current number of node
-- groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is
-- required. NodeGroupsToRemove is a list of NodeGroupIds to remove from
-- the cluster. ElastiCache for Redis will attempt to remove all node
-- groups listed by NodeGroupsToRemove from the cluster.
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Core.Maybe [Core.Text])
decreaseNodeGroupsInGlobalReplicationGroup_globalNodeGroupsToRetain = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalNodeGroupsToRetain} -> globalNodeGroupsToRetain) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalNodeGroupsToRetain = a} :: DecreaseNodeGroupsInGlobalReplicationGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the Global Datastore
decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Text
decreaseNodeGroupsInGlobalReplicationGroup_globalReplicationGroupId = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

-- | The number of node groups (shards) that results from the modification of
-- the shard configuration
decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Int
decreaseNodeGroupsInGlobalReplicationGroup_nodeGroupCount = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {nodeGroupCount} -> nodeGroupCount) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {nodeGroupCount = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

-- | Indicates that the shard reconfiguration process begins immediately. At
-- present, the only permitted value for this parameter is true.
decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Core.Bool
decreaseNodeGroupsInGlobalReplicationGroup_applyImmediately = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroup' {applyImmediately} -> applyImmediately) (\s@DecreaseNodeGroupsInGlobalReplicationGroup' {} a -> s {applyImmediately = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)

instance
  Core.AWSRequest
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  type
    AWSResponse
      DecreaseNodeGroupsInGlobalReplicationGroup =
      DecreaseNodeGroupsInGlobalReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DecreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          DecreaseNodeGroupsInGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DecreaseNodeGroupsInGlobalReplicationGroup

instance
  Core.NFData
    DecreaseNodeGroupsInGlobalReplicationGroup

instance
  Core.ToHeaders
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DecreaseNodeGroupsInGlobalReplicationGroup
  where
  toQuery
    DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DecreaseNodeGroupsInGlobalReplicationGroup" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2015-02-02" :: Core.ByteString),
          "GlobalNodeGroupsToRemove"
            Core.=: Core.toQuery
              ( Core.toQueryList "GlobalNodeGroupId"
                  Core.<$> globalNodeGroupsToRemove
              ),
          "GlobalNodeGroupsToRetain"
            Core.=: Core.toQuery
              ( Core.toQueryList "GlobalNodeGroupId"
                  Core.<$> globalNodeGroupsToRetain
              ),
          "GlobalReplicationGroupId"
            Core.=: globalReplicationGroupId,
          "NodeGroupCount" Core.=: nodeGroupCount,
          "ApplyImmediately" Core.=: applyImmediately
        ]

-- | /See:/ 'newDecreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroupResponse = DecreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe GlobalReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseNodeGroupsInGlobalReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroup', 'decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup' - Undocumented member.
--
-- 'httpStatus', 'decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus' - The response's http status code.
newDecreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DecreaseNodeGroupsInGlobalReplicationGroupResponse
newDecreaseNodeGroupsInGlobalReplicationGroupResponse
  pHttpStatus_ =
    DecreaseNodeGroupsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Undocumented member.
decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse (Core.Maybe GlobalReplicationGroup)
decreaseNodeGroupsInGlobalReplicationGroupResponse_globalReplicationGroup = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroupResponse' {globalReplicationGroup} -> globalReplicationGroup) (\s@DecreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {globalReplicationGroup = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)

-- | The response's http status code.
decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse Core.Int
decreaseNodeGroupsInGlobalReplicationGroupResponse_httpStatus = Lens.lens (\DecreaseNodeGroupsInGlobalReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@DecreaseNodeGroupsInGlobalReplicationGroupResponse' {} a -> s {httpStatus = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)

instance
  Core.NFData
    DecreaseNodeGroupsInGlobalReplicationGroupResponse

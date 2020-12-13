{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the number of node groups in a Global Datastore
module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a request
    DecreaseNodeGroupsInGlobalReplicationGroup (..),
    mkDecreaseNodeGroupsInGlobalReplicationGroup,

    -- ** Request lenses
    dngigrgGlobalNodeGroupsToRemove,
    dngigrgNodeGroupCount,
    dngigrgGlobalReplicationGroupId,
    dngigrgApplyImmediately,
    dngigrgGlobalNodeGroupsToRetain,

    -- * Destructuring the response
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    mkDecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** Response lenses
    dngigrgrsGlobalReplicationGroup,
    dngigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDecreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroup = DecreaseNodeGroupsInGlobalReplicationGroup'
  { -- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
    globalNodeGroupsToRemove :: Lude.Maybe [Lude.Text],
    -- | The number of node groups (shards) that results from the modification of the shard configuration
    nodeGroupCount :: Lude.Int,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text,
    -- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true.
    applyImmediately :: Lude.Bool,
    -- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
    globalNodeGroupsToRetain :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseNodeGroupsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'globalNodeGroupsToRemove' - If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
-- * 'nodeGroupCount' - The number of node groups (shards) that results from the modification of the shard configuration
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'applyImmediately' - Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true.
-- * 'globalNodeGroupsToRetain' - If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
mkDecreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'nodeGroupCount'
  Lude.Int ->
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  DecreaseNodeGroupsInGlobalReplicationGroup
mkDecreaseNodeGroupsInGlobalReplicationGroup
  pNodeGroupCount_
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    DecreaseNodeGroupsInGlobalReplicationGroup'
      { globalNodeGroupsToRemove =
          Lude.Nothing,
        nodeGroupCount = pNodeGroupCount_,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_,
        globalNodeGroupsToRetain = Lude.Nothing
      }

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
--
-- /Note:/ Consider using 'globalNodeGroupsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalNodeGroupsToRemove :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Lude.Maybe [Lude.Text])
dngigrgGlobalNodeGroupsToRemove = Lens.lens (globalNodeGroupsToRemove :: DecreaseNodeGroupsInGlobalReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {globalNodeGroupsToRemove = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED dngigrgGlobalNodeGroupsToRemove "Use generic-lens or generic-optics with 'globalNodeGroupsToRemove' instead." #-}

-- | The number of node groups (shards) that results from the modification of the shard configuration
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgNodeGroupCount :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Lude.Int
dngigrgNodeGroupCount = Lens.lens (nodeGroupCount :: DecreaseNodeGroupsInGlobalReplicationGroup -> Lude.Int) (\s a -> s {nodeGroupCount = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED dngigrgNodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalReplicationGroupId :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Lude.Text
dngigrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: DecreaseNodeGroupsInGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED dngigrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgApplyImmediately :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup Lude.Bool
dngigrgApplyImmediately = Lens.lens (applyImmediately :: DecreaseNodeGroupsInGlobalReplicationGroup -> Lude.Bool) (\s a -> s {applyImmediately = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED dngigrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
--
-- /Note:/ Consider using 'globalNodeGroupsToRetain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgGlobalNodeGroupsToRetain :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroup (Lude.Maybe [Lude.Text])
dngigrgGlobalNodeGroupsToRetain = Lens.lens (globalNodeGroupsToRetain :: DecreaseNodeGroupsInGlobalReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {globalNodeGroupsToRetain = a} :: DecreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED dngigrgGlobalNodeGroupsToRetain "Use generic-lens or generic-optics with 'globalNodeGroupsToRetain' instead." #-}

instance Lude.AWSRequest DecreaseNodeGroupsInGlobalReplicationGroup where
  type
    Rs DecreaseNodeGroupsInGlobalReplicationGroup =
      DecreaseNodeGroupsInGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DecreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          DecreaseNodeGroupsInGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DecreaseNodeGroupsInGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DecreaseNodeGroupsInGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DecreaseNodeGroupsInGlobalReplicationGroup where
  toQuery DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DecreaseNodeGroupsInGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "GlobalNodeGroupsToRemove"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "GlobalNodeGroupId"
                Lude.<$> globalNodeGroupsToRemove
            ),
        "NodeGroupCount" Lude.=: nodeGroupCount,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately,
        "GlobalNodeGroupsToRetain"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "GlobalNodeGroupId"
                Lude.<$> globalNodeGroupsToRetain
            )
      ]

-- | /See:/ 'mkDecreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroupResponse = DecreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseNodeGroupsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkDecreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DecreaseNodeGroupsInGlobalReplicationGroupResponse
mkDecreaseNodeGroupsInGlobalReplicationGroupResponse
  pResponseStatus_ =
    DecreaseNodeGroupsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgrsGlobalReplicationGroup :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
dngigrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)
{-# DEPRECATED dngigrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngigrgrsResponseStatus :: Lens.Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse Lude.Int
dngigrgrsResponseStatus = Lens.lens (responseStatus :: DecreaseNodeGroupsInGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DecreaseNodeGroupsInGlobalReplicationGroupResponse)
{-# DEPRECATED dngigrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

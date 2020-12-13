{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increase the number of node groups in the Global Datastore
module Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a request
    IncreaseNodeGroupsInGlobalReplicationGroup (..),
    mkIncreaseNodeGroupsInGlobalReplicationGroup,

    -- ** Request lenses
    ingigrgNodeGroupCount,
    ingigrgGlobalReplicationGroupId,
    ingigrgApplyImmediately,
    ingigrgRegionalConfigurations,

    -- * Destructuring the response
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    mkIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** Response lenses
    ingigrgrsGlobalReplicationGroup,
    ingigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkIncreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroup = IncreaseNodeGroupsInGlobalReplicationGroup'
  { -- | The number of node groups you wish to add
    nodeGroupCount :: Lude.Int,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text,
    -- | Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
    applyImmediately :: Lude.Bool,
    -- | Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
    regionalConfigurations :: Lude.Maybe [RegionalConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IncreaseNodeGroupsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'nodeGroupCount' - The number of node groups you wish to add
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'applyImmediately' - Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
-- * 'regionalConfigurations' - Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
mkIncreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'nodeGroupCount'
  Lude.Int ->
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  IncreaseNodeGroupsInGlobalReplicationGroup
mkIncreaseNodeGroupsInGlobalReplicationGroup
  pNodeGroupCount_
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    IncreaseNodeGroupsInGlobalReplicationGroup'
      { nodeGroupCount =
          pNodeGroupCount_,
        globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_,
        regionalConfigurations = Lude.Nothing
      }

-- | The number of node groups you wish to add
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgNodeGroupCount :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Lude.Int
ingigrgNodeGroupCount = Lens.lens (nodeGroupCount :: IncreaseNodeGroupsInGlobalReplicationGroup -> Lude.Int) (\s a -> s {nodeGroupCount = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED ingigrgNodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgGlobalReplicationGroupId :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Lude.Text
ingigrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: IncreaseNodeGroupsInGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED ingigrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgApplyImmediately :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Lude.Bool
ingigrgApplyImmediately = Lens.lens (applyImmediately :: IncreaseNodeGroupsInGlobalReplicationGroup -> Lude.Bool) (\s a -> s {applyImmediately = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED ingigrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
--
-- /Note:/ Consider using 'regionalConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgRegionalConfigurations :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup (Lude.Maybe [RegionalConfiguration])
ingigrgRegionalConfigurations = Lens.lens (regionalConfigurations :: IncreaseNodeGroupsInGlobalReplicationGroup -> Lude.Maybe [RegionalConfiguration]) (\s a -> s {regionalConfigurations = a} :: IncreaseNodeGroupsInGlobalReplicationGroup)
{-# DEPRECATED ingigrgRegionalConfigurations "Use generic-lens or generic-optics with 'regionalConfigurations' instead." #-}

instance Lude.AWSRequest IncreaseNodeGroupsInGlobalReplicationGroup where
  type
    Rs IncreaseNodeGroupsInGlobalReplicationGroup =
      IncreaseNodeGroupsInGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "IncreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          IncreaseNodeGroupsInGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IncreaseNodeGroupsInGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath IncreaseNodeGroupsInGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery IncreaseNodeGroupsInGlobalReplicationGroup where
  toQuery IncreaseNodeGroupsInGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("IncreaseNodeGroupsInGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "NodeGroupCount" Lude.=: nodeGroupCount,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately,
        "RegionalConfigurations"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "RegionalConfiguration"
                Lude.<$> regionalConfigurations
            )
      ]

-- | /See:/ 'mkIncreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroupResponse = IncreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IncreaseNodeGroupsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkIncreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IncreaseNodeGroupsInGlobalReplicationGroupResponse
mkIncreaseNodeGroupsInGlobalReplicationGroupResponse
  pResponseStatus_ =
    IncreaseNodeGroupsInGlobalReplicationGroupResponse'
      { globalReplicationGroup =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgrsGlobalReplicationGroup :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
ingigrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: IncreaseNodeGroupsInGlobalReplicationGroupResponse)
{-# DEPRECATED ingigrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgrsResponseStatus :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse Lude.Int
ingigrgrsResponseStatus = Lens.lens (responseStatus :: IncreaseNodeGroupsInGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IncreaseNodeGroupsInGlobalReplicationGroupResponse)
{-# DEPRECATED ingigrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

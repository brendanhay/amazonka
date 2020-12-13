{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deleting a Global Datastore is a two-step process:
--
--
--     * First, you must 'DisassociateGlobalReplicationGroup' to remove the secondary clusters in the Global Datastore.
--
--
--     * Once the Global Datastore contains only the primary cluster, you can use DeleteGlobalReplicationGroup API to delete the Global Datastore while retainining the primary cluster using Retain…= true.
--
--
-- Since the Global Datastore has only a primary cluster, you can delete the Global Datastore while retaining the primary by setting @RetainPrimaryCluster=true@ .
-- When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
  ( -- * Creating a request
    DeleteGlobalReplicationGroup (..),
    mkDeleteGlobalReplicationGroup,

    -- ** Request lenses
    dRetainPrimaryReplicationGroup,
    dGlobalReplicationGroupId,

    -- * Destructuring the response
    DeleteGlobalReplicationGroupResponse (..),
    mkDeleteGlobalReplicationGroupResponse,

    -- ** Response lenses
    dgrgfrsGlobalReplicationGroup,
    dgrgfrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { -- | The primary replication group is retained as a standalone replication group.
    retainPrimaryReplicationGroup :: Lude.Bool,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'retainPrimaryReplicationGroup' - The primary replication group is retained as a standalone replication group.
-- * 'globalReplicationGroupId' - The name of the Global Datastore
mkDeleteGlobalReplicationGroup ::
  -- | 'retainPrimaryReplicationGroup'
  Lude.Bool ->
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  DeleteGlobalReplicationGroup
mkDeleteGlobalReplicationGroup
  pRetainPrimaryReplicationGroup_
  pGlobalReplicationGroupId_ =
    DeleteGlobalReplicationGroup'
      { retainPrimaryReplicationGroup =
          pRetainPrimaryReplicationGroup_,
        globalReplicationGroupId = pGlobalReplicationGroupId_
      }

-- | The primary replication group is retained as a standalone replication group.
--
-- /Note:/ Consider using 'retainPrimaryReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetainPrimaryReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroup Lude.Bool
dRetainPrimaryReplicationGroup = Lens.lens (retainPrimaryReplicationGroup :: DeleteGlobalReplicationGroup -> Lude.Bool) (\s a -> s {retainPrimaryReplicationGroup = a} :: DeleteGlobalReplicationGroup)
{-# DEPRECATED dRetainPrimaryReplicationGroup "Use generic-lens or generic-optics with 'retainPrimaryReplicationGroup' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGlobalReplicationGroupId :: Lens.Lens' DeleteGlobalReplicationGroup Lude.Text
dGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: DeleteGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: DeleteGlobalReplicationGroup)
{-# DEPRECATED dGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

instance Lude.AWSRequest DeleteGlobalReplicationGroup where
  type
    Rs DeleteGlobalReplicationGroup =
      DeleteGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteGlobalReplicationGroupResult"
      ( \s h x ->
          DeleteGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGlobalReplicationGroup where
  toQuery DeleteGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "RetainPrimaryReplicationGroup"
          Lude.=: retainPrimaryReplicationGroup,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId
      ]

-- | /See:/ 'mkDeleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkDeleteGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGlobalReplicationGroupResponse
mkDeleteGlobalReplicationGroupResponse pResponseStatus_ =
  DeleteGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgfrsGlobalReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
dgrgfrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: DeleteGlobalReplicationGroupResponse)
{-# DEPRECATED dgrgfrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgfrsResponseStatus :: Lens.Lens' DeleteGlobalReplicationGroupResponse Lude.Int
dgrgfrsResponseStatus = Lens.lens (responseStatus :: DeleteGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGlobalReplicationGroupResponse)
{-# DEPRECATED dgrgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

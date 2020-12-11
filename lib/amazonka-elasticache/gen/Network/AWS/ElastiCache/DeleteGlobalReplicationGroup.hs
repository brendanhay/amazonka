{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dGlobalReplicationGroupId,
    dRetainPrimaryReplicationGroup,

    -- * Destructuring the response
    DeleteGlobalReplicationGroupResponse (..),
    mkDeleteGlobalReplicationGroupResponse,

    -- ** Response lenses
    dgrggrsGlobalReplicationGroup,
    dgrggrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { globalReplicationGroupId ::
      Lude.Text,
    retainPrimaryReplicationGroup ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'retainPrimaryReplicationGroup' - The primary replication group is retained as a standalone replication group.
mkDeleteGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'retainPrimaryReplicationGroup'
  Lude.Bool ->
  DeleteGlobalReplicationGroup
mkDeleteGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pRetainPrimaryReplicationGroup_ =
    DeleteGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        retainPrimaryReplicationGroup = pRetainPrimaryReplicationGroup_
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGlobalReplicationGroupId :: Lens.Lens' DeleteGlobalReplicationGroup Lude.Text
dGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: DeleteGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: DeleteGlobalReplicationGroup)
{-# DEPRECATED dGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The primary replication group is retained as a standalone replication group.
--
-- /Note:/ Consider using 'retainPrimaryReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetainPrimaryReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroup Lude.Bool
dRetainPrimaryReplicationGroup = Lens.lens (retainPrimaryReplicationGroup :: DeleteGlobalReplicationGroup -> Lude.Bool) (\s a -> s {retainPrimaryReplicationGroup = a} :: DeleteGlobalReplicationGroup)
{-# DEPRECATED dRetainPrimaryReplicationGroup "Use generic-lens or generic-optics with 'retainPrimaryReplicationGroup' instead." #-}

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
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "RetainPrimaryReplicationGroup"
          Lude.=: retainPrimaryReplicationGroup
      ]

-- | /See:/ 'mkDeleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { globalReplicationGroup ::
      Lude.Maybe
        GlobalReplicationGroup,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' - Undocumented field.
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
dgrggrsGlobalReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
dgrggrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: DeleteGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: DeleteGlobalReplicationGroupResponse)
{-# DEPRECATED dgrggrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrggrsResponseStatus :: Lens.Lens' DeleteGlobalReplicationGroupResponse Lude.Int
dgrggrsResponseStatus = Lens.lens (responseStatus :: DeleteGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGlobalReplicationGroupResponse)
{-# DEPRECATED dgrggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

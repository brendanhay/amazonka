{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a secondary cluster from the Global Datastore using the Global Datastore name. The secondary cluster will no longer receive updates from the primary cluster, but will remain as a standalone cluster in that AWS region.
module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
  ( -- * Creating a request
    DisassociateGlobalReplicationGroup (..),
    mkDisassociateGlobalReplicationGroup,

    -- ** Request lenses
    dgrgfReplicationGroupRegion,
    dgrgfGlobalReplicationGroupId,
    dgrgfReplicationGroupId,

    -- * Destructuring the response
    DisassociateGlobalReplicationGroupResponse (..),
    mkDisassociateGlobalReplicationGroupResponse,

    -- ** Response lenses
    dgrgrsGlobalReplicationGroup,
    dgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { -- | The AWS region of secondary cluster you wish to remove from the Global Datastore
    replicationGroupRegion :: Lude.Text,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Lude.Text,
    -- | The name of the secondary cluster you wish to remove from the Global Datastore
    replicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'replicationGroupRegion' - The AWS region of secondary cluster you wish to remove from the Global Datastore
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'replicationGroupId' - The name of the secondary cluster you wish to remove from the Global Datastore
mkDisassociateGlobalReplicationGroup ::
  -- | 'replicationGroupRegion'
  Lude.Text ->
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'replicationGroupId'
  Lude.Text ->
  DisassociateGlobalReplicationGroup
mkDisassociateGlobalReplicationGroup
  pReplicationGroupRegion_
  pGlobalReplicationGroupId_
  pReplicationGroupId_ =
    DisassociateGlobalReplicationGroup'
      { replicationGroupRegion =
          pReplicationGroupRegion_,
        globalReplicationGroupId = pGlobalReplicationGroupId_,
        replicationGroupId = pReplicationGroupId_
      }

-- | The AWS region of secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgfReplicationGroupRegion :: Lens.Lens' DisassociateGlobalReplicationGroup Lude.Text
dgrgfReplicationGroupRegion = Lens.lens (replicationGroupRegion :: DisassociateGlobalReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupRegion = a} :: DisassociateGlobalReplicationGroup)
{-# DEPRECATED dgrgfReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgfGlobalReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Lude.Text
dgrgfGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: DisassociateGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: DisassociateGlobalReplicationGroup)
{-# DEPRECATED dgrgfGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The name of the secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgfReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Lude.Text
dgrgfReplicationGroupId = Lens.lens (replicationGroupId :: DisassociateGlobalReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupId = a} :: DisassociateGlobalReplicationGroup)
{-# DEPRECATED dgrgfReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest DisassociateGlobalReplicationGroup where
  type
    Rs DisassociateGlobalReplicationGroup =
      DisassociateGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DisassociateGlobalReplicationGroupResult"
      ( \s h x ->
          DisassociateGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateGlobalReplicationGroup where
  toQuery DisassociateGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ReplicationGroupRegion" Lude.=: replicationGroupRegion,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkDisassociateGlobalReplicationGroupResponse' smart constructor.
data DisassociateGlobalReplicationGroupResponse = DisassociateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Lude.Maybe GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' -
-- * 'responseStatus' - The response status code.
mkDisassociateGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateGlobalReplicationGroupResponse
mkDisassociateGlobalReplicationGroupResponse pResponseStatus_ =
  DisassociateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrsGlobalReplicationGroup :: Lens.Lens' DisassociateGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
dgrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: DisassociateGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: DisassociateGlobalReplicationGroupResponse)
{-# DEPRECATED dgrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrsResponseStatus :: Lens.Lens' DisassociateGlobalReplicationGroupResponse Lude.Int
dgrgrsResponseStatus = Lens.lens (responseStatus :: DisassociateGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateGlobalReplicationGroupResponse)
{-# DEPRECATED dgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

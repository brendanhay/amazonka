{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redistribute slots to ensure uniform distribution across existing shards in the cluster.
module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
  ( -- * Creating a request
    RebalanceSlotsInGlobalReplicationGroup (..),
    mkRebalanceSlotsInGlobalReplicationGroup,

    -- ** Request lenses
    rsigrgGlobalReplicationGroupId,
    rsigrgApplyImmediately,

    -- * Destructuring the response
    RebalanceSlotsInGlobalReplicationGroupResponse (..),
    mkRebalanceSlotsInGlobalReplicationGroupResponse,

    -- ** Response lenses
    rsigrgrsGlobalReplicationGroup,
    rsigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebalanceSlotsInGlobalReplicationGroup' smart constructor.
data RebalanceSlotsInGlobalReplicationGroup = RebalanceSlotsInGlobalReplicationGroup'
  { globalReplicationGroupId ::
      Lude.Text,
    applyImmediately ::
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

-- | Creates a value of 'RebalanceSlotsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - If @True@ , redistribution is applied immediately.
-- * 'globalReplicationGroupId' - The name of the Global Datastore
mkRebalanceSlotsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  RebalanceSlotsInGlobalReplicationGroup
mkRebalanceSlotsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    RebalanceSlotsInGlobalReplicationGroup'
      { globalReplicationGroupId =
          pGlobalReplicationGroupId_,
        applyImmediately = pApplyImmediately_
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgGlobalReplicationGroupId :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Lude.Text
rsigrgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: RebalanceSlotsInGlobalReplicationGroup -> Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: RebalanceSlotsInGlobalReplicationGroup)
{-# DEPRECATED rsigrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | If @True@ , redistribution is applied immediately.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgApplyImmediately :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroup Lude.Bool
rsigrgApplyImmediately = Lens.lens (applyImmediately :: RebalanceSlotsInGlobalReplicationGroup -> Lude.Bool) (\s a -> s {applyImmediately = a} :: RebalanceSlotsInGlobalReplicationGroup)
{-# DEPRECATED rsigrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Lude.AWSRequest RebalanceSlotsInGlobalReplicationGroup where
  type
    Rs RebalanceSlotsInGlobalReplicationGroup =
      RebalanceSlotsInGlobalReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "RebalanceSlotsInGlobalReplicationGroupResult"
      ( \s h x ->
          RebalanceSlotsInGlobalReplicationGroupResponse'
            Lude.<$> (x Lude..@? "GlobalReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebalanceSlotsInGlobalReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RebalanceSlotsInGlobalReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery RebalanceSlotsInGlobalReplicationGroup where
  toQuery RebalanceSlotsInGlobalReplicationGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RebalanceSlotsInGlobalReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately
      ]

-- | /See:/ 'mkRebalanceSlotsInGlobalReplicationGroupResponse' smart constructor.
data RebalanceSlotsInGlobalReplicationGroupResponse = RebalanceSlotsInGlobalReplicationGroupResponse'
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RebalanceSlotsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'globalReplicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRebalanceSlotsInGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebalanceSlotsInGlobalReplicationGroupResponse
mkRebalanceSlotsInGlobalReplicationGroupResponse pResponseStatus_ =
  RebalanceSlotsInGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgrsGlobalReplicationGroup :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse (Lude.Maybe GlobalReplicationGroup)
rsigrgrsGlobalReplicationGroup = Lens.lens (globalReplicationGroup :: RebalanceSlotsInGlobalReplicationGroupResponse -> Lude.Maybe GlobalReplicationGroup) (\s a -> s {globalReplicationGroup = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)
{-# DEPRECATED rsigrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsigrgrsResponseStatus :: Lens.Lens' RebalanceSlotsInGlobalReplicationGroupResponse Lude.Int
rsigrgrsResponseStatus = Lens.lens (responseStatus :: RebalanceSlotsInGlobalReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebalanceSlotsInGlobalReplicationGroupResponse)
{-# DEPRECATED rsigrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

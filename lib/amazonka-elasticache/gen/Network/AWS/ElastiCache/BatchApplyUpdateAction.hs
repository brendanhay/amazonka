{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.BatchApplyUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Apply the service update. For more information on service updates and applying them, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/applying-updates.html Applying Service Updates> .
module Network.AWS.ElastiCache.BatchApplyUpdateAction
  ( -- * Creating a request
    BatchApplyUpdateAction (..),
    mkBatchApplyUpdateAction,

    -- ** Request lenses
    bauaCacheClusterIds,
    bauaReplicationGroupIds,
    bauaServiceUpdateName,

    -- * Destructuring the response
    UpdateActionResultsMessage (..),
    mkUpdateActionResultsMessage,

    -- ** Response lenses
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { cacheClusterIds ::
      Lude.Maybe [Lude.Text],
    replicationGroupIds :: Lude.Maybe [Lude.Text],
    serviceUpdateName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchApplyUpdateAction' with the minimum fields required to make a request.
--
-- * 'cacheClusterIds' - The cache cluster IDs
-- * 'replicationGroupIds' - The replication group IDs
-- * 'serviceUpdateName' - The unique ID of the service update
mkBatchApplyUpdateAction ::
  -- | 'serviceUpdateName'
  Lude.Text ->
  BatchApplyUpdateAction
mkBatchApplyUpdateAction pServiceUpdateName_ =
  BatchApplyUpdateAction'
    { cacheClusterIds = Lude.Nothing,
      replicationGroupIds = Lude.Nothing,
      serviceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaCacheClusterIds :: Lens.Lens' BatchApplyUpdateAction (Lude.Maybe [Lude.Text])
bauaCacheClusterIds = Lens.lens (cacheClusterIds :: BatchApplyUpdateAction -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheClusterIds = a} :: BatchApplyUpdateAction)
{-# DEPRECATED bauaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaReplicationGroupIds :: Lens.Lens' BatchApplyUpdateAction (Lude.Maybe [Lude.Text])
bauaReplicationGroupIds = Lens.lens (replicationGroupIds :: BatchApplyUpdateAction -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationGroupIds = a} :: BatchApplyUpdateAction)
{-# DEPRECATED bauaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaServiceUpdateName :: Lens.Lens' BatchApplyUpdateAction Lude.Text
bauaServiceUpdateName = Lens.lens (serviceUpdateName :: BatchApplyUpdateAction -> Lude.Text) (\s a -> s {serviceUpdateName = a} :: BatchApplyUpdateAction)
{-# DEPRECATED bauaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

instance Lude.AWSRequest BatchApplyUpdateAction where
  type Rs BatchApplyUpdateAction = UpdateActionResultsMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "BatchApplyUpdateActionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders BatchApplyUpdateAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchApplyUpdateAction where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchApplyUpdateAction where
  toQuery BatchApplyUpdateAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BatchApplyUpdateAction" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> cacheClusterIds),
        "ReplicationGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> replicationGroupIds),
        "ServiceUpdateName" Lude.=: serviceUpdateName
      ]

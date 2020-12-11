{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.BatchStopUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop the service update. For more information on service updates and stopping them, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/stopping-self-service-updates.html Stopping Service Updates> .
module Network.AWS.ElastiCache.BatchStopUpdateAction
  ( -- * Creating a request
    BatchStopUpdateAction (..),
    mkBatchStopUpdateAction,

    -- ** Request lenses
    bsuaCacheClusterIds,
    bsuaReplicationGroupIds,
    bsuaServiceUpdateName,

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

-- | /See:/ 'mkBatchStopUpdateAction' smart constructor.
data BatchStopUpdateAction = BatchStopUpdateAction'
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

-- | Creates a value of 'BatchStopUpdateAction' with the minimum fields required to make a request.
--
-- * 'cacheClusterIds' - The cache cluster IDs
-- * 'replicationGroupIds' - The replication group IDs
-- * 'serviceUpdateName' - The unique ID of the service update
mkBatchStopUpdateAction ::
  -- | 'serviceUpdateName'
  Lude.Text ->
  BatchStopUpdateAction
mkBatchStopUpdateAction pServiceUpdateName_ =
  BatchStopUpdateAction'
    { cacheClusterIds = Lude.Nothing,
      replicationGroupIds = Lude.Nothing,
      serviceUpdateName = pServiceUpdateName_
    }

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaCacheClusterIds :: Lens.Lens' BatchStopUpdateAction (Lude.Maybe [Lude.Text])
bsuaCacheClusterIds = Lens.lens (cacheClusterIds :: BatchStopUpdateAction -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheClusterIds = a} :: BatchStopUpdateAction)
{-# DEPRECATED bsuaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaReplicationGroupIds :: Lens.Lens' BatchStopUpdateAction (Lude.Maybe [Lude.Text])
bsuaReplicationGroupIds = Lens.lens (replicationGroupIds :: BatchStopUpdateAction -> Lude.Maybe [Lude.Text]) (\s a -> s {replicationGroupIds = a} :: BatchStopUpdateAction)
{-# DEPRECATED bsuaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaServiceUpdateName :: Lens.Lens' BatchStopUpdateAction Lude.Text
bsuaServiceUpdateName = Lens.lens (serviceUpdateName :: BatchStopUpdateAction -> Lude.Text) (\s a -> s {serviceUpdateName = a} :: BatchStopUpdateAction)
{-# DEPRECATED bsuaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

instance Lude.AWSRequest BatchStopUpdateAction where
  type Rs BatchStopUpdateAction = UpdateActionResultsMessage
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "BatchStopUpdateActionResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders BatchStopUpdateAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchStopUpdateAction where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchStopUpdateAction where
  toQuery BatchStopUpdateAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BatchStopUpdateAction" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> cacheClusterIds),
        "ReplicationGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> replicationGroupIds),
        "ServiceUpdateName" Lude.=: serviceUpdateName
      ]

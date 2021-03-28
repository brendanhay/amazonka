{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchStopUpdateAction (..)
    , mkBatchStopUpdateAction
    -- ** Request lenses
    , bsuaServiceUpdateName
    , bsuaCacheClusterIds
    , bsuaReplicationGroupIds

     -- * Destructuring the response
    , Types.UpdateActionResultsMessage (..)
    , Types.mkUpdateActionResultsMessage
    -- ** Response lenses
    , Types.uarmProcessedUpdateActions
    , Types.uarmUnprocessedUpdateActions
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchStopUpdateAction' smart constructor.
data BatchStopUpdateAction = BatchStopUpdateAction'
  { serviceUpdateName :: Core.Text
    -- ^ The unique ID of the service update
  , cacheClusterIds :: Core.Maybe [Core.Text]
    -- ^ The cache cluster IDs
  , replicationGroupIds :: Core.Maybe [Core.Text]
    -- ^ The replication group IDs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopUpdateAction' value with any optional fields omitted.
mkBatchStopUpdateAction
    :: Core.Text -- ^ 'serviceUpdateName'
    -> BatchStopUpdateAction
mkBatchStopUpdateAction serviceUpdateName
  = BatchStopUpdateAction'{serviceUpdateName,
                           cacheClusterIds = Core.Nothing, replicationGroupIds = Core.Nothing}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaServiceUpdateName :: Lens.Lens' BatchStopUpdateAction Core.Text
bsuaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# INLINEABLE bsuaServiceUpdateName #-}
{-# DEPRECATED serviceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead"  #-}

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaCacheClusterIds :: Lens.Lens' BatchStopUpdateAction (Core.Maybe [Core.Text])
bsuaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# INLINEABLE bsuaCacheClusterIds #-}
{-# DEPRECATED cacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead"  #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaReplicationGroupIds :: Lens.Lens' BatchStopUpdateAction (Core.Maybe [Core.Text])
bsuaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# INLINEABLE bsuaReplicationGroupIds #-}
{-# DEPRECATED replicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead"  #-}

instance Core.ToQuery BatchStopUpdateAction where
        toQuery BatchStopUpdateAction{..}
          = Core.toQueryPair "Action" ("BatchStopUpdateAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceUpdateName" serviceUpdateName
              Core.<>
              Core.toQueryPair "CacheClusterIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   cacheClusterIds)
              Core.<>
              Core.toQueryPair "ReplicationGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   replicationGroupIds)

instance Core.ToHeaders BatchStopUpdateAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchStopUpdateAction where
        type Rs BatchStopUpdateAction = Types.UpdateActionResultsMessage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "BatchStopUpdateActionResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

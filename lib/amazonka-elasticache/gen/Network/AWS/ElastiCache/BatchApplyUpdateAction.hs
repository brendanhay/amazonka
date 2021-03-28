{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      BatchApplyUpdateAction (..)
    , mkBatchApplyUpdateAction
    -- ** Request lenses
    , bauaServiceUpdateName
    , bauaCacheClusterIds
    , bauaReplicationGroupIds

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

-- | /See:/ 'mkBatchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { serviceUpdateName :: Core.Text
    -- ^ The unique ID of the service update
  , cacheClusterIds :: Core.Maybe [Core.Text]
    -- ^ The cache cluster IDs
  , replicationGroupIds :: Core.Maybe [Core.Text]
    -- ^ The replication group IDs
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchApplyUpdateAction' value with any optional fields omitted.
mkBatchApplyUpdateAction
    :: Core.Text -- ^ 'serviceUpdateName'
    -> BatchApplyUpdateAction
mkBatchApplyUpdateAction serviceUpdateName
  = BatchApplyUpdateAction'{serviceUpdateName,
                            cacheClusterIds = Core.Nothing, replicationGroupIds = Core.Nothing}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaServiceUpdateName :: Lens.Lens' BatchApplyUpdateAction Core.Text
bauaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# INLINEABLE bauaServiceUpdateName #-}
{-# DEPRECATED serviceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead"  #-}

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaCacheClusterIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Core.Text])
bauaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# INLINEABLE bauaCacheClusterIds #-}
{-# DEPRECATED cacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead"  #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaReplicationGroupIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Core.Text])
bauaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# INLINEABLE bauaReplicationGroupIds #-}
{-# DEPRECATED replicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead"  #-}

instance Core.ToQuery BatchApplyUpdateAction where
        toQuery BatchApplyUpdateAction{..}
          = Core.toQueryPair "Action" ("BatchApplyUpdateAction" :: Core.Text)
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

instance Core.ToHeaders BatchApplyUpdateAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest BatchApplyUpdateAction where
        type Rs BatchApplyUpdateAction = Types.UpdateActionResultsMessage
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
          = Response.receiveXMLWrapper "BatchApplyUpdateActionResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    bsuaServiceUpdateName,
    bsuaCacheClusterIds,
    bsuaReplicationGroupIds,

    -- * Destructuring the response
    Types.UpdateActionResultsMessage (..),
    Types.mkUpdateActionResultsMessage,

    -- ** Response lenses
    Types.uarmProcessedUpdateActions,
    Types.uarmUnprocessedUpdateActions,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchStopUpdateAction' smart constructor.
data BatchStopUpdateAction = BatchStopUpdateAction'
  { -- | The unique ID of the service update
    serviceUpdateName :: Types.String,
    -- | The cache cluster IDs
    cacheClusterIds :: Core.Maybe [Types.String],
    -- | The replication group IDs
    replicationGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchStopUpdateAction' value with any optional fields omitted.
mkBatchStopUpdateAction ::
  -- | 'serviceUpdateName'
  Types.String ->
  BatchStopUpdateAction
mkBatchStopUpdateAction serviceUpdateName =
  BatchStopUpdateAction'
    { serviceUpdateName,
      cacheClusterIds = Core.Nothing,
      replicationGroupIds = Core.Nothing
    }

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaServiceUpdateName :: Lens.Lens' BatchStopUpdateAction Types.String
bsuaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED bsuaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaCacheClusterIds :: Lens.Lens' BatchStopUpdateAction (Core.Maybe [Types.String])
bsuaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# DEPRECATED bsuaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsuaReplicationGroupIds :: Lens.Lens' BatchStopUpdateAction (Core.Maybe [Types.String])
bsuaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# DEPRECATED bsuaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

instance Core.AWSRequest BatchStopUpdateAction where
  type Rs BatchStopUpdateAction = Types.UpdateActionResultsMessage
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "BatchStopUpdateAction")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ServiceUpdateName" serviceUpdateName)
                Core.<> ( Core.toQueryValue
                            "CacheClusterIds"
                            (Core.toQueryList "member" Core.<$> cacheClusterIds)
                        )
                Core.<> ( Core.toQueryValue
                            "ReplicationGroupIds"
                            (Core.toQueryList "member" Core.<$> replicationGroupIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "BatchStopUpdateActionResult"
      (\s h x -> Core.parseXML x)

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    bauaServiceUpdateName,
    bauaCacheClusterIds,
    bauaReplicationGroupIds,

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

-- | /See:/ 'mkBatchApplyUpdateAction' smart constructor.
data BatchApplyUpdateAction = BatchApplyUpdateAction'
  { -- | The unique ID of the service update
    serviceUpdateName :: Types.String,
    -- | The cache cluster IDs
    cacheClusterIds :: Core.Maybe [Types.String],
    -- | The replication group IDs
    replicationGroupIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchApplyUpdateAction' value with any optional fields omitted.
mkBatchApplyUpdateAction ::
  -- | 'serviceUpdateName'
  Types.String ->
  BatchApplyUpdateAction
mkBatchApplyUpdateAction serviceUpdateName =
  BatchApplyUpdateAction'
    { serviceUpdateName,
      cacheClusterIds = Core.Nothing,
      replicationGroupIds = Core.Nothing
    }

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaServiceUpdateName :: Lens.Lens' BatchApplyUpdateAction Types.String
bauaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED bauaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The cache cluster IDs
--
-- /Note:/ Consider using 'cacheClusterIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaCacheClusterIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Types.String])
bauaCacheClusterIds = Lens.field @"cacheClusterIds"
{-# DEPRECATED bauaCacheClusterIds "Use generic-lens or generic-optics with 'cacheClusterIds' instead." #-}

-- | The replication group IDs
--
-- /Note:/ Consider using 'replicationGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bauaReplicationGroupIds :: Lens.Lens' BatchApplyUpdateAction (Core.Maybe [Types.String])
bauaReplicationGroupIds = Lens.field @"replicationGroupIds"
{-# DEPRECATED bauaReplicationGroupIds "Use generic-lens or generic-optics with 'replicationGroupIds' instead." #-}

instance Core.AWSRequest BatchApplyUpdateAction where
  type Rs BatchApplyUpdateAction = Types.UpdateActionResultsMessage
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
            ( Core.pure ("Action", "BatchApplyUpdateAction")
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
      "BatchApplyUpdateActionResult"
      (\s h x -> Core.parseXML x)

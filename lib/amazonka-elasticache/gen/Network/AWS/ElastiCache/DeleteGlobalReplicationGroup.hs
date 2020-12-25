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
    dGlobalReplicationGroupId,
    dRetainPrimaryReplicationGroup,

    -- * Destructuring the response
    DeleteGlobalReplicationGroupResponse (..),
    mkDeleteGlobalReplicationGroupResponse,

    -- ** Response lenses
    dgrgrgrsGlobalReplicationGroup,
    dgrgrgrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Types.String,
    -- | The primary replication group is retained as a standalone replication group.
    retainPrimaryReplicationGroup :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalReplicationGroup' value with any optional fields omitted.
mkDeleteGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Types.String ->
  -- | 'retainPrimaryReplicationGroup'
  Core.Bool ->
  DeleteGlobalReplicationGroup
mkDeleteGlobalReplicationGroup
  globalReplicationGroupId
  retainPrimaryReplicationGroup =
    DeleteGlobalReplicationGroup'
      { globalReplicationGroupId,
        retainPrimaryReplicationGroup
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGlobalReplicationGroupId :: Lens.Lens' DeleteGlobalReplicationGroup Types.String
dGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED dGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The primary replication group is retained as a standalone replication group.
--
-- /Note:/ Consider using 'retainPrimaryReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRetainPrimaryReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroup Core.Bool
dRetainPrimaryReplicationGroup = Lens.field @"retainPrimaryReplicationGroup"
{-# DEPRECATED dRetainPrimaryReplicationGroup "Use generic-lens or generic-optics with 'retainPrimaryReplicationGroup' instead." #-}

instance Core.AWSRequest DeleteGlobalReplicationGroup where
  type
    Rs DeleteGlobalReplicationGroup =
      DeleteGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "DeleteGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupId"
                            globalReplicationGroupId
                        )
                Core.<> ( Core.toQueryValue
                            "RetainPrimaryReplicationGroup"
                            retainPrimaryReplicationGroup
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteGlobalReplicationGroupResult"
      ( \s h x ->
          DeleteGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalReplicationGroupResponse' value with any optional fields omitted.
mkDeleteGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGlobalReplicationGroupResponse
mkDeleteGlobalReplicationGroupResponse responseStatus =
  DeleteGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrgrsGlobalReplicationGroup :: Lens.Lens' DeleteGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
dgrgrgrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED dgrgrgrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrgrsResponseStatus :: Lens.Lens' DeleteGlobalReplicationGroupResponse Core.Int
dgrgrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrgrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

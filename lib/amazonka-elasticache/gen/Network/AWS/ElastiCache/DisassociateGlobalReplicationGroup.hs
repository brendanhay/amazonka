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
    dgrgGlobalReplicationGroupId,
    dgrgReplicationGroupId,
    dgrgReplicationGroupRegion,

    -- * Destructuring the response
    DisassociateGlobalReplicationGroupResponse (..),
    mkDisassociateGlobalReplicationGroupResponse,

    -- ** Response lenses
    dgrgrrsGlobalReplicationGroup,
    dgrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Types.String,
    -- | The name of the secondary cluster you wish to remove from the Global Datastore
    replicationGroupId :: Types.String,
    -- | The AWS region of secondary cluster you wish to remove from the Global Datastore
    replicationGroupRegion :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateGlobalReplicationGroup' value with any optional fields omitted.
mkDisassociateGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Types.String ->
  -- | 'replicationGroupId'
  Types.String ->
  -- | 'replicationGroupRegion'
  Types.String ->
  DisassociateGlobalReplicationGroup
mkDisassociateGlobalReplicationGroup
  globalReplicationGroupId
  replicationGroupId
  replicationGroupRegion =
    DisassociateGlobalReplicationGroup'
      { globalReplicationGroupId,
        replicationGroupId,
        replicationGroupRegion
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgGlobalReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Types.String
dgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED dgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The name of the secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgReplicationGroupId :: Lens.Lens' DisassociateGlobalReplicationGroup Types.String
dgrgReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED dgrgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The AWS region of secondary cluster you wish to remove from the Global Datastore
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgReplicationGroupRegion :: Lens.Lens' DisassociateGlobalReplicationGroup Types.String
dgrgReplicationGroupRegion = Lens.field @"replicationGroupRegion"
{-# DEPRECATED dgrgReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

instance Core.AWSRequest DisassociateGlobalReplicationGroup where
  type
    Rs DisassociateGlobalReplicationGroup =
      DisassociateGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "DisassociateGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupId"
                            globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> ( Core.toQueryValue
                            "ReplicationGroupRegion"
                            replicationGroupRegion
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DisassociateGlobalReplicationGroupResult"
      ( \s h x ->
          DisassociateGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateGlobalReplicationGroupResponse' smart constructor.
data DisassociateGlobalReplicationGroupResponse = DisassociateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateGlobalReplicationGroupResponse' value with any optional fields omitted.
mkDisassociateGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateGlobalReplicationGroupResponse
mkDisassociateGlobalReplicationGroupResponse responseStatus =
  DisassociateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrrsGlobalReplicationGroup :: Lens.Lens' DisassociateGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
dgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED dgrgrrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrrsResponseStatus :: Lens.Lens' DisassociateGlobalReplicationGroupResponse Core.Int
dgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

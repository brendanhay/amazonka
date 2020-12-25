{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveFromGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an Aurora secondary cluster from an Aurora global database cluster. The cluster becomes a standalone cluster with read-write capability instead of being read-only and receiving data from a primary cluster in a different region.
module Network.AWS.RDS.RemoveFromGlobalCluster
  ( -- * Creating a request
    RemoveFromGlobalCluster (..),
    mkRemoveFromGlobalCluster,

    -- ** Request lenses
    rfgcDbClusterIdentifier,
    rfgcGlobalClusterIdentifier,

    -- * Destructuring the response
    RemoveFromGlobalClusterResponse (..),
    mkRemoveFromGlobalClusterResponse,

    -- ** Response lenses
    rfgcrrsGlobalCluster,
    rfgcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveFromGlobalCluster' smart constructor.
data RemoveFromGlobalCluster = RemoveFromGlobalCluster'
  { -- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
    dbClusterIdentifier :: Core.Maybe Types.DbClusterIdentifier,
    -- | The cluster identifier to detach from the Aurora global database cluster.
    globalClusterIdentifier :: Core.Maybe Types.GlobalClusterIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFromGlobalCluster' value with any optional fields omitted.
mkRemoveFromGlobalCluster ::
  RemoveFromGlobalCluster
mkRemoveFromGlobalCluster =
  RemoveFromGlobalCluster'
    { dbClusterIdentifier = Core.Nothing,
      globalClusterIdentifier = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) identifying the cluster that was detached from the Aurora global database cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcDbClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Types.DbClusterIdentifier)
rfgcDbClusterIdentifier = Lens.field @"dbClusterIdentifier"
{-# DEPRECATED rfgcDbClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The cluster identifier to detach from the Aurora global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcGlobalClusterIdentifier :: Lens.Lens' RemoveFromGlobalCluster (Core.Maybe Types.GlobalClusterIdentifier)
rfgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED rfgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

instance Core.AWSRequest RemoveFromGlobalCluster where
  type Rs RemoveFromGlobalCluster = RemoveFromGlobalClusterResponse
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
            ( Core.pure ("Action", "RemoveFromGlobalCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DbClusterIdentifier"
                            Core.<$> dbClusterIdentifier
                        )
                Core.<> ( Core.toQueryValue "GlobalClusterIdentifier"
                            Core.<$> globalClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RemoveFromGlobalClusterResult"
      ( \s h x ->
          RemoveFromGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveFromGlobalClusterResponse' smart constructor.
data RemoveFromGlobalClusterResponse = RemoveFromGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFromGlobalClusterResponse' value with any optional fields omitted.
mkRemoveFromGlobalClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveFromGlobalClusterResponse
mkRemoveFromGlobalClusterResponse responseStatus =
  RemoveFromGlobalClusterResponse'
    { globalCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrrsGlobalCluster :: Lens.Lens' RemoveFromGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
rfgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# DEPRECATED rfgcrrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfgcrrsResponseStatus :: Lens.Lens' RemoveFromGlobalClusterResponse Core.Int
rfgcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rfgcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a global database cluster. The primary and secondary clusters must already be detached or destroyed first.
module Network.AWS.RDS.DeleteGlobalCluster
  ( -- * Creating a request
    DeleteGlobalCluster (..),
    mkDeleteGlobalCluster,

    -- ** Request lenses
    dgcGlobalClusterIdentifier,

    -- * Destructuring the response
    DeleteGlobalClusterResponse (..),
    mkDeleteGlobalClusterResponse,

    -- ** Response lenses
    dgcrrsGlobalCluster,
    dgcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGlobalCluster' smart constructor.
newtype DeleteGlobalCluster = DeleteGlobalCluster'
  { -- | The cluster identifier of the global database cluster being deleted.
    globalClusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalCluster' value with any optional fields omitted.
mkDeleteGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Types.String ->
  DeleteGlobalCluster
mkDeleteGlobalCluster globalClusterIdentifier =
  DeleteGlobalCluster' {globalClusterIdentifier}

-- | The cluster identifier of the global database cluster being deleted.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcGlobalClusterIdentifier :: Lens.Lens' DeleteGlobalCluster Types.String
dgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED dgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

instance Core.AWSRequest DeleteGlobalCluster where
  type Rs DeleteGlobalCluster = DeleteGlobalClusterResponse
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
            ( Core.pure ("Action", "DeleteGlobalCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "GlobalClusterIdentifier"
                            globalClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteGlobalClusterResult"
      ( \s h x ->
          DeleteGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteGlobalClusterResponse' smart constructor.
data DeleteGlobalClusterResponse = DeleteGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGlobalClusterResponse' value with any optional fields omitted.
mkDeleteGlobalClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteGlobalClusterResponse
mkDeleteGlobalClusterResponse responseStatus =
  DeleteGlobalClusterResponse'
    { globalCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsGlobalCluster :: Lens.Lens' DeleteGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
dgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# DEPRECATED dgcrrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcrrsResponseStatus :: Lens.Lens' DeleteGlobalClusterResponse Core.Int
dgcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

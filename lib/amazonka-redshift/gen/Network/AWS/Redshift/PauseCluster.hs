{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PauseCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Pauses a cluster.
module Network.AWS.Redshift.PauseCluster
  ( -- * Creating a request
    PauseCluster (..),
    mkPauseCluster,

    -- ** Request lenses
    pcClusterIdentifier,

    -- * Destructuring the response
    PauseClusterResponse (..),
    mkPauseClusterResponse,

    -- ** Response lenses
    pcrrsCluster,
    pcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation.
--
-- /See:/ 'mkPauseCluster' smart constructor.
newtype PauseCluster = PauseCluster'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PauseCluster' value with any optional fields omitted.
mkPauseCluster ::
  -- | 'clusterIdentifier'
  Types.String ->
  PauseCluster
mkPauseCluster clusterIdentifier = PauseCluster' {clusterIdentifier}

-- | The identifier of the cluster to be paused.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcClusterIdentifier :: Lens.Lens' PauseCluster Types.String
pcClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED pcClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest PauseCluster where
  type Rs PauseCluster = PauseClusterResponse
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
            ( Core.pure ("Action", "PauseCluster")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "PauseClusterResult"
      ( \s h x ->
          PauseClusterResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPauseClusterResponse' smart constructor.
data PauseClusterResponse = PauseClusterResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PauseClusterResponse' value with any optional fields omitted.
mkPauseClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PauseClusterResponse
mkPauseClusterResponse responseStatus =
  PauseClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsCluster :: Lens.Lens' PauseClusterResponse (Core.Maybe Types.Cluster)
pcrrsCluster = Lens.field @"cluster"
{-# DEPRECATED pcrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrrsResponseStatus :: Lens.Lens' PauseClusterResponse Core.Int
pcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

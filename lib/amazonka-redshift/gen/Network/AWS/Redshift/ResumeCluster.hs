{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ResumeCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes a paused cluster.
module Network.AWS.Redshift.ResumeCluster
  ( -- * Creating a request
    ResumeCluster (..),
    mkResumeCluster,

    -- ** Request lenses
    rcgClusterIdentifier,

    -- * Destructuring the response
    ResumeClusterResponse (..),
    mkResumeClusterResponse,

    -- ** Response lenses
    rcrfrsCluster,
    rcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Describes a resume cluster operation. For example, a scheduled action to run the @ResumeCluster@ API operation.
--
-- /See:/ 'mkResumeCluster' smart constructor.
newtype ResumeCluster = ResumeCluster'
  { -- | The identifier of the cluster to be resumed.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeCluster' value with any optional fields omitted.
mkResumeCluster ::
  -- | 'clusterIdentifier'
  Types.String ->
  ResumeCluster
mkResumeCluster clusterIdentifier =
  ResumeCluster' {clusterIdentifier}

-- | The identifier of the cluster to be resumed.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgClusterIdentifier :: Lens.Lens' ResumeCluster Types.String
rcgClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rcgClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest ResumeCluster where
  type Rs ResumeCluster = ResumeClusterResponse
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
            ( Core.pure ("Action", "ResumeCluster")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ResumeClusterResult"
      ( \s h x ->
          ResumeClusterResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkResumeClusterResponse' smart constructor.
data ResumeClusterResponse = ResumeClusterResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResumeClusterResponse' value with any optional fields omitted.
mkResumeClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResumeClusterResponse
mkResumeClusterResponse responseStatus =
  ResumeClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrfrsCluster :: Lens.Lens' ResumeClusterResponse (Core.Maybe Types.Cluster)
rcrfrsCluster = Lens.field @"cluster"
{-# DEPRECATED rcrfrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrfrsResponseStatus :: Lens.Lens' ResumeClusterResponse Core.Int
rcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

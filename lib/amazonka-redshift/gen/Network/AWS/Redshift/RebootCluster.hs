{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RebootCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a cluster. This action is taken as soon as possible. It results in a momentary outage to the cluster, during which the cluster status is set to @rebooting@ . A cluster event is created when the reboot is completed. Any pending cluster modifications (see 'ModifyCluster' ) are applied at this reboot. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RebootCluster
  ( -- * Creating a request
    RebootCluster (..),
    mkRebootCluster,

    -- ** Request lenses
    rcfClusterIdentifier,

    -- * Destructuring the response
    RebootClusterResponse (..),
    mkRebootClusterResponse,

    -- ** Response lenses
    rrsCluster,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRebootCluster' smart constructor.
newtype RebootCluster = RebootCluster'
  { -- | The cluster identifier.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootCluster' value with any optional fields omitted.
mkRebootCluster ::
  -- | 'clusterIdentifier'
  Types.String ->
  RebootCluster
mkRebootCluster clusterIdentifier =
  RebootCluster' {clusterIdentifier}

-- | The cluster identifier.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfClusterIdentifier :: Lens.Lens' RebootCluster Types.String
rcfClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rcfClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest RebootCluster where
  type Rs RebootCluster = RebootClusterResponse
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
            ( Core.pure ("Action", "RebootCluster")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "RebootClusterResult"
      ( \s h x ->
          RebootClusterResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RebootClusterResponse' value with any optional fields omitted.
mkRebootClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebootClusterResponse
mkRebootClusterResponse responseStatus =
  RebootClusterResponse' {cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsCluster :: Lens.Lens' RebootClusterResponse (Core.Maybe Types.Cluster)
rrsCluster = Lens.field @"cluster"
{-# DEPRECATED rrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RebootClusterResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

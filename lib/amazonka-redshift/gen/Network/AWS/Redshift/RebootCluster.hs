{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RebootCluster (..)
    , mkRebootCluster
    -- ** Request lenses
    , rcfClusterIdentifier

    -- * Destructuring the response
    , RebootClusterResponse (..)
    , mkRebootClusterResponse
    -- ** Response lenses
    , rrsCluster
    , rrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkRebootCluster' smart constructor.
newtype RebootCluster = RebootCluster'
  { clusterIdentifier :: Core.Text
    -- ^ The cluster identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootCluster' value with any optional fields omitted.
mkRebootCluster
    :: Core.Text -- ^ 'clusterIdentifier'
    -> RebootCluster
mkRebootCluster clusterIdentifier
  = RebootCluster'{clusterIdentifier}

-- | The cluster identifier.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfClusterIdentifier :: Lens.Lens' RebootCluster Core.Text
rcfClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE rcfClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery RebootCluster where
        toQuery RebootCluster{..}
          = Core.toQueryPair "Action" ("RebootCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.ToHeaders RebootCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RebootCluster where
        type Rs RebootCluster = RebootClusterResponse
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
          = Response.receiveXMLWrapper "RebootClusterResult"
              (\ s h x ->
                 RebootClusterResponse' Core.<$>
                   (x Core..@? "Cluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootClusterResponse' smart constructor.
data RebootClusterResponse = RebootClusterResponse'
  { cluster :: Core.Maybe Types.Cluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RebootClusterResponse' value with any optional fields omitted.
mkRebootClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebootClusterResponse
mkRebootClusterResponse responseStatus
  = RebootClusterResponse'{cluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsCluster :: Lens.Lens' RebootClusterResponse (Core.Maybe Types.Cluster)
rrsCluster = Lens.field @"cluster"
{-# INLINEABLE rrsCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RebootClusterResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon Aurora DB cluster that was stopped using the AWS console, the stop-db-cluster AWS CLI command, or the StopDBCluster action.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.StartDBCluster
    (
    -- * Creating a request
      StartDBCluster (..)
    , mkStartDBCluster
    -- ** Request lenses
    , sdbcDBClusterIdentifier

    -- * Destructuring the response
    , StartDBClusterResponse (..)
    , mkStartDBClusterResponse
    -- ** Response lenses
    , sdbcrrsDBCluster
    , sdbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartDBCluster' smart constructor.
newtype StartDBCluster = StartDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartDBCluster' value with any optional fields omitted.
mkStartDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> StartDBCluster
mkStartDBCluster dBClusterIdentifier
  = StartDBCluster'{dBClusterIdentifier}

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcDBClusterIdentifier :: Lens.Lens' StartDBCluster Core.Text
sdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE sdbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

instance Core.ToQuery StartDBCluster where
        toQuery StartDBCluster{..}
          = Core.toQueryPair "Action" ("StartDBCluster" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier

instance Core.ToHeaders StartDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest StartDBCluster where
        type Rs StartDBCluster = StartDBClusterResponse
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
          = Response.receiveXMLWrapper "StartDBClusterResult"
              (\ s h x ->
                 StartDBClusterResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartDBClusterResponse' smart constructor.
data StartDBClusterResponse = StartDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartDBClusterResponse' value with any optional fields omitted.
mkStartDBClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartDBClusterResponse
mkStartDBClusterResponse responseStatus
  = StartDBClusterResponse'{dBCluster = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrrsDBCluster :: Lens.Lens' StartDBClusterResponse (Core.Maybe Types.DBCluster)
sdbcrrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE sdbcrrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbcrrsResponseStatus :: Lens.Lens' StartDBClusterResponse Core.Int
sdbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

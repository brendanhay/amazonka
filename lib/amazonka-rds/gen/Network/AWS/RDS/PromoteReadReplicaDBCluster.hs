{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PromoteReadReplicaDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Promotes a read replica DB cluster to a standalone DB cluster.
module Network.AWS.RDS.PromoteReadReplicaDBCluster
    (
    -- * Creating a request
      PromoteReadReplicaDBCluster (..)
    , mkPromoteReadReplicaDBCluster
    -- ** Request lenses
    , prrdbcDBClusterIdentifier

    -- * Destructuring the response
    , PromoteReadReplicaDBClusterResponse (..)
    , mkPromoteReadReplicaDBClusterResponse
    -- ** Response lenses
    , prrdbcrrsDBCluster
    , prrdbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkPromoteReadReplicaDBCluster' smart constructor.
newtype PromoteReadReplicaDBCluster = PromoteReadReplicaDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ The identifier of the DB cluster read replica to promote. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster read replica.
--
--
-- Example: @my-cluster-replica1@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PromoteReadReplicaDBCluster' value with any optional fields omitted.
mkPromoteReadReplicaDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> PromoteReadReplicaDBCluster
mkPromoteReadReplicaDBCluster dBClusterIdentifier
  = PromoteReadReplicaDBCluster'{dBClusterIdentifier}

-- | The identifier of the DB cluster read replica to promote. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster read replica.
--
--
-- Example: @my-cluster-replica1@ 
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdbcDBClusterIdentifier :: Lens.Lens' PromoteReadReplicaDBCluster Core.Text
prrdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE prrdbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

instance Core.ToQuery PromoteReadReplicaDBCluster where
        toQuery PromoteReadReplicaDBCluster{..}
          = Core.toQueryPair "Action"
              ("PromoteReadReplicaDBCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier

instance Core.ToHeaders PromoteReadReplicaDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PromoteReadReplicaDBCluster where
        type Rs PromoteReadReplicaDBCluster =
             PromoteReadReplicaDBClusterResponse
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
          = Response.receiveXMLWrapper "PromoteReadReplicaDBClusterResult"
              (\ s h x ->
                 PromoteReadReplicaDBClusterResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPromoteReadReplicaDBClusterResponse' smart constructor.
data PromoteReadReplicaDBClusterResponse = PromoteReadReplicaDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PromoteReadReplicaDBClusterResponse' value with any optional fields omitted.
mkPromoteReadReplicaDBClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PromoteReadReplicaDBClusterResponse
mkPromoteReadReplicaDBClusterResponse responseStatus
  = PromoteReadReplicaDBClusterResponse'{dBCluster = Core.Nothing,
                                         responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdbcrrsDBCluster :: Lens.Lens' PromoteReadReplicaDBClusterResponse (Core.Maybe Types.DBCluster)
prrdbcrrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE prrdbcrrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prrdbcrrsResponseStatus :: Lens.Lens' PromoteReadReplicaDBClusterResponse Core.Int
prrdbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE prrdbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.FailoverDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forces a failover for a DB cluster.
--
-- A failover for a DB cluster promotes one of the Aurora Replicas (read-only instances) in the DB cluster to be the primary instance (the cluster writer).
-- Amazon Aurora will automatically fail over to an Aurora Replica, if one exists, when the primary instance fails. You can force a failover when you want to simulate a failure of a primary instance for testing. Because each instance in a DB cluster has its own endpoint address, you will need to clean up and re-establish any existing connections that use those endpoint addresses when the failover is complete.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.FailoverDBCluster
    (
    -- * Creating a request
      FailoverDBCluster (..)
    , mkFailoverDBCluster
    -- ** Request lenses
    , fdbcDBClusterIdentifier
    , fdbcTargetDBInstanceIdentifier

    -- * Destructuring the response
    , FailoverDBClusterResponse (..)
    , mkFailoverDBClusterResponse
    -- ** Response lenses
    , fdbcrrsDBCluster
    , fdbcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkFailoverDBCluster' smart constructor.
data FailoverDBCluster = FailoverDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ A DB cluster identifier to force a failover for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
  , targetDBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The name of the instance to promote to the primary instance.
--
-- You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverDBCluster' value with any optional fields omitted.
mkFailoverDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> FailoverDBCluster
mkFailoverDBCluster dBClusterIdentifier
  = FailoverDBCluster'{dBClusterIdentifier,
                       targetDBInstanceIdentifier = Core.Nothing}

-- | A DB cluster identifier to force a failover for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdbcDBClusterIdentifier :: Lens.Lens' FailoverDBCluster Core.Text
fdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE fdbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The name of the instance to promote to the primary instance.
--
-- You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
--
-- /Note:/ Consider using 'targetDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdbcTargetDBInstanceIdentifier :: Lens.Lens' FailoverDBCluster (Core.Maybe Core.Text)
fdbcTargetDBInstanceIdentifier = Lens.field @"targetDBInstanceIdentifier"
{-# INLINEABLE fdbcTargetDBInstanceIdentifier #-}
{-# DEPRECATED targetDBInstanceIdentifier "Use generic-lens or generic-optics with 'targetDBInstanceIdentifier' instead"  #-}

instance Core.ToQuery FailoverDBCluster where
        toQuery FailoverDBCluster{..}
          = Core.toQueryPair "Action" ("FailoverDBCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TargetDBInstanceIdentifier")
                targetDBInstanceIdentifier

instance Core.ToHeaders FailoverDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest FailoverDBCluster where
        type Rs FailoverDBCluster = FailoverDBClusterResponse
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
          = Response.receiveXMLWrapper "FailoverDBClusterResult"
              (\ s h x ->
                 FailoverDBClusterResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkFailoverDBClusterResponse' smart constructor.
data FailoverDBClusterResponse = FailoverDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FailoverDBClusterResponse' value with any optional fields omitted.
mkFailoverDBClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> FailoverDBClusterResponse
mkFailoverDBClusterResponse responseStatus
  = FailoverDBClusterResponse'{dBCluster = Core.Nothing,
                               responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdbcrrsDBCluster :: Lens.Lens' FailoverDBClusterResponse (Core.Maybe Types.DBCluster)
fdbcrrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE fdbcrrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdbcrrsResponseStatus :: Lens.Lens' FailoverDBClusterResponse Core.Int
fdbcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE fdbcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

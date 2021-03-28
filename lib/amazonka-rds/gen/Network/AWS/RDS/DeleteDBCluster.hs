{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster. When you delete a DB cluster, all automated backups for that DB cluster are deleted and can't be recovered. Manual DB cluster snapshots of the specified DB cluster are not deleted.
--
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.DeleteDBCluster
    (
    -- * Creating a request
      DeleteDBCluster (..)
    , mkDeleteDBCluster
    -- ** Request lenses
    , dDBClusterIdentifier
    , dFinalDBSnapshotIdentifier
    , dSkipFinalSnapshot

    -- * Destructuring the response
    , DeleteDBClusterResponse (..)
    , mkDeleteDBClusterResponse
    -- ** Response lenses
    , ddbcrfrsDBCluster
    , ddbcrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBCluster' smart constructor.
data DeleteDBCluster = DeleteDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing DBClusterIdentifier.
--
--
  , finalDBSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is disabled. 
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
  , skipFinalSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to skip the creation of a final DB cluster snapshot before the DB cluster is deleted. If skip is specified, no DB cluster snapshot is created. If skip isn't specified, a DB cluster snapshot is created before the DB cluster is deleted. By default, skip isn't specified, and the DB cluster snapshot is created. By default, this parameter is disabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBCluster' value with any optional fields omitted.
mkDeleteDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> DeleteDBCluster
mkDeleteDBCluster dBClusterIdentifier
  = DeleteDBCluster'{dBClusterIdentifier,
                     finalDBSnapshotIdentifier = Core.Nothing,
                     skipFinalSnapshot = Core.Nothing}

-- | The DB cluster identifier for the DB cluster to be deleted. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBClusterIdentifier :: Lens.Lens' DeleteDBCluster Core.Text
dDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE dDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The DB cluster snapshot identifier of the new DB cluster snapshot created when @SkipFinalSnapshot@ is disabled. 
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'finalDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFinalDBSnapshotIdentifier :: Lens.Lens' DeleteDBCluster (Core.Maybe Core.Text)
dFinalDBSnapshotIdentifier = Lens.field @"finalDBSnapshotIdentifier"
{-# INLINEABLE dFinalDBSnapshotIdentifier #-}
{-# DEPRECATED finalDBSnapshotIdentifier "Use generic-lens or generic-optics with 'finalDBSnapshotIdentifier' instead"  #-}

-- | A value that indicates whether to skip the creation of a final DB cluster snapshot before the DB cluster is deleted. If skip is specified, no DB cluster snapshot is created. If skip isn't specified, a DB cluster snapshot is created before the DB cluster is deleted. By default, skip isn't specified, and the DB cluster snapshot is created. By default, this parameter is disabled.
--
-- /Note:/ Consider using 'skipFinalSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSkipFinalSnapshot :: Lens.Lens' DeleteDBCluster (Core.Maybe Core.Bool)
dSkipFinalSnapshot = Lens.field @"skipFinalSnapshot"
{-# INLINEABLE dSkipFinalSnapshot #-}
{-# DEPRECATED skipFinalSnapshot "Use generic-lens or generic-optics with 'skipFinalSnapshot' instead"  #-}

instance Core.ToQuery DeleteDBCluster where
        toQuery DeleteDBCluster{..}
          = Core.toQueryPair "Action" ("DeleteDBCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "FinalDBSnapshotIdentifier")
                finalDBSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SkipFinalSnapshot")
                skipFinalSnapshot

instance Core.ToHeaders DeleteDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBCluster where
        type Rs DeleteDBCluster = DeleteDBClusterResponse
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
          = Response.receiveXMLWrapper "DeleteDBClusterResult"
              (\ s h x ->
                 DeleteDBClusterResponse' Core.<$>
                   (x Core..@? "DBCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBClusterResponse' smart constructor.
data DeleteDBClusterResponse = DeleteDBClusterResponse'
  { dBCluster :: Core.Maybe Types.DBCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDBClusterResponse' value with any optional fields omitted.
mkDeleteDBClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDBClusterResponse
mkDeleteDBClusterResponse responseStatus
  = DeleteDBClusterResponse'{dBCluster = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrfrsDBCluster :: Lens.Lens' DeleteDBClusterResponse (Core.Maybe Types.DBCluster)
ddbcrfrsDBCluster = Lens.field @"dBCluster"
{-# INLINEABLE ddbcrfrsDBCluster #-}
{-# DEPRECATED dBCluster "Use generic-lens or generic-optics with 'dBCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrfrsResponseStatus :: Lens.Lens' DeleteDBClusterResponse Core.Int
ddbcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

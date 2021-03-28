{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy operation is terminated.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.DeleteDBClusterSnapshot
    (
    -- * Creating a request
      DeleteDBClusterSnapshot (..)
    , mkDeleteDBClusterSnapshot
    -- ** Request lenses
    , ddbcsDBClusterSnapshotIdentifier

    -- * Destructuring the response
    , DeleteDBClusterSnapshotResponse (..)
    , mkDeleteDBClusterSnapshotResponse
    -- ** Response lenses
    , ddbcsrrsDBClusterSnapshot
    , ddbcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBClusterSnapshot' smart constructor.
newtype DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
  { dBClusterSnapshotIdentifier :: Core.Text
    -- ^ The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBClusterSnapshot' value with any optional fields omitted.
mkDeleteDBClusterSnapshot
    :: Core.Text -- ^ 'dBClusterSnapshotIdentifier'
    -> DeleteDBClusterSnapshot
mkDeleteDBClusterSnapshot dBClusterSnapshotIdentifier
  = DeleteDBClusterSnapshot'{dBClusterSnapshotIdentifier}

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsDBClusterSnapshotIdentifier :: Lens.Lens' DeleteDBClusterSnapshot Core.Text
ddbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE ddbcsDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

instance Core.ToQuery DeleteDBClusterSnapshot where
        toQuery DeleteDBClusterSnapshot{..}
          = Core.toQueryPair "Action"
              ("DeleteDBClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterSnapshotIdentifier"
                dBClusterSnapshotIdentifier

instance Core.ToHeaders DeleteDBClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBClusterSnapshot where
        type Rs DeleteDBClusterSnapshot = DeleteDBClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "DeleteDBClusterSnapshotResult"
              (\ s h x ->
                 DeleteDBClusterSnapshotResponse' Core.<$>
                   (x Core..@? "DBClusterSnapshot") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBClusterSnapshotResponse' smart constructor.
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
  { dBClusterSnapshot :: Core.Maybe Types.DBClusterSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDBClusterSnapshotResponse' value with any optional fields omitted.
mkDeleteDBClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDBClusterSnapshotResponse
mkDeleteDBClusterSnapshotResponse responseStatus
  = DeleteDBClusterSnapshotResponse'{dBClusterSnapshot =
                                       Core.Nothing,
                                     responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrrsDBClusterSnapshot :: Lens.Lens' DeleteDBClusterSnapshotResponse (Core.Maybe Types.DBClusterSnapshot)
ddbcsrrsDBClusterSnapshot = Lens.field @"dBClusterSnapshot"
{-# INLINEABLE ddbcsrrsDBClusterSnapshot #-}
{-# DEPRECATED dBClusterSnapshot "Use generic-lens or generic-optics with 'dBClusterSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrrsResponseStatus :: Lens.Lens' DeleteDBClusterSnapshotResponse Core.Int
ddbcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

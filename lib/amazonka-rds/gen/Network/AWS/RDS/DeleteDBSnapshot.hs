{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB snapshot. If the snapshot is being copied, the copy operation is terminated.
module Network.AWS.RDS.DeleteDBSnapshot
    (
    -- * Creating a request
      DeleteDBSnapshot (..)
    , mkDeleteDBSnapshot
    -- ** Request lenses
    , dDBSnapshotIdentifier

    -- * Destructuring the response
    , DeleteDBSnapshotResponse (..)
    , mkDeleteDBSnapshotResponse
    -- ** Response lenses
    , ddbsrfrsDBSnapshot
    , ddbsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteDBSnapshot' smart constructor.
newtype DeleteDBSnapshot = DeleteDBSnapshot'
  { dBSnapshotIdentifier :: Core.Text
    -- ^ The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDBSnapshot' value with any optional fields omitted.
mkDeleteDBSnapshot
    :: Core.Text -- ^ 'dBSnapshotIdentifier'
    -> DeleteDBSnapshot
mkDeleteDBSnapshot dBSnapshotIdentifier
  = DeleteDBSnapshot'{dBSnapshotIdentifier}

-- | The DB snapshot identifier.
--
-- Constraints: Must be the name of an existing DB snapshot in the @available@ state.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSnapshotIdentifier :: Lens.Lens' DeleteDBSnapshot Core.Text
dDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE dDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

instance Core.ToQuery DeleteDBSnapshot where
        toQuery DeleteDBSnapshot{..}
          = Core.toQueryPair "Action" ("DeleteDBSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBSnapshotIdentifier" dBSnapshotIdentifier

instance Core.ToHeaders DeleteDBSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteDBSnapshot where
        type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
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
          = Response.receiveXMLWrapper "DeleteDBSnapshotResult"
              (\ s h x ->
                 DeleteDBSnapshotResponse' Core.<$>
                   (x Core..@? "DBSnapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDBSnapshotResponse' value with any optional fields omitted.
mkDeleteDBSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDBSnapshotResponse
mkDeleteDBSnapshotResponse responseStatus
  = DeleteDBSnapshotResponse'{dBSnapshot = Core.Nothing,
                              responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrfrsDBSnapshot :: Lens.Lens' DeleteDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
ddbsrfrsDBSnapshot = Lens.field @"dBSnapshot"
{-# INLINEABLE ddbsrfrsDBSnapshot #-}
{-# DEPRECATED dBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrfrsResponseStatus :: Lens.Lens' DeleteDBSnapshotResponse Core.Int
ddbsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

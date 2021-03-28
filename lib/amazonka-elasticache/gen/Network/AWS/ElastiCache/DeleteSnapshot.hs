{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing snapshot. When you receive a successful response from this operation, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteSnapshot
    (
    -- * Creating a request
      DeleteSnapshot (..)
    , mkDeleteSnapshot
    -- ** Request lenses
    , dSnapshotName

    -- * Destructuring the response
    , DeleteSnapshotResponse (..)
    , mkDeleteSnapshotResponse
    -- ** Response lenses
    , dsrfrsSnapshot
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteSnapshot@ operation.
--
-- /See:/ 'mkDeleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot'
  { snapshotName :: Core.Text
    -- ^ The name of the snapshot to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshot' value with any optional fields omitted.
mkDeleteSnapshot
    :: Core.Text -- ^ 'snapshotName'
    -> DeleteSnapshot
mkDeleteSnapshot snapshotName = DeleteSnapshot'{snapshotName}

-- | The name of the snapshot to be deleted.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotName :: Lens.Lens' DeleteSnapshot Core.Text
dSnapshotName = Lens.field @"snapshotName"
{-# INLINEABLE dSnapshotName #-}
{-# DEPRECATED snapshotName "Use generic-lens or generic-optics with 'snapshotName' instead"  #-}

instance Core.ToQuery DeleteSnapshot where
        toQuery DeleteSnapshot{..}
          = Core.toQueryPair "Action" ("DeleteSnapshot" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotName" snapshotName

instance Core.ToHeaders DeleteSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteSnapshot where
        type Rs DeleteSnapshot = DeleteSnapshotResponse
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
          = Response.receiveXMLWrapper "DeleteSnapshotResult"
              (\ s h x ->
                 DeleteSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteSnapshotResponse' value with any optional fields omitted.
mkDeleteSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSnapshotResponse
mkDeleteSnapshotResponse responseStatus
  = DeleteSnapshotResponse'{snapshot = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSnapshot :: Lens.Lens' DeleteSnapshotResponse (Core.Maybe Types.Snapshot)
dsrfrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE dsrfrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DeleteSnapshotResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

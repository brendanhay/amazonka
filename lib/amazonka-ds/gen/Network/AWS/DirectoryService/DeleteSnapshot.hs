{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory snapshot.
module Network.AWS.DirectoryService.DeleteSnapshot
    (
    -- * Creating a request
      DeleteSnapshot (..)
    , mkDeleteSnapshot
    -- ** Request lenses
    , dsSnapshotId

    -- * Destructuring the response
    , DeleteSnapshotResponse (..)
    , mkDeleteSnapshotResponse
    -- ** Response lenses
    , dsrfrsSnapshotId
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot'
  { snapshotId :: Types.SnapshotId
    -- ^ The identifier of the directory snapshot to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshot' value with any optional fields omitted.
mkDeleteSnapshot
    :: Types.SnapshotId -- ^ 'snapshotId'
    -> DeleteSnapshot
mkDeleteSnapshot snapshotId = DeleteSnapshot'{snapshotId}

-- | The identifier of the directory snapshot to be deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotId :: Lens.Lens' DeleteSnapshot Types.SnapshotId
dsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

instance Core.ToQuery DeleteSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSnapshot where
        toHeaders DeleteSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DeleteSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSnapshot where
        toJSON DeleteSnapshot{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SnapshotId" Core..= snapshotId)])

instance Core.AWSRequest DeleteSnapshot where
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSnapshotResponse' Core.<$>
                   (x Core..:? "SnapshotId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The identifier of the directory snapshot that was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotResponse' value with any optional fields omitted.
mkDeleteSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSnapshotResponse
mkDeleteSnapshotResponse responseStatus
  = DeleteSnapshotResponse'{snapshotId = Core.Nothing,
                            responseStatus}

-- | The identifier of the directory snapshot that was deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsSnapshotId :: Lens.Lens' DeleteSnapshotResponse (Core.Maybe Types.SnapshotId)
dsrfrsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE dsrfrsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DeleteSnapshotResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

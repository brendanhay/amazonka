{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified disk snapshot.
--
-- When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.
-- The @delete disk snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @disk snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDiskSnapshot
    (
    -- * Creating a request
      DeleteDiskSnapshot (..)
    , mkDeleteDiskSnapshot
    -- ** Request lenses
    , ddsDiskSnapshotName

    -- * Destructuring the response
    , DeleteDiskSnapshotResponse (..)
    , mkDeleteDiskSnapshotResponse
    -- ** Response lenses
    , ddsrrsOperations
    , ddsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDiskSnapshot' smart constructor.
newtype DeleteDiskSnapshot = DeleteDiskSnapshot'
  { diskSnapshotName :: Types.ResourceName
    -- ^ The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDiskSnapshot' value with any optional fields omitted.
mkDeleteDiskSnapshot
    :: Types.ResourceName -- ^ 'diskSnapshotName'
    -> DeleteDiskSnapshot
mkDeleteDiskSnapshot diskSnapshotName
  = DeleteDiskSnapshot'{diskSnapshotName}

-- | The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDiskSnapshotName :: Lens.Lens' DeleteDiskSnapshot Types.ResourceName
ddsDiskSnapshotName = Lens.field @"diskSnapshotName"
{-# INLINEABLE ddsDiskSnapshotName #-}
{-# DEPRECATED diskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead"  #-}

instance Core.ToQuery DeleteDiskSnapshot where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDiskSnapshot where
        toHeaders DeleteDiskSnapshot{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteDiskSnapshot")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDiskSnapshot where
        toJSON DeleteDiskSnapshot{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("diskSnapshotName" Core..= diskSnapshotName)])

instance Core.AWSRequest DeleteDiskSnapshot where
        type Rs DeleteDiskSnapshot = DeleteDiskSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDiskSnapshotResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDiskSnapshotResponse' smart constructor.
data DeleteDiskSnapshotResponse = DeleteDiskSnapshotResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDiskSnapshotResponse' value with any optional fields omitted.
mkDeleteDiskSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDiskSnapshotResponse
mkDeleteDiskSnapshotResponse responseStatus
  = DeleteDiskSnapshotResponse'{operations = Core.Nothing,
                                responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsOperations :: Lens.Lens' DeleteDiskSnapshotResponse (Core.Maybe [Types.Operation])
ddsrrsOperations = Lens.field @"operations"
{-# INLINEABLE ddsrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsResponseStatus :: Lens.Lens' DeleteDiskSnapshotResponse Core.Int
ddsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

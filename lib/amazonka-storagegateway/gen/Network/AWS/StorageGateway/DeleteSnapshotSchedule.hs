{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot of a volume.
--
-- You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/backing-up-volumes.html Backing up your volumes> . In the @DeleteSnapshotSchedule@ request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    (
    -- * Creating a request
      DeleteSnapshotSchedule (..)
    , mkDeleteSnapshotSchedule
    -- ** Request lenses
    , dVolumeARN

    -- * Destructuring the response
    , DeleteSnapshotScheduleResponse (..)
    , mkDeleteSnapshotScheduleResponse
    -- ** Response lenses
    , dssrfrsVolumeARN
    , dssrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkDeleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { volumeARN :: Types.VolumeARN
    -- ^ The volume which snapshot schedule to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotSchedule' value with any optional fields omitted.
mkDeleteSnapshotSchedule
    :: Types.VolumeARN -- ^ 'volumeARN'
    -> DeleteSnapshotSchedule
mkDeleteSnapshotSchedule volumeARN
  = DeleteSnapshotSchedule'{volumeARN}

-- | The volume which snapshot schedule to delete.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVolumeARN :: Lens.Lens' DeleteSnapshotSchedule Types.VolumeARN
dVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE dVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

instance Core.ToQuery DeleteSnapshotSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSnapshotSchedule where
        toHeaders DeleteSnapshotSchedule{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.DeleteSnapshotSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSnapshotSchedule where
        toJSON DeleteSnapshotSchedule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeARN" Core..= volumeARN)])

instance Core.AWSRequest DeleteSnapshotSchedule where
        type Rs DeleteSnapshotSchedule = DeleteSnapshotScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSnapshotScheduleResponse' Core.<$>
                   (x Core..:? "VolumeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
  { volumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The volume which snapshot schedule was deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSnapshotScheduleResponse' value with any optional fields omitted.
mkDeleteSnapshotScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSnapshotScheduleResponse
mkDeleteSnapshotScheduleResponse responseStatus
  = DeleteSnapshotScheduleResponse'{volumeARN = Core.Nothing,
                                    responseStatus}

-- | The volume which snapshot schedule was deleted.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrfrsVolumeARN :: Lens.Lens' DeleteSnapshotScheduleResponse (Core.Maybe Types.VolumeARN)
dssrfrsVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE dssrfrsVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrfrsResponseStatus :: Lens.Lens' DeleteSnapshotScheduleResponse Core.Int
dssrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dssrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

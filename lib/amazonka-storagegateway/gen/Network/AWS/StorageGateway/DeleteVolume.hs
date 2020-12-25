{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified storage volume that you previously created using the 'CreateCachediSCSIVolume' or 'CreateStorediSCSIVolume' API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume.
--
-- Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
-- In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.
module Network.AWS.StorageGateway.DeleteVolume
  ( -- * Creating a request
    DeleteVolume (..),
    mkDeleteVolume,

    -- ** Request lenses
    dvfVolumeARN,

    -- * Destructuring the response
    DeleteVolumeResponse (..),
    mkDeleteVolumeResponse,

    -- ** Response lenses
    dvrfrsVolumeARN,
    dvrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the 'DeleteVolumeInput$VolumeARN' to delete.
--
-- /See:/ 'mkDeleteVolume' smart constructor.
newtype DeleteVolume = DeleteVolume'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Types.VolumeARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVolume' value with any optional fields omitted.
mkDeleteVolume ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  DeleteVolume
mkDeleteVolume volumeARN = DeleteVolume' {volumeARN}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfVolumeARN :: Lens.Lens' DeleteVolume Types.VolumeARN
dvfVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED dvfVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Core.FromJSON DeleteVolume where
  toJSON DeleteVolume {..} =
    Core.object
      (Core.catMaybes [Core.Just ("VolumeARN" Core..= volumeARN)])

instance Core.AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.DeleteVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            Core.<$> (x Core..:? "VolumeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the storage volume that was deleted.
--
-- /See:/ 'mkDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVolumeResponse' value with any optional fields omitted.
mkDeleteVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteVolumeResponse
mkDeleteVolumeResponse responseStatus =
  DeleteVolumeResponse' {volumeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrfrsVolumeARN :: Lens.Lens' DeleteVolumeResponse (Core.Maybe Types.VolumeARN)
dvrfrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED dvrfrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrfrsResponseStatus :: Lens.Lens' DeleteVolumeResponse Core.Int
dvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

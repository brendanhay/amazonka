{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the gateway volumes specified in the request. The list of gateway volumes in the request must be from one gateway. In the response, AWS Storage Gateway returns volume information sorted by volume ARNs. This operation is only supported in stored volume gateway type.
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    (
    -- * Creating a request
      DescribeStorediSCSIVolumes (..)
    , mkDescribeStorediSCSIVolumes
    -- ** Request lenses
    , dsscsivVolumeARNs

    -- * Destructuring the response
    , DescribeStorediSCSIVolumesResponse (..)
    , mkDescribeStorediSCSIVolumesResponse
    -- ** Response lenses
    , dsscsivrrsStorediSCSIVolumes
    , dsscsivrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing a list of 'DescribeStorediSCSIVolumesInput$VolumeARNs' .
--
-- /See:/ 'mkDescribeStorediSCSIVolumes' smart constructor.
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
  { volumeARNs :: [Types.VolumeARN]
    -- ^ An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStorediSCSIVolumes' value with any optional fields omitted.
mkDescribeStorediSCSIVolumes
    :: DescribeStorediSCSIVolumes
mkDescribeStorediSCSIVolumes
  = DescribeStorediSCSIVolumes'{volumeARNs = Core.mempty}

-- | An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
--
-- /Note:/ Consider using 'volumeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscsivVolumeARNs :: Lens.Lens' DescribeStorediSCSIVolumes [Types.VolumeARN]
dsscsivVolumeARNs = Lens.field @"volumeARNs"
{-# INLINEABLE dsscsivVolumeARNs #-}
{-# DEPRECATED volumeARNs "Use generic-lens or generic-optics with 'volumeARNs' instead"  #-}

instance Core.ToQuery DescribeStorediSCSIVolumes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStorediSCSIVolumes where
        toHeaders DescribeStorediSCSIVolumes{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.DescribeStorediSCSIVolumes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStorediSCSIVolumes where
        toJSON DescribeStorediSCSIVolumes{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeARNs" Core..= volumeARNs)])

instance Core.AWSRequest DescribeStorediSCSIVolumes where
        type Rs DescribeStorediSCSIVolumes =
             DescribeStorediSCSIVolumesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStorediSCSIVolumesResponse' Core.<$>
                   (x Core..:? "StorediSCSIVolumes") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeStorediSCSIVolumesResponse' smart constructor.
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
  { storediSCSIVolumes :: Core.Maybe [Types.StorediSCSIVolume]
    -- ^ Describes a single unit of output from 'DescribeStorediSCSIVolumes' . The following fields are returned:
--
--
--     * @ChapEnabled@ : Indicates whether mutual CHAP is enabled for the iSCSI target.
--
--
--     * @LunNumber@ : The logical disk number.
--
--
--     * @NetworkInterfaceId@ : The network interface ID of the stored volume that initiator use to map the stored volume as an iSCSI target.
--
--
--     * @NetworkInterfacePort@ : The port used to communicate with iSCSI targets.
--
--
--     * @PreservedExistingData@ : Indicates when the stored volume was created, existing data on the underlying local disk was preserved.
--
--
--     * @SourceSnapshotId@ : If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. @snap-1122aabb@ . Otherwise, this field is not included.
--
--
--     * @StorediSCSIVolumes@ : An array of StorediSCSIVolume objects where each object contains metadata about one stored volume.
--
--
--     * @TargetARN@ : The Amazon Resource Name (ARN) of the volume target.
--
--
--     * @VolumeARN@ : The Amazon Resource Name (ARN) of the stored volume.
--
--
--     * @VolumeDiskId@ : The disk ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
--
--     * @VolumeId@ : The unique identifier of the storage volume, e.g. @vol-1122AABB@ .
--
--
--     * @VolumeiSCSIAttributes@ : An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
--
--     * @VolumeProgress@ : Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
--
--     * @VolumeSizeInBytes@ : The size of the volume in bytes.
--
--
--     * @VolumeStatus@ : One of the @VolumeStatus@ values that indicates the state of the volume.
--
--
--     * @VolumeType@ : One of the enumeration values describing the type of the volume. Currently, only @STORED@ volumes are supported.
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeStorediSCSIVolumesResponse' value with any optional fields omitted.
mkDescribeStorediSCSIVolumesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStorediSCSIVolumesResponse
mkDescribeStorediSCSIVolumesResponse responseStatus
  = DescribeStorediSCSIVolumesResponse'{storediSCSIVolumes =
                                          Core.Nothing,
                                        responseStatus}

-- | Describes a single unit of output from 'DescribeStorediSCSIVolumes' . The following fields are returned:
--
--
--     * @ChapEnabled@ : Indicates whether mutual CHAP is enabled for the iSCSI target.
--
--
--     * @LunNumber@ : The logical disk number.
--
--
--     * @NetworkInterfaceId@ : The network interface ID of the stored volume that initiator use to map the stored volume as an iSCSI target.
--
--
--     * @NetworkInterfacePort@ : The port used to communicate with iSCSI targets.
--
--
--     * @PreservedExistingData@ : Indicates when the stored volume was created, existing data on the underlying local disk was preserved.
--
--
--     * @SourceSnapshotId@ : If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. @snap-1122aabb@ . Otherwise, this field is not included.
--
--
--     * @StorediSCSIVolumes@ : An array of StorediSCSIVolume objects where each object contains metadata about one stored volume.
--
--
--     * @TargetARN@ : The Amazon Resource Name (ARN) of the volume target.
--
--
--     * @VolumeARN@ : The Amazon Resource Name (ARN) of the stored volume.
--
--
--     * @VolumeDiskId@ : The disk ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
--
--     * @VolumeId@ : The unique identifier of the storage volume, e.g. @vol-1122AABB@ .
--
--
--     * @VolumeiSCSIAttributes@ : An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
--
--     * @VolumeProgress@ : Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
--
--     * @VolumeSizeInBytes@ : The size of the volume in bytes.
--
--
--     * @VolumeStatus@ : One of the @VolumeStatus@ values that indicates the state of the volume.
--
--
--     * @VolumeType@ : One of the enumeration values describing the type of the volume. Currently, only @STORED@ volumes are supported.
--
--
--
-- /Note:/ Consider using 'storediSCSIVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscsivrrsStorediSCSIVolumes :: Lens.Lens' DescribeStorediSCSIVolumesResponse (Core.Maybe [Types.StorediSCSIVolume])
dsscsivrrsStorediSCSIVolumes = Lens.field @"storediSCSIVolumes"
{-# INLINEABLE dsscsivrrsStorediSCSIVolumes #-}
{-# DEPRECATED storediSCSIVolumes "Use generic-lens or generic-optics with 'storediSCSIVolumes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscsivrrsResponseStatus :: Lens.Lens' DescribeStorediSCSIVolumesResponse Core.Int
dsscsivrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsscsivrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

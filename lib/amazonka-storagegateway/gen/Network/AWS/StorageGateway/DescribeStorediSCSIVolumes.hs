{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeStorediSCSIVolumes (..),
    mkDescribeStorediSCSIVolumes,

    -- ** Request lenses
    dsscsivVolumeARNs,

    -- * Destructuring the response
    DescribeStorediSCSIVolumesResponse (..),
    mkDescribeStorediSCSIVolumesResponse,

    -- ** Response lenses
    dsscsivrsStorediSCSIVolumes,
    dsscsivrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing a list of 'DescribeStorediSCSIVolumesInput$VolumeARNs' .
--
-- /See:/ 'mkDescribeStorediSCSIVolumes' smart constructor.
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
  { volumeARNs ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStorediSCSIVolumes' with the minimum fields required to make a request.
--
-- * 'volumeARNs' - An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
mkDescribeStorediSCSIVolumes ::
  DescribeStorediSCSIVolumes
mkDescribeStorediSCSIVolumes =
  DescribeStorediSCSIVolumes' {volumeARNs = Lude.mempty}

-- | An array of strings where each string represents the Amazon Resource Name (ARN) of a stored volume. All of the specified stored volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
--
-- /Note:/ Consider using 'volumeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscsivVolumeARNs :: Lens.Lens' DescribeStorediSCSIVolumes [Lude.Text]
dsscsivVolumeARNs = Lens.lens (volumeARNs :: DescribeStorediSCSIVolumes -> [Lude.Text]) (\s a -> s {volumeARNs = a} :: DescribeStorediSCSIVolumes)
{-# DEPRECATED dsscsivVolumeARNs "Use generic-lens or generic-optics with 'volumeARNs' instead." #-}

instance Lude.AWSRequest DescribeStorediSCSIVolumes where
  type
    Rs DescribeStorediSCSIVolumes =
      DescribeStorediSCSIVolumesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStorediSCSIVolumesResponse'
            Lude.<$> (x Lude..?> "StorediSCSIVolumes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStorediSCSIVolumes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeStorediSCSIVolumes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStorediSCSIVolumes where
  toJSON DescribeStorediSCSIVolumes' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARNs" Lude..= volumeARNs)])

instance Lude.ToPath DescribeStorediSCSIVolumes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStorediSCSIVolumes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStorediSCSIVolumesResponse' smart constructor.
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
  { storediSCSIVolumes ::
      Lude.Maybe
        [StorediSCSIVolume],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStorediSCSIVolumesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'storediSCSIVolumes' - Describes a single unit of output from 'DescribeStorediSCSIVolumes' . The following fields are returned:
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
mkDescribeStorediSCSIVolumesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStorediSCSIVolumesResponse
mkDescribeStorediSCSIVolumesResponse pResponseStatus_ =
  DescribeStorediSCSIVolumesResponse'
    { storediSCSIVolumes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

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
dsscsivrsStorediSCSIVolumes :: Lens.Lens' DescribeStorediSCSIVolumesResponse (Lude.Maybe [StorediSCSIVolume])
dsscsivrsStorediSCSIVolumes = Lens.lens (storediSCSIVolumes :: DescribeStorediSCSIVolumesResponse -> Lude.Maybe [StorediSCSIVolume]) (\s a -> s {storediSCSIVolumes = a} :: DescribeStorediSCSIVolumesResponse)
{-# DEPRECATED dsscsivrsStorediSCSIVolumes "Use generic-lens or generic-optics with 'storediSCSIVolumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsscsivrsResponseStatus :: Lens.Lens' DescribeStorediSCSIVolumesResponse Lude.Int
dsscsivrsResponseStatus = Lens.lens (responseStatus :: DescribeStorediSCSIVolumesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStorediSCSIVolumesResponse)
{-# DEPRECATED dsscsivrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

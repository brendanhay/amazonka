{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the gateway volumes specified in the request.
-- The list of gateway volumes in the request must be from one gateway. In
-- the response, AWS Storage Gateway returns volume information sorted by
-- volume ARNs. This operation is only supported in stored volume gateway
-- type.
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
  ( -- * Creating a Request
    DescribeStorediSCSIVolumes (..),
    newDescribeStorediSCSIVolumes,

    -- * Request Lenses
    describeStorediSCSIVolumes_volumeARNs,

    -- * Destructuring the Response
    DescribeStorediSCSIVolumesResponse (..),
    newDescribeStorediSCSIVolumesResponse,

    -- * Response Lenses
    describeStorediSCSIVolumesResponse_storediSCSIVolumes,
    describeStorediSCSIVolumesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing a list of
-- DescribeStorediSCSIVolumesInput$VolumeARNs.
--
-- /See:/ 'newDescribeStorediSCSIVolumes' smart constructor.
data DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
  { -- | An array of strings where each string represents the Amazon Resource
    -- Name (ARN) of a stored volume. All of the specified stored volumes must
    -- be from the same gateway. Use ListVolumes to get volume ARNs for a
    -- gateway.
    volumeARNs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStorediSCSIVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARNs', 'describeStorediSCSIVolumes_volumeARNs' - An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a stored volume. All of the specified stored volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
newDescribeStorediSCSIVolumes ::
  DescribeStorediSCSIVolumes
newDescribeStorediSCSIVolumes =
  DescribeStorediSCSIVolumes'
    { volumeARNs =
        Core.mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a stored volume. All of the specified stored volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
describeStorediSCSIVolumes_volumeARNs :: Lens.Lens' DescribeStorediSCSIVolumes [Core.Text]
describeStorediSCSIVolumes_volumeARNs = Lens.lens (\DescribeStorediSCSIVolumes' {volumeARNs} -> volumeARNs) (\s@DescribeStorediSCSIVolumes' {} a -> s {volumeARNs = a} :: DescribeStorediSCSIVolumes) Core.. Lens._Coerce

instance Core.AWSRequest DescribeStorediSCSIVolumes where
  type
    AWSResponse DescribeStorediSCSIVolumes =
      DescribeStorediSCSIVolumesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorediSCSIVolumesResponse'
            Core.<$> ( x Core..?> "StorediSCSIVolumes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStorediSCSIVolumes

instance Core.NFData DescribeStorediSCSIVolumes

instance Core.ToHeaders DescribeStorediSCSIVolumes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeStorediSCSIVolumes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeStorediSCSIVolumes where
  toJSON DescribeStorediSCSIVolumes' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("VolumeARNs" Core..= volumeARNs)]
      )

instance Core.ToPath DescribeStorediSCSIVolumes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStorediSCSIVolumes where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeStorediSCSIVolumesResponse' smart constructor.
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
  { -- | Describes a single unit of output from DescribeStorediSCSIVolumes. The
    -- following fields are returned:
    --
    -- -   @ChapEnabled@: Indicates whether mutual CHAP is enabled for the
    --     iSCSI target.
    --
    -- -   @LunNumber@: The logical disk number.
    --
    -- -   @NetworkInterfaceId@: The network interface ID of the stored volume
    --     that initiator use to map the stored volume as an iSCSI target.
    --
    -- -   @NetworkInterfacePort@: The port used to communicate with iSCSI
    --     targets.
    --
    -- -   @PreservedExistingData@: Indicates when the stored volume was
    --     created, existing data on the underlying local disk was preserved.
    --
    -- -   @SourceSnapshotId@: If the stored volume was created from a
    --     snapshot, this field contains the snapshot ID used, e.g.
    --     @snap-1122aabb@. Otherwise, this field is not included.
    --
    -- -   @StorediSCSIVolumes@: An array of StorediSCSIVolume objects where
    --     each object contains metadata about one stored volume.
    --
    -- -   @TargetARN@: The Amazon Resource Name (ARN) of the volume target.
    --
    -- -   @VolumeARN@: The Amazon Resource Name (ARN) of the stored volume.
    --
    -- -   @VolumeDiskId@: The disk ID of the local disk that was specified in
    --     the CreateStorediSCSIVolume operation.
    --
    -- -   @VolumeId@: The unique identifier of the storage volume, e.g.
    --     @vol-1122AABB@.
    --
    -- -   @VolumeiSCSIAttributes@: An VolumeiSCSIAttributes object that
    --     represents a collection of iSCSI attributes for one stored volume.
    --
    -- -   @VolumeProgress@: Represents the percentage complete if the volume
    --     is restoring or bootstrapping that represents the percent of data
    --     transferred. This field does not appear in the response if the
    --     stored volume is not restoring or bootstrapping.
    --
    -- -   @VolumeSizeInBytes@: The size of the volume in bytes.
    --
    -- -   @VolumeStatus@: One of the @VolumeStatus@ values that indicates the
    --     state of the volume.
    --
    -- -   @VolumeType@: One of the enumeration values describing the type of
    --     the volume. Currently, only @STORED@ volumes are supported.
    storediSCSIVolumes :: Core.Maybe [StorediSCSIVolume],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStorediSCSIVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storediSCSIVolumes', 'describeStorediSCSIVolumesResponse_storediSCSIVolumes' - Describes a single unit of output from DescribeStorediSCSIVolumes. The
-- following fields are returned:
--
-- -   @ChapEnabled@: Indicates whether mutual CHAP is enabled for the
--     iSCSI target.
--
-- -   @LunNumber@: The logical disk number.
--
-- -   @NetworkInterfaceId@: The network interface ID of the stored volume
--     that initiator use to map the stored volume as an iSCSI target.
--
-- -   @NetworkInterfacePort@: The port used to communicate with iSCSI
--     targets.
--
-- -   @PreservedExistingData@: Indicates when the stored volume was
--     created, existing data on the underlying local disk was preserved.
--
-- -   @SourceSnapshotId@: If the stored volume was created from a
--     snapshot, this field contains the snapshot ID used, e.g.
--     @snap-1122aabb@. Otherwise, this field is not included.
--
-- -   @StorediSCSIVolumes@: An array of StorediSCSIVolume objects where
--     each object contains metadata about one stored volume.
--
-- -   @TargetARN@: The Amazon Resource Name (ARN) of the volume target.
--
-- -   @VolumeARN@: The Amazon Resource Name (ARN) of the stored volume.
--
-- -   @VolumeDiskId@: The disk ID of the local disk that was specified in
--     the CreateStorediSCSIVolume operation.
--
-- -   @VolumeId@: The unique identifier of the storage volume, e.g.
--     @vol-1122AABB@.
--
-- -   @VolumeiSCSIAttributes@: An VolumeiSCSIAttributes object that
--     represents a collection of iSCSI attributes for one stored volume.
--
-- -   @VolumeProgress@: Represents the percentage complete if the volume
--     is restoring or bootstrapping that represents the percent of data
--     transferred. This field does not appear in the response if the
--     stored volume is not restoring or bootstrapping.
--
-- -   @VolumeSizeInBytes@: The size of the volume in bytes.
--
-- -   @VolumeStatus@: One of the @VolumeStatus@ values that indicates the
--     state of the volume.
--
-- -   @VolumeType@: One of the enumeration values describing the type of
--     the volume. Currently, only @STORED@ volumes are supported.
--
-- 'httpStatus', 'describeStorediSCSIVolumesResponse_httpStatus' - The response's http status code.
newDescribeStorediSCSIVolumesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStorediSCSIVolumesResponse
newDescribeStorediSCSIVolumesResponse pHttpStatus_ =
  DescribeStorediSCSIVolumesResponse'
    { storediSCSIVolumes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes a single unit of output from DescribeStorediSCSIVolumes. The
-- following fields are returned:
--
-- -   @ChapEnabled@: Indicates whether mutual CHAP is enabled for the
--     iSCSI target.
--
-- -   @LunNumber@: The logical disk number.
--
-- -   @NetworkInterfaceId@: The network interface ID of the stored volume
--     that initiator use to map the stored volume as an iSCSI target.
--
-- -   @NetworkInterfacePort@: The port used to communicate with iSCSI
--     targets.
--
-- -   @PreservedExistingData@: Indicates when the stored volume was
--     created, existing data on the underlying local disk was preserved.
--
-- -   @SourceSnapshotId@: If the stored volume was created from a
--     snapshot, this field contains the snapshot ID used, e.g.
--     @snap-1122aabb@. Otherwise, this field is not included.
--
-- -   @StorediSCSIVolumes@: An array of StorediSCSIVolume objects where
--     each object contains metadata about one stored volume.
--
-- -   @TargetARN@: The Amazon Resource Name (ARN) of the volume target.
--
-- -   @VolumeARN@: The Amazon Resource Name (ARN) of the stored volume.
--
-- -   @VolumeDiskId@: The disk ID of the local disk that was specified in
--     the CreateStorediSCSIVolume operation.
--
-- -   @VolumeId@: The unique identifier of the storage volume, e.g.
--     @vol-1122AABB@.
--
-- -   @VolumeiSCSIAttributes@: An VolumeiSCSIAttributes object that
--     represents a collection of iSCSI attributes for one stored volume.
--
-- -   @VolumeProgress@: Represents the percentage complete if the volume
--     is restoring or bootstrapping that represents the percent of data
--     transferred. This field does not appear in the response if the
--     stored volume is not restoring or bootstrapping.
--
-- -   @VolumeSizeInBytes@: The size of the volume in bytes.
--
-- -   @VolumeStatus@: One of the @VolumeStatus@ values that indicates the
--     state of the volume.
--
-- -   @VolumeType@: One of the enumeration values describing the type of
--     the volume. Currently, only @STORED@ volumes are supported.
describeStorediSCSIVolumesResponse_storediSCSIVolumes :: Lens.Lens' DescribeStorediSCSIVolumesResponse (Core.Maybe [StorediSCSIVolume])
describeStorediSCSIVolumesResponse_storediSCSIVolumes = Lens.lens (\DescribeStorediSCSIVolumesResponse' {storediSCSIVolumes} -> storediSCSIVolumes) (\s@DescribeStorediSCSIVolumesResponse' {} a -> s {storediSCSIVolumes = a} :: DescribeStorediSCSIVolumesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStorediSCSIVolumesResponse_httpStatus :: Lens.Lens' DescribeStorediSCSIVolumesResponse Core.Int
describeStorediSCSIVolumesResponse_httpStatus = Lens.lens (\DescribeStorediSCSIVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeStorediSCSIVolumesResponse' {} a -> s {httpStatus = a} :: DescribeStorediSCSIVolumesResponse)

instance
  Core.NFData
    DescribeStorediSCSIVolumesResponse

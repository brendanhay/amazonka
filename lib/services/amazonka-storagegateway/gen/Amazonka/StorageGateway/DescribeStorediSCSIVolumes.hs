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
-- Module      : Amazonka.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the gateway volumes specified in the request.
-- The list of gateway volumes in the request must be from one gateway. In
-- the response, Storage Gateway returns volume information sorted by
-- volume ARNs. This operation is only supported in stored volume gateway
-- type.
module Amazonka.StorageGateway.DescribeStorediSCSIVolumes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing a list of
-- DescribeStorediSCSIVolumesInput$VolumeARNs.
--
-- /See:/ 'newDescribeStorediSCSIVolumes' smart constructor.
data DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
  { -- | An array of strings where each string represents the Amazon Resource
    -- Name (ARN) of a stored volume. All of the specified stored volumes must
    -- be from the same gateway. Use ListVolumes to get volume ARNs for a
    -- gateway.
    volumeARNs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a stored volume. All of the specified stored volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
describeStorediSCSIVolumes_volumeARNs :: Lens.Lens' DescribeStorediSCSIVolumes [Prelude.Text]
describeStorediSCSIVolumes_volumeARNs = Lens.lens (\DescribeStorediSCSIVolumes' {volumeARNs} -> volumeARNs) (\s@DescribeStorediSCSIVolumes' {} a -> s {volumeARNs = a} :: DescribeStorediSCSIVolumes) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeStorediSCSIVolumes where
  type
    AWSResponse DescribeStorediSCSIVolumes =
      DescribeStorediSCSIVolumesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorediSCSIVolumesResponse'
            Prelude.<$> ( x Data..?> "StorediSCSIVolumes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStorediSCSIVolumes where
  hashWithSalt _salt DescribeStorediSCSIVolumes' {..} =
    _salt `Prelude.hashWithSalt` volumeARNs

instance Prelude.NFData DescribeStorediSCSIVolumes where
  rnf DescribeStorediSCSIVolumes' {..} =
    Prelude.rnf volumeARNs

instance Data.ToHeaders DescribeStorediSCSIVolumes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeStorediSCSIVolumes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStorediSCSIVolumes where
  toJSON DescribeStorediSCSIVolumes' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARNs" Data..= volumeARNs)]
      )

instance Data.ToPath DescribeStorediSCSIVolumes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStorediSCSIVolumes where
  toQuery = Prelude.const Prelude.mempty

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
    storediSCSIVolumes :: Prelude.Maybe [StorediSCSIVolume],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeStorediSCSIVolumesResponse
newDescribeStorediSCSIVolumesResponse pHttpStatus_ =
  DescribeStorediSCSIVolumesResponse'
    { storediSCSIVolumes =
        Prelude.Nothing,
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
describeStorediSCSIVolumesResponse_storediSCSIVolumes :: Lens.Lens' DescribeStorediSCSIVolumesResponse (Prelude.Maybe [StorediSCSIVolume])
describeStorediSCSIVolumesResponse_storediSCSIVolumes = Lens.lens (\DescribeStorediSCSIVolumesResponse' {storediSCSIVolumes} -> storediSCSIVolumes) (\s@DescribeStorediSCSIVolumesResponse' {} a -> s {storediSCSIVolumes = a} :: DescribeStorediSCSIVolumesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStorediSCSIVolumesResponse_httpStatus :: Lens.Lens' DescribeStorediSCSIVolumesResponse Prelude.Int
describeStorediSCSIVolumesResponse_httpStatus = Lens.lens (\DescribeStorediSCSIVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeStorediSCSIVolumesResponse' {} a -> s {httpStatus = a} :: DescribeStorediSCSIVolumesResponse)

instance
  Prelude.NFData
    DescribeStorediSCSIVolumesResponse
  where
  rnf DescribeStorediSCSIVolumesResponse' {..} =
    Prelude.rnf storediSCSIVolumes
      `Prelude.seq` Prelude.rnf httpStatus

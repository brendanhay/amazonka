{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a gateway from a volume recovery point. This
-- operation is only supported in the cached volume gateway type.
--
-- A volume recovery point is a point in time at which all data of the
-- volume is consistent and from which you can create a snapshot. To get a
-- list of volume recovery point for cached volume gateway, use
-- ListVolumeRecoveryPoints.
--
-- In the @CreateSnapshotFromVolumeRecoveryPoint@ request, you identify the
-- volume by providing its Amazon Resource Name (ARN). You must also
-- provide a description for the snapshot. When the gateway takes a
-- snapshot of the specified volume, the snapshot and its description
-- appear in the AWS Storage Gateway console. In response, the gateway
-- returns you a snapshot ID. You can use this snapshot ID to check the
-- snapshot progress or later use it when you want to create a volume from
-- a snapshot.
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSnapshots.html DescribeSnapshots>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteSnapshot.html DeleteSnapshot>
-- in the /Amazon Elastic Compute Cloud API Reference/.
module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
  ( -- * Creating a Request
    CreateSnapshotFromVolumeRecoveryPoint (..),
    newCreateSnapshotFromVolumeRecoveryPoint,

    -- * Request Lenses
    createSnapshotFromVolumeRecoveryPoint_tags,
    createSnapshotFromVolumeRecoveryPoint_volumeARN,
    createSnapshotFromVolumeRecoveryPoint_snapshotDescription,

    -- * Destructuring the Response
    CreateSnapshotFromVolumeRecoveryPointResponse (..),
    newCreateSnapshotFromVolumeRecoveryPointResponse,

    -- * Response Lenses
    createSnapshotFromVolumeRecoveryPointResponse_volumeARN,
    createSnapshotFromVolumeRecoveryPointResponse_snapshotId,
    createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime,
    createSnapshotFromVolumeRecoveryPointResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newCreateSnapshotFromVolumeRecoveryPoint' smart constructor.
data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint'
  { -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
    -- a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
    -- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
    -- for specified VolumeARN.
    volumeARN :: Prelude.Text,
    -- | Textual description of the snapshot that appears in the Amazon EC2
    -- console, Elastic Block Store snapshots panel in the __Description__
    -- field, and in the AWS Storage Gateway snapshot __Details__ pane,
    -- __Description__ field.
    snapshotDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotFromVolumeRecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSnapshotFromVolumeRecoveryPoint_tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'volumeARN', 'createSnapshotFromVolumeRecoveryPoint_volumeARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
--
-- 'snapshotDescription', 'createSnapshotFromVolumeRecoveryPoint_snapshotDescription' - Textual description of the snapshot that appears in the Amazon EC2
-- console, Elastic Block Store snapshots panel in the __Description__
-- field, and in the AWS Storage Gateway snapshot __Details__ pane,
-- __Description__ field.
newCreateSnapshotFromVolumeRecoveryPoint ::
  -- | 'volumeARN'
  Prelude.Text ->
  -- | 'snapshotDescription'
  Prelude.Text ->
  CreateSnapshotFromVolumeRecoveryPoint
newCreateSnapshotFromVolumeRecoveryPoint
  pVolumeARN_
  pSnapshotDescription_ =
    CreateSnapshotFromVolumeRecoveryPoint'
      { tags =
          Prelude.Nothing,
        volumeARN = pVolumeARN_,
        snapshotDescription =
          pSnapshotDescription_
      }

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createSnapshotFromVolumeRecoveryPoint_tags :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint (Prelude.Maybe [Tag])
createSnapshotFromVolumeRecoveryPoint_tags = Lens.lens (\CreateSnapshotFromVolumeRecoveryPoint' {tags} -> tags) (\s@CreateSnapshotFromVolumeRecoveryPoint' {} a -> s {tags = a} :: CreateSnapshotFromVolumeRecoveryPoint) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
createSnapshotFromVolumeRecoveryPoint_volumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Prelude.Text
createSnapshotFromVolumeRecoveryPoint_volumeARN = Lens.lens (\CreateSnapshotFromVolumeRecoveryPoint' {volumeARN} -> volumeARN) (\s@CreateSnapshotFromVolumeRecoveryPoint' {} a -> s {volumeARN = a} :: CreateSnapshotFromVolumeRecoveryPoint)

-- | Textual description of the snapshot that appears in the Amazon EC2
-- console, Elastic Block Store snapshots panel in the __Description__
-- field, and in the AWS Storage Gateway snapshot __Details__ pane,
-- __Description__ field.
createSnapshotFromVolumeRecoveryPoint_snapshotDescription :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPoint Prelude.Text
createSnapshotFromVolumeRecoveryPoint_snapshotDescription = Lens.lens (\CreateSnapshotFromVolumeRecoveryPoint' {snapshotDescription} -> snapshotDescription) (\s@CreateSnapshotFromVolumeRecoveryPoint' {} a -> s {snapshotDescription = a} :: CreateSnapshotFromVolumeRecoveryPoint)

instance
  Prelude.AWSRequest
    CreateSnapshotFromVolumeRecoveryPoint
  where
  type
    Rs CreateSnapshotFromVolumeRecoveryPoint =
      CreateSnapshotFromVolumeRecoveryPointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotFromVolumeRecoveryPointResponse'
            Prelude.<$> (x Prelude..?> "VolumeARN")
              Prelude.<*> (x Prelude..?> "SnapshotId")
              Prelude.<*> (x Prelude..?> "VolumeRecoveryPointTime")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSnapshotFromVolumeRecoveryPoint

instance
  Prelude.NFData
    CreateSnapshotFromVolumeRecoveryPoint

instance
  Prelude.ToHeaders
    CreateSnapshotFromVolumeRecoveryPoint
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.CreateSnapshotFromVolumeRecoveryPoint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    CreateSnapshotFromVolumeRecoveryPoint
  where
  toJSON CreateSnapshotFromVolumeRecoveryPoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("VolumeARN" Prelude..= volumeARN),
            Prelude.Just
              ( "SnapshotDescription"
                  Prelude..= snapshotDescription
              )
          ]
      )

instance
  Prelude.ToPath
    CreateSnapshotFromVolumeRecoveryPoint
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateSnapshotFromVolumeRecoveryPoint
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSnapshotFromVolumeRecoveryPointResponse' smart constructor.
data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse'
  { -- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
    -- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
    -- for specified VolumeARN.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The time the volume was created from the recovery point.
    volumeRecoveryPointTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotFromVolumeRecoveryPointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'createSnapshotFromVolumeRecoveryPointResponse_volumeARN' - The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
--
-- 'snapshotId', 'createSnapshotFromVolumeRecoveryPointResponse_snapshotId' - The ID of the snapshot.
--
-- 'volumeRecoveryPointTime', 'createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime' - The time the volume was created from the recovery point.
--
-- 'httpStatus', 'createSnapshotFromVolumeRecoveryPointResponse_httpStatus' - The response's http status code.
newCreateSnapshotFromVolumeRecoveryPointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotFromVolumeRecoveryPointResponse
newCreateSnapshotFromVolumeRecoveryPointResponse
  pHttpStatus_ =
    CreateSnapshotFromVolumeRecoveryPointResponse'
      { volumeARN =
          Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        volumeRecoveryPointTime =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
createSnapshotFromVolumeRecoveryPointResponse_volumeARN :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
createSnapshotFromVolumeRecoveryPointResponse_volumeARN = Lens.lens (\CreateSnapshotFromVolumeRecoveryPointResponse' {volumeARN} -> volumeARN) (\s@CreateSnapshotFromVolumeRecoveryPointResponse' {} a -> s {volumeARN = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)

-- | The ID of the snapshot.
createSnapshotFromVolumeRecoveryPointResponse_snapshotId :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
createSnapshotFromVolumeRecoveryPointResponse_snapshotId = Lens.lens (\CreateSnapshotFromVolumeRecoveryPointResponse' {snapshotId} -> snapshotId) (\s@CreateSnapshotFromVolumeRecoveryPointResponse' {} a -> s {snapshotId = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)

-- | The time the volume was created from the recovery point.
createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Prelude.Maybe Prelude.Text)
createSnapshotFromVolumeRecoveryPointResponse_volumeRecoveryPointTime = Lens.lens (\CreateSnapshotFromVolumeRecoveryPointResponse' {volumeRecoveryPointTime} -> volumeRecoveryPointTime) (\s@CreateSnapshotFromVolumeRecoveryPointResponse' {} a -> s {volumeRecoveryPointTime = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)

-- | The response's http status code.
createSnapshotFromVolumeRecoveryPointResponse_httpStatus :: Lens.Lens' CreateSnapshotFromVolumeRecoveryPointResponse Prelude.Int
createSnapshotFromVolumeRecoveryPointResponse_httpStatus = Lens.lens (\CreateSnapshotFromVolumeRecoveryPointResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotFromVolumeRecoveryPointResponse' {} a -> s {httpStatus = a} :: CreateSnapshotFromVolumeRecoveryPointResponse)

instance
  Prelude.NFData
    CreateSnapshotFromVolumeRecoveryPointResponse

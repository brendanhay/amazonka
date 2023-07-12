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
-- Module      : Amazonka.StorageGateway.CreateSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a volume.
--
-- Storage Gateway provides the ability to back up point-in-time snapshots
-- of your data to Amazon Simple Storage (Amazon S3) for durable off-site
-- recovery, and also import the data to an Amazon Elastic Block Store
-- (EBS) volume in Amazon Elastic Compute Cloud (EC2). You can take
-- snapshots of your gateway volume on a scheduled or ad hoc basis. This
-- API enables you to take an ad hoc snapshot. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#SchedulingSnapshot Editing a snapshot schedule>.
--
-- In the @CreateSnapshot@ request, you identify the volume by providing
-- its Amazon Resource Name (ARN). You must also provide description for
-- the snapshot. When Storage Gateway takes the snapshot of specified
-- volume, the snapshot and description appears in the Storage Gateway
-- console. In response, Storage Gateway returns you a snapshot ID. You can
-- use this snapshot ID to check the snapshot progress or later use it when
-- you want to create a volume from a snapshot. This operation is only
-- supported in stored and cached volume gateway type.
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeSnapshots.html DescribeSnapshots>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteSnapshot.html DeleteSnapshot>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- Volume and snapshot IDs are changing to a longer length ID format. For
-- more information, see the important note on the
-- <https://docs.aws.amazon.com/storagegateway/latest/APIReference/Welcome.html Welcome>
-- page.
module Amazonka.StorageGateway.CreateSnapshot
  ( -- * Creating a Request
    CreateSnapshot (..),
    newCreateSnapshot,

    -- * Request Lenses
    createSnapshot_tags,
    createSnapshot_volumeARN,
    createSnapshot_snapshotDescription,

    -- * Destructuring the Response
    CreateSnapshotResponse (..),
    newCreateSnapshotResponse,

    -- * Response Lenses
    createSnapshotResponse_snapshotId,
    createSnapshotResponse_volumeARN,
    createSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   CreateSnapshotInput$SnapshotDescription
--
-- -   CreateSnapshotInput$VolumeARN
--
-- /See:/ 'newCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
    -- a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
    -- operation to return a list of gateway volumes.
    volumeARN :: Prelude.Text,
    -- | Textual description of the snapshot that appears in the Amazon EC2
    -- console, Elastic Block Store snapshots panel in the __Description__
    -- field, and in the Storage Gateway snapshot __Details__ pane,
    -- __Description__ field.
    snapshotDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSnapshot_tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'volumeARN', 'createSnapshot_volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
--
-- 'snapshotDescription', 'createSnapshot_snapshotDescription' - Textual description of the snapshot that appears in the Amazon EC2
-- console, Elastic Block Store snapshots panel in the __Description__
-- field, and in the Storage Gateway snapshot __Details__ pane,
-- __Description__ field.
newCreateSnapshot ::
  -- | 'volumeARN'
  Prelude.Text ->
  -- | 'snapshotDescription'
  Prelude.Text ->
  CreateSnapshot
newCreateSnapshot pVolumeARN_ pSnapshotDescription_ =
  CreateSnapshot'
    { tags = Prelude.Nothing,
      volumeARN = pVolumeARN_,
      snapshotDescription = pSnapshotDescription_
    }

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is
-- a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createSnapshot_tags :: Lens.Lens' CreateSnapshot (Prelude.Maybe [Tag])
createSnapshot_tags = Lens.lens (\CreateSnapshot' {tags} -> tags) (\s@CreateSnapshot' {} a -> s {tags = a} :: CreateSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
createSnapshot_volumeARN :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_volumeARN = Lens.lens (\CreateSnapshot' {volumeARN} -> volumeARN) (\s@CreateSnapshot' {} a -> s {volumeARN = a} :: CreateSnapshot)

-- | Textual description of the snapshot that appears in the Amazon EC2
-- console, Elastic Block Store snapshots panel in the __Description__
-- field, and in the Storage Gateway snapshot __Details__ pane,
-- __Description__ field.
createSnapshot_snapshotDescription :: Lens.Lens' CreateSnapshot Prelude.Text
createSnapshot_snapshotDescription = Lens.lens (\CreateSnapshot' {snapshotDescription} -> snapshotDescription) (\s@CreateSnapshot' {} a -> s {snapshotDescription = a} :: CreateSnapshot)

instance Core.AWSRequest CreateSnapshot where
  type
    AWSResponse CreateSnapshot =
      CreateSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSnapshotResponse'
            Prelude.<$> (x Data..?> "SnapshotId")
            Prelude.<*> (x Data..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSnapshot where
  hashWithSalt _salt CreateSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` snapshotDescription

instance Prelude.NFData CreateSnapshot where
  rnf CreateSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf snapshotDescription

instance Data.ToHeaders CreateSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CreateSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSnapshot where
  toJSON CreateSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("VolumeARN" Data..= volumeARN),
            Prelude.Just
              ("SnapshotDescription" Data..= snapshotDescription)
          ]
      )

instance Data.ToPath CreateSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { -- | The snapshot ID that is used to refer to the snapshot in future
    -- operations such as describing snapshots (Amazon Elastic Compute Cloud
    -- API @DescribeSnapshots@) or creating a volume from a snapshot
    -- (CreateStorediSCSIVolume).
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the volume of which the snapshot was
    -- taken.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'createSnapshotResponse_snapshotId' - The snapshot ID that is used to refer to the snapshot in future
-- operations such as describing snapshots (Amazon Elastic Compute Cloud
-- API @DescribeSnapshots@) or creating a volume from a snapshot
-- (CreateStorediSCSIVolume).
--
-- 'volumeARN', 'createSnapshotResponse_volumeARN' - The Amazon Resource Name (ARN) of the volume of which the snapshot was
-- taken.
--
-- 'httpStatus', 'createSnapshotResponse_httpStatus' - The response's http status code.
newCreateSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSnapshotResponse
newCreateSnapshotResponse pHttpStatus_ =
  CreateSnapshotResponse'
    { snapshotId =
        Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The snapshot ID that is used to refer to the snapshot in future
-- operations such as describing snapshots (Amazon Elastic Compute Cloud
-- API @DescribeSnapshots@) or creating a volume from a snapshot
-- (CreateStorediSCSIVolume).
createSnapshotResponse_snapshotId :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Prelude.Text)
createSnapshotResponse_snapshotId = Lens.lens (\CreateSnapshotResponse' {snapshotId} -> snapshotId) (\s@CreateSnapshotResponse' {} a -> s {snapshotId = a} :: CreateSnapshotResponse)

-- | The Amazon Resource Name (ARN) of the volume of which the snapshot was
-- taken.
createSnapshotResponse_volumeARN :: Lens.Lens' CreateSnapshotResponse (Prelude.Maybe Prelude.Text)
createSnapshotResponse_volumeARN = Lens.lens (\CreateSnapshotResponse' {volumeARN} -> volumeARN) (\s@CreateSnapshotResponse' {} a -> s {volumeARN = a} :: CreateSnapshotResponse)

-- | The response's http status code.
createSnapshotResponse_httpStatus :: Lens.Lens' CreateSnapshotResponse Prelude.Int
createSnapshotResponse_httpStatus = Lens.lens (\CreateSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateSnapshotResponse' {} a -> s {httpStatus = a} :: CreateSnapshotResponse)

instance Prelude.NFData CreateSnapshotResponse where
  rnf CreateSnapshotResponse' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf httpStatus

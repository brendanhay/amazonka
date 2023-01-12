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
-- Module      : Amazonka.MemoryDb.CopySnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a copy of an existing snapshot.
module Amazonka.MemoryDb.CopySnapshot
  ( -- * Creating a Request
    CopySnapshot (..),
    newCopySnapshot,

    -- * Request Lenses
    copySnapshot_kmsKeyId,
    copySnapshot_tags,
    copySnapshot_targetBucket,
    copySnapshot_sourceSnapshotName,
    copySnapshot_targetSnapshotName,

    -- * Destructuring the Response
    CopySnapshotResponse (..),
    newCopySnapshotResponse,

    -- * Response Lenses
    copySnapshotResponse_snapshot,
    copySnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The ID of the KMS key used to encrypt the target snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon S3 bucket to which the snapshot is exported. This parameter
    -- is used only when exporting a snapshot for external access. When using
    -- this parameter to export a snapshot, be sure MemoryDB has the needed
    -- permissions to this S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/snapshots-exporting.html Step 2: Grant MemoryDB Access to Your Amazon S3 Bucket>.
    targetBucket :: Prelude.Maybe Prelude.Text,
    -- | The name of an existing snapshot from which to make a copy.
    sourceSnapshotName :: Prelude.Text,
    -- | A name for the snapshot copy. MemoryDB does not permit overwriting a
    -- snapshot, therefore this name must be unique within its context -
    -- MemoryDB or an Amazon S3 bucket if exporting.
    targetSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopySnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'copySnapshot_kmsKeyId' - The ID of the KMS key used to encrypt the target snapshot.
--
-- 'tags', 'copySnapshot_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'targetBucket', 'copySnapshot_targetBucket' - The Amazon S3 bucket to which the snapshot is exported. This parameter
-- is used only when exporting a snapshot for external access. When using
-- this parameter to export a snapshot, be sure MemoryDB has the needed
-- permissions to this S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/snapshots-exporting.html Step 2: Grant MemoryDB Access to Your Amazon S3 Bucket>.
--
-- 'sourceSnapshotName', 'copySnapshot_sourceSnapshotName' - The name of an existing snapshot from which to make a copy.
--
-- 'targetSnapshotName', 'copySnapshot_targetSnapshotName' - A name for the snapshot copy. MemoryDB does not permit overwriting a
-- snapshot, therefore this name must be unique within its context -
-- MemoryDB or an Amazon S3 bucket if exporting.
newCopySnapshot ::
  -- | 'sourceSnapshotName'
  Prelude.Text ->
  -- | 'targetSnapshotName'
  Prelude.Text ->
  CopySnapshot
newCopySnapshot
  pSourceSnapshotName_
  pTargetSnapshotName_ =
    CopySnapshot'
      { kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetBucket = Prelude.Nothing,
        sourceSnapshotName = pSourceSnapshotName_,
        targetSnapshotName = pTargetSnapshotName_
      }

-- | The ID of the KMS key used to encrypt the target snapshot.
copySnapshot_kmsKeyId :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_kmsKeyId = Lens.lens (\CopySnapshot' {kmsKeyId} -> kmsKeyId) (\s@CopySnapshot' {} a -> s {kmsKeyId = a} :: CopySnapshot)

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
copySnapshot_tags :: Lens.Lens' CopySnapshot (Prelude.Maybe [Tag])
copySnapshot_tags = Lens.lens (\CopySnapshot' {tags} -> tags) (\s@CopySnapshot' {} a -> s {tags = a} :: CopySnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 bucket to which the snapshot is exported. This parameter
-- is used only when exporting a snapshot for external access. When using
-- this parameter to export a snapshot, be sure MemoryDB has the needed
-- permissions to this S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/MemoryDB/latest/devguide/snapshots-exporting.html Step 2: Grant MemoryDB Access to Your Amazon S3 Bucket>.
copySnapshot_targetBucket :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_targetBucket = Lens.lens (\CopySnapshot' {targetBucket} -> targetBucket) (\s@CopySnapshot' {} a -> s {targetBucket = a} :: CopySnapshot)

-- | The name of an existing snapshot from which to make a copy.
copySnapshot_sourceSnapshotName :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_sourceSnapshotName = Lens.lens (\CopySnapshot' {sourceSnapshotName} -> sourceSnapshotName) (\s@CopySnapshot' {} a -> s {sourceSnapshotName = a} :: CopySnapshot)

-- | A name for the snapshot copy. MemoryDB does not permit overwriting a
-- snapshot, therefore this name must be unique within its context -
-- MemoryDB or an Amazon S3 bucket if exporting.
copySnapshot_targetSnapshotName :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_targetSnapshotName = Lens.lens (\CopySnapshot' {targetSnapshotName} -> targetSnapshotName) (\s@CopySnapshot' {} a -> s {targetSnapshotName = a} :: CopySnapshot)

instance Core.AWSRequest CopySnapshot where
  type AWSResponse CopySnapshot = CopySnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            Prelude.<$> (x Data..?> "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopySnapshot where
  hashWithSalt _salt CopySnapshot' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetBucket
      `Prelude.hashWithSalt` sourceSnapshotName
      `Prelude.hashWithSalt` targetSnapshotName

instance Prelude.NFData CopySnapshot where
  rnf CopySnapshot' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetBucket
      `Prelude.seq` Prelude.rnf sourceSnapshotName
      `Prelude.seq` Prelude.rnf targetSnapshotName

instance Data.ToHeaders CopySnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.CopySnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopySnapshot where
  toJSON CopySnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetBucket" Data..=) Prelude.<$> targetBucket,
            Prelude.Just
              ("SourceSnapshotName" Data..= sourceSnapshotName),
            Prelude.Just
              ("TargetSnapshotName" Data..= targetSnapshotName)
          ]
      )

instance Data.ToPath CopySnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopySnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | Represents a copy of an entire cluster as of the time when the snapshot
    -- was taken.
    snapshot :: Prelude.Maybe Snapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopySnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshot', 'copySnapshotResponse_snapshot' - Represents a copy of an entire cluster as of the time when the snapshot
-- was taken.
--
-- 'httpStatus', 'copySnapshotResponse_httpStatus' - The response's http status code.
newCopySnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopySnapshotResponse
newCopySnapshotResponse pHttpStatus_ =
  CopySnapshotResponse'
    { snapshot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents a copy of an entire cluster as of the time when the snapshot
-- was taken.
copySnapshotResponse_snapshot :: Lens.Lens' CopySnapshotResponse (Prelude.Maybe Snapshot)
copySnapshotResponse_snapshot = Lens.lens (\CopySnapshotResponse' {snapshot} -> snapshot) (\s@CopySnapshotResponse' {} a -> s {snapshot = a} :: CopySnapshotResponse)

-- | The response's http status code.
copySnapshotResponse_httpStatus :: Lens.Lens' CopySnapshotResponse Prelude.Int
copySnapshotResponse_httpStatus = Lens.lens (\CopySnapshotResponse' {httpStatus} -> httpStatus) (\s@CopySnapshotResponse' {} a -> s {httpStatus = a} :: CopySnapshotResponse)

instance Prelude.NFData CopySnapshotResponse where
  rnf CopySnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus

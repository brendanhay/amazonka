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
-- Module      : Amazonka.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a copy of an existing snapshot.
--
-- This operation is valid for Redis only.
--
-- Users or groups that have permissions to use the @CopySnapshot@
-- operation can create their own Amazon S3 buckets and copy snapshots to
-- it. To control access to your snapshots, use an IAM policy to control
-- who has the ability to use the @CopySnapshot@ operation. For more
-- information about using IAM to control the use of ElastiCache
-- operations, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html Exporting Snapshots>
-- and
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/IAM.html Authentication & Access Control>.
--
-- You could receive the following error messages.
--
-- __Error Messages__
--
-- -   __Error Message:__ The S3 bucket %s is outside of the region.
--
--     __Solution:__ Create an Amazon S3 bucket in the same region as your
--     snapshot. For more information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket>
--     in the ElastiCache User Guide.
--
-- -   __Error Message:__ The S3 bucket %s does not exist.
--
--     __Solution:__ Create an Amazon S3 bucket in the same region as your
--     snapshot. For more information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket>
--     in the ElastiCache User Guide.
--
-- -   __Error Message:__ The S3 bucket %s is not owned by the
--     authenticated user.
--
--     __Solution:__ Create an Amazon S3 bucket in the same region as your
--     snapshot. For more information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket>
--     in the ElastiCache User Guide.
--
-- -   __Error Message:__ The authenticated user does not have sufficient
--     permissions to perform the desired activity.
--
--     __Solution:__ Contact your system administrator to get the needed
--     permissions.
--
-- -   __Error Message:__ The S3 bucket %s already contains an object with
--     key %s.
--
--     __Solution:__ Give the @TargetSnapshotName@ a new and unique value.
--     If exporting a snapshot, you could alternatively create a new Amazon
--     S3 bucket and use this same value for @TargetSnapshotName@.
--
-- -   __Error Message:__ ElastiCache has not been granted READ permissions
--     %s on the S3 Bucket.
--
--     __Solution:__ Add List and Read permissions on the bucket. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
--     in the ElastiCache User Guide.
--
-- -   __Error Message:__ ElastiCache has not been granted WRITE
--     permissions %s on the S3 Bucket.
--
--     __Solution:__ Add Upload\/Delete permissions on the bucket. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
--     in the ElastiCache User Guide.
--
-- -   __Error Message:__ ElastiCache has not been granted READ_ACP
--     permissions %s on the S3 Bucket.
--
--     __Solution:__ Add View Permissions on the bucket. For more
--     information, see
--     <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
--     in the ElastiCache User Guide.
module Amazonka.ElastiCache.CopySnapshot
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
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CopySnapshotMessage@ operation.
--
-- /See:/ 'newCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The ID of the KMS key used to encrypt the target snapshot.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon S3 bucket to which the snapshot is exported. This parameter
    -- is used only when exporting a snapshot for external access.
    --
    -- When using this parameter to export a snapshot, be sure Amazon
    -- ElastiCache has the needed permissions to this S3 bucket. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
    -- in the /Amazon ElastiCache User Guide/.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html Exporting a Snapshot>
    -- in the /Amazon ElastiCache User Guide/.
    targetBucket :: Prelude.Maybe Prelude.Text,
    -- | The name of an existing snapshot from which to make a copy.
    sourceSnapshotName :: Prelude.Text,
    -- | A name for the snapshot copy. ElastiCache does not permit overwriting a
    -- snapshot, therefore this name must be unique within its context -
    -- ElastiCache or an Amazon S3 bucket if exporting.
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
-- is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon
-- ElastiCache has the needed permissions to this S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
-- in the /Amazon ElastiCache User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html Exporting a Snapshot>
-- in the /Amazon ElastiCache User Guide/.
--
-- 'sourceSnapshotName', 'copySnapshot_sourceSnapshotName' - The name of an existing snapshot from which to make a copy.
--
-- 'targetSnapshotName', 'copySnapshot_targetSnapshotName' - A name for the snapshot copy. ElastiCache does not permit overwriting a
-- snapshot, therefore this name must be unique within its context -
-- ElastiCache or an Amazon S3 bucket if exporting.
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
-- is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon
-- ElastiCache has the needed permissions to this S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket>
-- in the /Amazon ElastiCache User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html Exporting a Snapshot>
-- in the /Amazon ElastiCache User Guide/.
copySnapshot_targetBucket :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_targetBucket = Lens.lens (\CopySnapshot' {targetBucket} -> targetBucket) (\s@CopySnapshot' {} a -> s {targetBucket = a} :: CopySnapshot)

-- | The name of an existing snapshot from which to make a copy.
copySnapshot_sourceSnapshotName :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_sourceSnapshotName = Lens.lens (\CopySnapshot' {sourceSnapshotName} -> sourceSnapshotName) (\s@CopySnapshot' {} a -> s {sourceSnapshotName = a} :: CopySnapshot)

-- | A name for the snapshot copy. ElastiCache does not permit overwriting a
-- snapshot, therefore this name must be unique within its context -
-- ElastiCache or an Amazon S3 bucket if exporting.
copySnapshot_targetSnapshotName :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_targetSnapshotName = Lens.lens (\CopySnapshot' {targetSnapshotName} -> targetSnapshotName) (\s@CopySnapshot' {} a -> s {targetSnapshotName = a} :: CopySnapshot)

instance Core.AWSRequest CopySnapshot where
  type AWSResponse CopySnapshot = CopySnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CopySnapshotResult"
      ( \s h x ->
          CopySnapshotResponse'
            Prelude.<$> (x Data..@? "Snapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopySnapshot where
  hashWithSalt _salt CopySnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
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
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopySnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopySnapshot where
  toQuery CopySnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopySnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "KmsKeyId" Data.=: kmsKeyId,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TargetBucket" Data.=: targetBucket,
        "SourceSnapshotName" Data.=: sourceSnapshotName,
        "TargetSnapshotName" Data.=: targetSnapshotName
      ]

-- | /See:/ 'newCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { snapshot :: Prelude.Maybe Snapshot,
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
-- 'snapshot', 'copySnapshotResponse_snapshot' - Undocumented member.
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

-- | Undocumented member.
copySnapshotResponse_snapshot :: Lens.Lens' CopySnapshotResponse (Prelude.Maybe Snapshot)
copySnapshotResponse_snapshot = Lens.lens (\CopySnapshotResponse' {snapshot} -> snapshot) (\s@CopySnapshotResponse' {} a -> s {snapshot = a} :: CopySnapshotResponse)

-- | The response's http status code.
copySnapshotResponse_httpStatus :: Lens.Lens' CopySnapshotResponse Prelude.Int
copySnapshotResponse_httpStatus = Lens.lens (\CopySnapshotResponse' {httpStatus} -> httpStatus) (\s@CopySnapshotResponse' {} a -> s {httpStatus = a} :: CopySnapshotResponse)

instance Prelude.NFData CopySnapshotResponse where
  rnf CopySnapshotResponse' {..} =
    Prelude.rnf snapshot
      `Prelude.seq` Prelude.rnf httpStatus

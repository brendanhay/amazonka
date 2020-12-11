{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a copy of an existing snapshot.
--
-- /Important:/ Users or groups that have permissions to use the @CopySnapshot@ operation can create their own Amazon S3 buckets and copy snapshots to it. To control access to your snapshots, use an IAM policy to control who has the ability to use the @CopySnapshot@ operation. For more information about using IAM to control the use of ElastiCache operations, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html Exporting Snapshots> and <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/IAM.html Authentication & Access Control> .
-- You could receive the following error messages.
-- __Error Messages__
--
--     * __Error Message:__ The S3 bucket %s is outside of the region.
-- __Solution:__ Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket> in the ElastiCache User Guide.
--
--
--     * __Error Message:__ The S3 bucket %s does not exist.
-- __Solution:__ Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket> in the ElastiCache User Guide.
--
--
--     * __Error Message:__ The S3 bucket %s is not owned by the authenticated user.
-- __Solution:__ Create an Amazon S3 bucket in the same region as your snapshot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-create-s3-bucket Step 1: Create an Amazon S3 Bucket> in the ElastiCache User Guide.
--
--
--     * __Error Message:__ The authenticated user does not have sufficient permissions to perform the desired activity.
-- __Solution:__ Contact your system administrator to get the needed permissions.
--
--
--     * __Error Message:__ The S3 bucket %s already contains an object with key %s.
-- __Solution:__ Give the @TargetSnapshotName@ a new and unique value. If exporting a snapshot, you could alternatively create a new Amazon S3 bucket and use this same value for @TargetSnapshotName@ .
--
--
--     * __Error Message: __ ElastiCache has not been granted READ permissions %s on the S3 Bucket.
-- __Solution:__ Add List and Read permissions on the bucket. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the ElastiCache User Guide.
--
--
--     * __Error Message: __ ElastiCache has not been granted WRITE permissions %s on the S3 Bucket.
-- __Solution:__ Add Upload/Delete permissions on the bucket. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the ElastiCache User Guide.
--
--
--     * __Error Message: __ ElastiCache has not been granted READ_ACP permissions %s on the S3 Bucket.
-- __Solution:__ Add View Permissions on the bucket. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the ElastiCache User Guide.
module Network.AWS.ElastiCache.CopySnapshot
  ( -- * Creating a request
    CopySnapshot (..),
    mkCopySnapshot,

    -- ** Request lenses
    csTargetBucket,
    csKMSKeyId,
    csSourceSnapshotName,
    csTargetSnapshotName,

    -- * Destructuring the response
    CopySnapshotResponse (..),
    mkCopySnapshotResponse,

    -- ** Response lenses
    csrsSnapshot,
    csrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CopySnapshotMessage@ operation.
--
-- /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { targetBucket ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    sourceSnapshotName :: Lude.Text,
    targetSnapshotName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The ID of the KMS key used to encrypt the target snapshot.
-- * 'sourceSnapshotName' - The name of an existing snapshot from which to make a copy.
-- * 'targetBucket' - The Amazon S3 bucket to which the snapshot is exported. This parameter is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon ElastiCache has the needed permissions to this S3 bucket. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the /Amazon ElastiCache User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Snapshots.Exporting.html Exporting a Snapshot> in the /Amazon ElastiCache User Guide/ .
-- * 'targetSnapshotName' - A name for the snapshot copy. ElastiCache does not permit overwriting a snapshot, therefore this name must be unique within its context - ElastiCache or an Amazon S3 bucket if exporting.
mkCopySnapshot ::
  -- | 'sourceSnapshotName'
  Lude.Text ->
  -- | 'targetSnapshotName'
  Lude.Text ->
  CopySnapshot
mkCopySnapshot pSourceSnapshotName_ pTargetSnapshotName_ =
  CopySnapshot'
    { targetBucket = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      sourceSnapshotName = pSourceSnapshotName_,
      targetSnapshotName = pTargetSnapshotName_
    }

-- | The Amazon S3 bucket to which the snapshot is exported. This parameter is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon ElastiCache has the needed permissions to this S3 bucket. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the /Amazon ElastiCache User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Snapshots.Exporting.html Exporting a Snapshot> in the /Amazon ElastiCache User Guide/ .
--
-- /Note:/ Consider using 'targetBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetBucket :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csTargetBucket = Lens.lens (targetBucket :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {targetBucket = a} :: CopySnapshot)
{-# DEPRECATED csTargetBucket "Use generic-lens or generic-optics with 'targetBucket' instead." #-}

-- | The ID of the KMS key used to encrypt the target snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKMSKeyId :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csKMSKeyId = Lens.lens (kmsKeyId :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CopySnapshot)
{-# DEPRECATED csKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of an existing snapshot from which to make a copy.
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSnapshotName :: Lens.Lens' CopySnapshot Lude.Text
csSourceSnapshotName = Lens.lens (sourceSnapshotName :: CopySnapshot -> Lude.Text) (\s a -> s {sourceSnapshotName = a} :: CopySnapshot)
{-# DEPRECATED csSourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead." #-}

-- | A name for the snapshot copy. ElastiCache does not permit overwriting a snapshot, therefore this name must be unique within its context - ElastiCache or an Amazon S3 bucket if exporting.
--
-- /Note:/ Consider using 'targetSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetSnapshotName :: Lens.Lens' CopySnapshot Lude.Text
csTargetSnapshotName = Lens.lens (targetSnapshotName :: CopySnapshot -> Lude.Text) (\s a -> s {targetSnapshotName = a} :: CopySnapshot)
{-# DEPRECATED csTargetSnapshotName "Use generic-lens or generic-optics with 'targetSnapshotName' instead." #-}

instance Lude.AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CopySnapshotResult"
      ( \s h x ->
          CopySnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopySnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopySnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopySnapshot where
  toQuery CopySnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopySnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "TargetBucket" Lude.=: targetBucket,
        "KmsKeyId" Lude.=: kmsKeyId,
        "SourceSnapshotName" Lude.=: sourceSnapshotName,
        "TargetSnapshotName" Lude.=: targetSnapshotName
      ]

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { snapshot ::
      Lude.Maybe Snapshot,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkCopySnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopySnapshotResponse
mkCopySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSnapshot :: Lens.Lens' CopySnapshotResponse (Lude.Maybe Snapshot)
csrsSnapshot = Lens.lens (snapshot :: CopySnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: CopySnapshotResponse)
{-# DEPRECATED csrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CopySnapshotResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CopySnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopySnapshotResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

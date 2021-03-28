{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.ElastiCache.CopySnapshot
    (
    -- * Creating a request
      CopySnapshot (..)
    , mkCopySnapshot
    -- ** Request lenses
    , csSourceSnapshotName
    , csTargetSnapshotName
    , csKmsKeyId
    , csTargetBucket

    -- * Destructuring the response
    , CopySnapshotResponse (..)
    , mkCopySnapshotResponse
    -- ** Response lenses
    , csrrsSnapshot
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CopySnapshotMessage@ operation.
--
-- /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { sourceSnapshotName :: Core.Text
    -- ^ The name of an existing snapshot from which to make a copy.
  , targetSnapshotName :: Core.Text
    -- ^ A name for the snapshot copy. ElastiCache does not permit overwriting a snapshot, therefore this name must be unique within its context - ElastiCache or an Amazon S3 bucket if exporting.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The ID of the KMS key used to encrypt the target snapshot.
  , targetBucket :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket to which the snapshot is exported. This parameter is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon ElastiCache has the needed permissions to this S3 bucket. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the /Amazon ElastiCache User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Snapshots.Exporting.html Exporting a Snapshot> in the /Amazon ElastiCache User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopySnapshot' value with any optional fields omitted.
mkCopySnapshot
    :: Core.Text -- ^ 'sourceSnapshotName'
    -> Core.Text -- ^ 'targetSnapshotName'
    -> CopySnapshot
mkCopySnapshot sourceSnapshotName targetSnapshotName
  = CopySnapshot'{sourceSnapshotName, targetSnapshotName,
                  kmsKeyId = Core.Nothing, targetBucket = Core.Nothing}

-- | The name of an existing snapshot from which to make a copy.
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSnapshotName :: Lens.Lens' CopySnapshot Core.Text
csSourceSnapshotName = Lens.field @"sourceSnapshotName"
{-# INLINEABLE csSourceSnapshotName #-}
{-# DEPRECATED sourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead"  #-}

-- | A name for the snapshot copy. ElastiCache does not permit overwriting a snapshot, therefore this name must be unique within its context - ElastiCache or an Amazon S3 bucket if exporting.
--
-- /Note:/ Consider using 'targetSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetSnapshotName :: Lens.Lens' CopySnapshot Core.Text
csTargetSnapshotName = Lens.field @"targetSnapshotName"
{-# INLINEABLE csTargetSnapshotName #-}
{-# DEPRECATED targetSnapshotName "Use generic-lens or generic-optics with 'targetSnapshotName' instead"  #-}

-- | The ID of the KMS key used to encrypt the target snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKmsKeyId :: Lens.Lens' CopySnapshot (Core.Maybe Core.Text)
csKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE csKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The Amazon S3 bucket to which the snapshot is exported. This parameter is used only when exporting a snapshot for external access.
--
-- When using this parameter to export a snapshot, be sure Amazon ElastiCache has the needed permissions to this S3 bucket. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/backups-exporting.html#backups-exporting-grant-access Step 2: Grant ElastiCache Access to Your Amazon S3 Bucket> in the /Amazon ElastiCache User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Snapshots.Exporting.html Exporting a Snapshot> in the /Amazon ElastiCache User Guide/ .
--
-- /Note:/ Consider using 'targetBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetBucket :: Lens.Lens' CopySnapshot (Core.Maybe Core.Text)
csTargetBucket = Lens.field @"targetBucket"
{-# INLINEABLE csTargetBucket #-}
{-# DEPRECATED targetBucket "Use generic-lens or generic-optics with 'targetBucket' instead"  #-}

instance Core.ToQuery CopySnapshot where
        toQuery CopySnapshot{..}
          = Core.toQueryPair "Action" ("CopySnapshot" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "SourceSnapshotName" sourceSnapshotName
              Core.<> Core.toQueryPair "TargetSnapshotName" targetSnapshotName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetBucket")
                targetBucket

instance Core.ToHeaders CopySnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopySnapshot where
        type Rs CopySnapshot = CopySnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CopySnapshotResult"
              (\ s h x ->
                 CopySnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CopySnapshotResponse' value with any optional fields omitted.
mkCopySnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopySnapshotResponse
mkCopySnapshotResponse responseStatus
  = CopySnapshotResponse'{snapshot = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSnapshot :: Lens.Lens' CopySnapshotResponse (Core.Maybe Types.Snapshot)
csrrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE csrrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CopySnapshotResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

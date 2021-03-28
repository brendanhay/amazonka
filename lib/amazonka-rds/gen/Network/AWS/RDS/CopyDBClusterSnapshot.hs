{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a snapshot of a DB cluster.
--
-- To copy a DB cluster snapshot from a shared manual DB cluster snapshot, @SourceDBClusterSnapshotIdentifier@ must be the Amazon Resource Name (ARN) of the shared DB cluster snapshot.
-- You can copy an encrypted DB cluster snapshot from another AWS Region. In that case, the AWS Region where you call the @CopyDBClusterSnapshot@ action is the destination AWS Region for the encrypted DB cluster snapshot to be copied to. To copy an encrypted DB cluster snapshot from another AWS Region, you must provide the following values:
--
--     * @KmsKeyId@ - The AWS Key Management System (AWS KMS) key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region.
--
--
--     * @PreSignedUrl@ - A URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ action to be called in the source AWS Region where the DB cluster snapshot is copied from. The pre-signed URL must be a valid request for the @CopyDBClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied.
-- The pre-signed URL request must contain the following parameter values:
--
--     * @KmsKeyId@ - The KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
--
--
--     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot is to be created in.
--
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ .
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
--
--     * @TargetDBClusterSnapshotIdentifier@ - The identifier for the new copy of the DB cluster snapshot in the destination AWS Region.
--
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the ARN format for the source AWS Region and is the same value as the @SourceDBClusterSnapshotIdentifier@ in the pre-signed URL. 
--
--
-- To cancel the copy operation once it is in progress, delete the target DB cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@ while that DB cluster snapshot is in "copying" status.
-- For more information on copying encrypted DB cluster snapshots from one AWS Region to another, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html Copying a Snapshot> in the /Amazon Aurora User Guide./ 
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.CopyDBClusterSnapshot
    (
    -- * Creating a request
      CopyDBClusterSnapshot (..)
    , mkCopyDBClusterSnapshot
    -- ** Request lenses
    , cdbcsfSourceDBClusterSnapshotIdentifier
    , cdbcsfTargetDBClusterSnapshotIdentifier
    , cdbcsfCopyTags
    , cdbcsfKmsKeyId
    , cdbcsfPreSignedUrl
    , cdbcsfTags

    -- * Destructuring the response
    , CopyDBClusterSnapshotResponse (..)
    , mkCopyDBClusterSnapshotResponse
    -- ** Response lenses
    , cdbcsrfrsDBClusterSnapshot
    , cdbcsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCopyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { sourceDBClusterSnapshotIdentifier :: Core.Text
    -- ^ The identifier of the DB cluster snapshot to copy. This parameter isn't case-sensitive.
--
-- You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another.
-- Constraints:
--
--     * Must specify a valid system snapshot in the "available" state.
--
--
--     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.
--
--
--     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. For more information, go to <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across AWS Regions> in the /Amazon Aurora User Guide./ 
--
--
-- Example: @my-cluster-snapshot1@ 
  , targetDBClusterSnapshotIdentifier :: Core.Text
    -- ^ The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster-snapshot2@ 
  , copyTags :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot. By default, tags are not copied.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. 
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot. 
-- If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ . 
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
  , preSignedUrl :: Core.Maybe Core.Text
    -- ^ The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB cluster snapshot from another AWS Region. Don't specify @PreSignedUrl@ when you are copying an encrypted DB cluster snapshot in the same AWS Region.
--
-- The pre-signed URL must be a valid request for the @CopyDBClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The pre-signed URL request must contain the following parameter values:
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
--
--
--     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot is to be created in.
--
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ .
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
  , tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBClusterSnapshot' value with any optional fields omitted.
mkCopyDBClusterSnapshot
    :: Core.Text -- ^ 'sourceDBClusterSnapshotIdentifier'
    -> Core.Text -- ^ 'targetDBClusterSnapshotIdentifier'
    -> CopyDBClusterSnapshot
mkCopyDBClusterSnapshot sourceDBClusterSnapshotIdentifier
  targetDBClusterSnapshotIdentifier
  = CopyDBClusterSnapshot'{sourceDBClusterSnapshotIdentifier,
                           targetDBClusterSnapshotIdentifier, copyTags = Core.Nothing,
                           kmsKeyId = Core.Nothing, preSignedUrl = Core.Nothing,
                           tags = Core.Nothing}

-- | The identifier of the DB cluster snapshot to copy. This parameter isn't case-sensitive.
--
-- You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another.
-- Constraints:
--
--     * Must specify a valid system snapshot in the "available" state.
--
--
--     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.
--
--
--     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. For more information, go to <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_CopySnapshot.html#USER_CopySnapshot.AcrossRegions Copying Snapshots Across AWS Regions> in the /Amazon Aurora User Guide./ 
--
--
-- Example: @my-cluster-snapshot1@ 
--
-- /Note:/ Consider using 'sourceDBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfSourceDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Core.Text
cdbcsfSourceDBClusterSnapshotIdentifier = Lens.field @"sourceDBClusterSnapshotIdentifier"
{-# INLINEABLE cdbcsfSourceDBClusterSnapshotIdentifier #-}
{-# DEPRECATED sourceDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterSnapshotIdentifier' instead"  #-}

-- | The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster-snapshot2@ 
--
-- /Note:/ Consider using 'targetDBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfTargetDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Core.Text
cdbcsfTargetDBClusterSnapshotIdentifier = Lens.field @"targetDBClusterSnapshotIdentifier"
{-# INLINEABLE cdbcsfTargetDBClusterSnapshotIdentifier #-}
{-# DEPRECATED targetDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'targetDBClusterSnapshotIdentifier' instead"  #-}

-- | A value that indicates whether to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfCopyTags :: Lens.Lens' CopyDBClusterSnapshot (Core.Maybe Core.Bool)
cdbcsfCopyTags = Lens.field @"copyTags"
{-# INLINEABLE cdbcsfCopyTags #-}
{-# DEPRECATED copyTags "Use generic-lens or generic-optics with 'copyTags' instead"  #-}

-- | The AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. 
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot. 
-- If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ . 
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfKmsKeyId :: Lens.Lens' CopyDBClusterSnapshot (Core.Maybe Core.Text)
cdbcsfKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE cdbcsfKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB cluster snapshot from another AWS Region. Don't specify @PreSignedUrl@ when you are copying an encrypted DB cluster snapshot in the same AWS Region.
--
-- The pre-signed URL must be a valid request for the @CopyDBClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The pre-signed URL request must contain the following parameter values:
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
--
--
--     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot is to be created in.
--
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ .
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfPreSignedUrl :: Lens.Lens' CopyDBClusterSnapshot (Core.Maybe Core.Text)
cdbcsfPreSignedUrl = Lens.field @"preSignedUrl"
{-# INLINEABLE cdbcsfPreSignedUrl #-}
{-# DEPRECATED preSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsfTags :: Lens.Lens' CopyDBClusterSnapshot (Core.Maybe [Types.Tag])
cdbcsfTags = Lens.field @"tags"
{-# INLINEABLE cdbcsfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CopyDBClusterSnapshot where
        toQuery CopyDBClusterSnapshot{..}
          = Core.toQueryPair "Action" ("CopyDBClusterSnapshot" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "SourceDBClusterSnapshotIdentifier"
                sourceDBClusterSnapshotIdentifier
              Core.<>
              Core.toQueryPair "TargetDBClusterSnapshotIdentifier"
                targetDBClusterSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTags") copyTags
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreSignedUrl")
                preSignedUrl
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CopyDBClusterSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopyDBClusterSnapshot where
        type Rs CopyDBClusterSnapshot = CopyDBClusterSnapshotResponse
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
          = Response.receiveXMLWrapper "CopyDBClusterSnapshotResult"
              (\ s h x ->
                 CopyDBClusterSnapshotResponse' Core.<$>
                   (x Core..@? "DBClusterSnapshot") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCopyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { dBClusterSnapshot :: Core.Maybe Types.DBClusterSnapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CopyDBClusterSnapshotResponse' value with any optional fields omitted.
mkCopyDBClusterSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyDBClusterSnapshotResponse
mkCopyDBClusterSnapshotResponse responseStatus
  = CopyDBClusterSnapshotResponse'{dBClusterSnapshot = Core.Nothing,
                                   responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrfrsDBClusterSnapshot :: Lens.Lens' CopyDBClusterSnapshotResponse (Core.Maybe Types.DBClusterSnapshot)
cdbcsrfrsDBClusterSnapshot = Lens.field @"dBClusterSnapshot"
{-# INLINEABLE cdbcsrfrsDBClusterSnapshot #-}
{-# DEPRECATED dBClusterSnapshot "Use generic-lens or generic-optics with 'dBClusterSnapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrfrsResponseStatus :: Lens.Lens' CopyDBClusterSnapshotResponse Core.Int
cdbcsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbcsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB snapshot. The source DB snapshot must be in the @available@ state.
--
-- You can copy a snapshot from one AWS Region to another. In that case, the AWS Region where you call the @CopyDBSnapshot@ action is the destination AWS Region for the DB snapshot copy.
-- For more information about copying snapshots, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopyDBSnapshot Copying a DB Snapshot> in the /Amazon RDS User Guide./
module Network.AWS.RDS.CopyDBSnapshot
  ( -- * Creating a request
    CopyDBSnapshot (..),
    mkCopyDBSnapshot,

    -- ** Request lenses
    cdbsSourceDBSnapshotIdentifier,
    cdbsTargetDBSnapshotIdentifier,
    cdbsCopyTags,
    cdbsKmsKeyId,
    cdbsOptionGroupName,
    cdbsPreSignedUrl,
    cdbsTags,
    cdbsTargetCustomAvailabilityZone,

    -- * Destructuring the response
    CopyDBSnapshotResponse (..),
    mkCopyDBSnapshotResponse,

    -- ** Response lenses
    cdbsrrsDBSnapshot,
    cdbsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCopyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
  { -- | The identifier for the source DB snapshot.
    --
    -- If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier. For example, you might specify @rds:mysql-instance1-snapshot-20130805@ .
    -- If the source snapshot is in a different AWS Region than the copy, specify a valid DB snapshot ARN. For example, you might specify @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@ .
    -- If you are copying from a shared manual DB snapshot, this parameter must be the Amazon Resource Name (ARN) of the shared DB snapshot.
    -- If you are copying an encrypted snapshot this parameter must be in the ARN format for the source AWS Region, and must match the @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.
    -- Constraints:
    --
    --     * Must specify a valid system snapshot in the "available" state.
    --
    --
    -- Example: @rds:mydb-2012-04-02-00-01@
    -- Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
    sourceDBSnapshotIdentifier :: Types.SourceDBSnapshotIdentifier,
    -- | The identifier for the copy of the snapshot.
    --
    -- Constraints:
    --
    --     * Can't be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-db-snapshot@
    targetDBSnapshotIdentifier :: Types.TargetDBSnapshotIdentifier,
    -- | A value that indicates whether to copy all tags from the source DB snapshot to the target DB snapshot. By default, tags are not copied.
    copyTags :: Core.Maybe Core.Bool,
    -- | The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.
    -- If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.
    -- If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.
    -- If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of an option group to associate with the copy of the snapshot.
    --
    -- Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> in the /Amazon RDS User Guide./
    optionGroupName :: Core.Maybe Types.OptionGroupName,
    -- | The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the source AWS Region that contains the source DB snapshot to copy.
    --
    -- You must specify this parameter when you copy an encrypted DB snapshot from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are copying an encrypted DB snapshot in the same AWS Region.
    -- The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:
    --
    --     * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot is copied to. This AWS Region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.
    -- For example, if you copy an encrypted DB snapshot from the us-west-2 AWS Region to the us-east-1 AWS Region, then you call the @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.
    --
    --
    --     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.
    --
    --
    --     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB snapshot from the us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ .
    --
    --
    -- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
    preSignedUrl :: Core.Maybe Types.PreSignedUrl,
    tags :: Core.Maybe [Types.Tag],
    -- | The external custom Availability Zone (CAZ) identifier for the target CAZ.
    --
    -- Example: @rds-caz-aiqhTgQv@ .
    targetCustomAvailabilityZone :: Core.Maybe Types.TargetCustomAvailabilityZone
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBSnapshot' value with any optional fields omitted.
mkCopyDBSnapshot ::
  -- | 'sourceDBSnapshotIdentifier'
  Types.SourceDBSnapshotIdentifier ->
  -- | 'targetDBSnapshotIdentifier'
  Types.TargetDBSnapshotIdentifier ->
  CopyDBSnapshot
mkCopyDBSnapshot
  sourceDBSnapshotIdentifier
  targetDBSnapshotIdentifier =
    CopyDBSnapshot'
      { sourceDBSnapshotIdentifier,
        targetDBSnapshotIdentifier,
        copyTags = Core.Nothing,
        kmsKeyId = Core.Nothing,
        optionGroupName = Core.Nothing,
        preSignedUrl = Core.Nothing,
        tags = Core.Nothing,
        targetCustomAvailabilityZone = Core.Nothing
      }

-- | The identifier for the source DB snapshot.
--
-- If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier. For example, you might specify @rds:mysql-instance1-snapshot-20130805@ .
-- If the source snapshot is in a different AWS Region than the copy, specify a valid DB snapshot ARN. For example, you might specify @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@ .
-- If you are copying from a shared manual DB snapshot, this parameter must be the Amazon Resource Name (ARN) of the shared DB snapshot.
-- If you are copying an encrypted snapshot this parameter must be in the ARN format for the source AWS Region, and must match the @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.
-- Constraints:
--
--     * Must specify a valid system snapshot in the "available" state.
--
--
-- Example: @rds:mydb-2012-04-02-00-01@
-- Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
--
-- /Note:/ Consider using 'sourceDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsSourceDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Types.SourceDBSnapshotIdentifier
cdbsSourceDBSnapshotIdentifier = Lens.field @"sourceDBSnapshotIdentifier"
{-# DEPRECATED cdbsSourceDBSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBSnapshotIdentifier' instead." #-}

-- | The identifier for the copy of the snapshot.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-db-snapshot@
--
-- /Note:/ Consider using 'targetDBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsTargetDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Types.TargetDBSnapshotIdentifier
cdbsTargetDBSnapshotIdentifier = Lens.field @"targetDBSnapshotIdentifier"
{-# DEPRECATED cdbsTargetDBSnapshotIdentifier "Use generic-lens or generic-optics with 'targetDBSnapshotIdentifier' instead." #-}

-- | A value that indicates whether to copy all tags from the source DB snapshot to the target DB snapshot. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsCopyTags :: Lens.Lens' CopyDBSnapshot (Core.Maybe Core.Bool)
cdbsCopyTags = Lens.field @"copyTags"
{-# DEPRECATED cdbsCopyTags "Use generic-lens or generic-optics with 'copyTags' instead." #-}

-- | The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.
-- If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.
-- If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.
-- If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsKmsKeyId :: Lens.Lens' CopyDBSnapshot (Core.Maybe Types.KmsKeyId)
cdbsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cdbsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsOptionGroupName :: Lens.Lens' CopyDBSnapshot (Core.Maybe Types.OptionGroupName)
cdbsOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED cdbsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the source AWS Region that contains the source DB snapshot to copy.
--
-- You must specify this parameter when you copy an encrypted DB snapshot from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are copying an encrypted DB snapshot in the same AWS Region.
-- The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:
--
--     * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot is copied to. This AWS Region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.
-- For example, if you copy an encrypted DB snapshot from the us-west-2 AWS Region to the us-east-1 AWS Region, then you call the @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.
--
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.
--
--
--     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB snapshot from the us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ .
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsPreSignedUrl :: Lens.Lens' CopyDBSnapshot (Core.Maybe Types.PreSignedUrl)
cdbsPreSignedUrl = Lens.field @"preSignedUrl"
{-# DEPRECATED cdbsPreSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsTags :: Lens.Lens' CopyDBSnapshot (Core.Maybe [Types.Tag])
cdbsTags = Lens.field @"tags"
{-# DEPRECATED cdbsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The external custom Availability Zone (CAZ) identifier for the target CAZ.
--
-- Example: @rds-caz-aiqhTgQv@ .
--
-- /Note:/ Consider using 'targetCustomAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsTargetCustomAvailabilityZone :: Lens.Lens' CopyDBSnapshot (Core.Maybe Types.TargetCustomAvailabilityZone)
cdbsTargetCustomAvailabilityZone = Lens.field @"targetCustomAvailabilityZone"
{-# DEPRECATED cdbsTargetCustomAvailabilityZone "Use generic-lens or generic-optics with 'targetCustomAvailabilityZone' instead." #-}

instance Core.AWSRequest CopyDBSnapshot where
  type Rs CopyDBSnapshot = CopyDBSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CopyDBSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "SourceDBSnapshotIdentifier"
                            sourceDBSnapshotIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetDBSnapshotIdentifier"
                            targetDBSnapshotIdentifier
                        )
                Core.<> (Core.toQueryValue "CopyTags" Core.<$> copyTags)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "OptionGroupName" Core.<$> optionGroupName)
                Core.<> (Core.toQueryValue "PreSignedUrl" Core.<$> preSignedUrl)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> ( Core.toQueryValue "TargetCustomAvailabilityZone"
                            Core.<$> targetCustomAvailabilityZone
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CopyDBSnapshotResult"
      ( \s h x ->
          CopyDBSnapshotResponse'
            Core.<$> (x Core..@? "DBSnapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyDBSnapshotResponse' smart constructor.
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
  { dBSnapshot :: Core.Maybe Types.DBSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CopyDBSnapshotResponse' value with any optional fields omitted.
mkCopyDBSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyDBSnapshotResponse
mkCopyDBSnapshotResponse responseStatus =
  CopyDBSnapshotResponse'
    { dBSnapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsrrsDBSnapshot :: Lens.Lens' CopyDBSnapshotResponse (Core.Maybe Types.DBSnapshot)
cdbsrrsDBSnapshot = Lens.field @"dBSnapshot"
{-# DEPRECATED cdbsrrsDBSnapshot "Use generic-lens or generic-optics with 'dBSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsrrsResponseStatus :: Lens.Lens' CopyDBSnapshotResponse Core.Int
cdbsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

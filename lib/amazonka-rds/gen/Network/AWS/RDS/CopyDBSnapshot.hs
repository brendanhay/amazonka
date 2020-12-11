{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cdsTargetCustomAvailabilityZone,
    cdsPreSignedURL,
    cdsCopyTags,
    cdsKMSKeyId,
    cdsOptionGroupName,
    cdsTags,
    cdsSourceDBSnapshotIdentifier,
    cdsTargetDBSnapshotIdentifier,

    -- * Destructuring the response
    CopyDBSnapshotResponse (..),
    mkCopyDBSnapshotResponse,

    -- ** Response lenses
    cdsrsDBSnapshot,
    cdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCopyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
  { targetCustomAvailabilityZone ::
      Lude.Maybe Lude.Text,
    preSignedURL :: Lude.Maybe Lude.Text,
    copyTags :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    optionGroupName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    sourceDBSnapshotIdentifier :: Lude.Text,
    targetDBSnapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBSnapshot' with the minimum fields required to make a request.
--
-- * 'copyTags' - A value that indicates whether to copy all tags from the source DB snapshot to the target DB snapshot. By default, tags are not copied.
-- * 'kmsKeyId' - The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.
-- If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.
-- If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.
-- If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- * 'optionGroupName' - The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> in the /Amazon RDS User Guide./
-- * 'preSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the source AWS Region that contains the source DB snapshot to copy.
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
-- * 'sourceDBSnapshotIdentifier' - The identifier for the source DB snapshot.
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
-- * 'tags' - Undocumented field.
-- * 'targetCustomAvailabilityZone' - The external custom Availability Zone (CAZ) identifier for the target CAZ.
--
-- Example: @rds-caz-aiqhTgQv@ .
-- * 'targetDBSnapshotIdentifier' - The identifier for the copy of the snapshot.
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
mkCopyDBSnapshot ::
  -- | 'sourceDBSnapshotIdentifier'
  Lude.Text ->
  -- | 'targetDBSnapshotIdentifier'
  Lude.Text ->
  CopyDBSnapshot
mkCopyDBSnapshot
  pSourceDBSnapshotIdentifier_
  pTargetDBSnapshotIdentifier_ =
    CopyDBSnapshot'
      { targetCustomAvailabilityZone = Lude.Nothing,
        preSignedURL = Lude.Nothing,
        copyTags = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        tags = Lude.Nothing,
        sourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier_,
        targetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier_
      }

-- | The external custom Availability Zone (CAZ) identifier for the target CAZ.
--
-- Example: @rds-caz-aiqhTgQv@ .
--
-- /Note:/ Consider using 'targetCustomAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTargetCustomAvailabilityZone :: Lens.Lens' CopyDBSnapshot (Lude.Maybe Lude.Text)
cdsTargetCustomAvailabilityZone = Lens.lens (targetCustomAvailabilityZone :: CopyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {targetCustomAvailabilityZone = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsTargetCustomAvailabilityZone "Use generic-lens or generic-optics with 'targetCustomAvailabilityZone' instead." #-}

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
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsPreSignedURL :: Lens.Lens' CopyDBSnapshot (Lude.Maybe Lude.Text)
cdsPreSignedURL = Lens.lens (preSignedURL :: CopyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

-- | A value that indicates whether to copy all tags from the source DB snapshot to the target DB snapshot. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsCopyTags :: Lens.Lens' CopyDBSnapshot (Lude.Maybe Lude.Bool)
cdsCopyTags = Lens.lens (copyTags :: CopyDBSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {copyTags = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsCopyTags "Use generic-lens or generic-optics with 'copyTags' instead." #-}

-- | The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.
-- If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.
-- If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.
-- If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsKMSKeyId :: Lens.Lens' CopyDBSnapshot (Lude.Maybe Lude.Text)
cdsKMSKeyId = Lens.lens (kmsKeyId :: CopyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of an option group to associate with the copy of the snapshot.
--
-- Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsOptionGroupName :: Lens.Lens' CopyDBSnapshot (Lude.Maybe Lude.Text)
cdsOptionGroupName = Lens.lens (optionGroupName :: CopyDBSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTags :: Lens.Lens' CopyDBSnapshot (Lude.Maybe [Tag])
cdsTags = Lens.lens (tags :: CopyDBSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
cdsSourceDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Lude.Text
cdsSourceDBSnapshotIdentifier = Lens.lens (sourceDBSnapshotIdentifier :: CopyDBSnapshot -> Lude.Text) (\s a -> s {sourceDBSnapshotIdentifier = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsSourceDBSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBSnapshotIdentifier' instead." #-}

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
cdsTargetDBSnapshotIdentifier :: Lens.Lens' CopyDBSnapshot Lude.Text
cdsTargetDBSnapshotIdentifier = Lens.lens (targetDBSnapshotIdentifier :: CopyDBSnapshot -> Lude.Text) (\s a -> s {targetDBSnapshotIdentifier = a} :: CopyDBSnapshot)
{-# DEPRECATED cdsTargetDBSnapshotIdentifier "Use generic-lens or generic-optics with 'targetDBSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest CopyDBSnapshot where
  type Rs CopyDBSnapshot = CopyDBSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CopyDBSnapshotResult"
      ( \s h x ->
          CopyDBSnapshotResponse'
            Lude.<$> (x Lude..@? "DBSnapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyDBSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyDBSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyDBSnapshot where
  toQuery CopyDBSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyDBSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "TargetCustomAvailabilityZone"
          Lude.=: targetCustomAvailabilityZone,
        "PreSignedUrl" Lude.=: preSignedURL,
        "CopyTags" Lude.=: copyTags,
        "KmsKeyId" Lude.=: kmsKeyId,
        "OptionGroupName" Lude.=: optionGroupName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "SourceDBSnapshotIdentifier" Lude.=: sourceDBSnapshotIdentifier,
        "TargetDBSnapshotIdentifier" Lude.=: targetDBSnapshotIdentifier
      ]

-- | /See:/ 'mkCopyDBSnapshotResponse' smart constructor.
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
  { dbSnapshot ::
      Lude.Maybe DBSnapshot,
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

-- | Creates a value of 'CopyDBSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshot' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCopyDBSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyDBSnapshotResponse
mkCopyDBSnapshotResponse pResponseStatus_ =
  CopyDBSnapshotResponse'
    { dbSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsDBSnapshot :: Lens.Lens' CopyDBSnapshotResponse (Lude.Maybe DBSnapshot)
cdsrsDBSnapshot = Lens.lens (dbSnapshot :: CopyDBSnapshotResponse -> Lude.Maybe DBSnapshot) (\s a -> s {dbSnapshot = a} :: CopyDBSnapshotResponse)
{-# DEPRECATED cdsrsDBSnapshot "Use generic-lens or generic-optics with 'dbSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CopyDBSnapshotResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CopyDBSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyDBSnapshotResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

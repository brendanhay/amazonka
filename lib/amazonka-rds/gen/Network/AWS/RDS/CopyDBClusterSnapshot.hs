{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CopyDBClusterSnapshot (..),
    mkCopyDBClusterSnapshot,

    -- ** Request lenses
    cdbcsTargetDBClusterSnapshotIdentifier,
    cdbcsPreSignedURL,
    cdbcsCopyTags,
    cdbcsKMSKeyId,
    cdbcsSourceDBClusterSnapshotIdentifier,
    cdbcsTags,

    -- * Destructuring the response
    CopyDBClusterSnapshotResponse (..),
    mkCopyDBClusterSnapshotResponse,

    -- ** Response lenses
    cdcsrsDBClusterSnapshot,
    cdcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCopyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { -- | The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter isn't case-sensitive.
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
    targetDBClusterSnapshotIdentifier :: Lude.Text,
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
    preSignedURL :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot. By default, tags are not copied.
    copyTags :: Lude.Maybe Lude.Bool,
    -- | The AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot.
    -- If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .
    -- To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
    -- If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
    kmsKeyId :: Lude.Maybe Lude.Text,
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
    sourceDBClusterSnapshotIdentifier :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'targetDBClusterSnapshotIdentifier' - The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter isn't case-sensitive.
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
-- * 'preSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB cluster snapshot from another AWS Region. Don't specify @PreSignedUrl@ when you are copying an encrypted DB cluster snapshot in the same AWS Region.
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
-- * 'copyTags' - A value that indicates whether to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot. By default, tags are not copied.
-- * 'kmsKeyId' - The AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot.
-- If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
-- * 'sourceDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to copy. This parameter isn't case-sensitive.
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
-- * 'tags' -
mkCopyDBClusterSnapshot ::
  -- | 'targetDBClusterSnapshotIdentifier'
  Lude.Text ->
  -- | 'sourceDBClusterSnapshotIdentifier'
  Lude.Text ->
  CopyDBClusterSnapshot
mkCopyDBClusterSnapshot
  pTargetDBClusterSnapshotIdentifier_
  pSourceDBClusterSnapshotIdentifier_ =
    CopyDBClusterSnapshot'
      { targetDBClusterSnapshotIdentifier =
          pTargetDBClusterSnapshotIdentifier_,
        preSignedURL = Lude.Nothing,
        copyTags = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        sourceDBClusterSnapshotIdentifier =
          pSourceDBClusterSnapshotIdentifier_,
        tags = Lude.Nothing
      }

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
cdbcsTargetDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Lude.Text
cdbcsTargetDBClusterSnapshotIdentifier = Lens.lens (targetDBClusterSnapshotIdentifier :: CopyDBClusterSnapshot -> Lude.Text) (\s a -> s {targetDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsTargetDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'targetDBClusterSnapshotIdentifier' instead." #-}

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
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsPreSignedURL :: Lens.Lens' CopyDBClusterSnapshot (Lude.Maybe Lude.Text)
cdbcsPreSignedURL = Lens.lens (preSignedURL :: CopyDBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

-- | A value that indicates whether to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsCopyTags :: Lens.Lens' CopyDBClusterSnapshot (Lude.Maybe Lude.Bool)
cdbcsCopyTags = Lens.lens (copyTags :: CopyDBClusterSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {copyTags = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsCopyTags "Use generic-lens or generic-optics with 'copyTags' instead." #-}

-- | The AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot.
-- If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .
-- To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsKMSKeyId :: Lens.Lens' CopyDBClusterSnapshot (Lude.Maybe Lude.Text)
cdbcsKMSKeyId = Lens.lens (kmsKeyId :: CopyDBClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

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
cdbcsSourceDBClusterSnapshotIdentifier :: Lens.Lens' CopyDBClusterSnapshot Lude.Text
cdbcsSourceDBClusterSnapshotIdentifier = Lens.lens (sourceDBClusterSnapshotIdentifier :: CopyDBClusterSnapshot -> Lude.Text) (\s a -> s {sourceDBClusterSnapshotIdentifier = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsSourceDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterSnapshotIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsTags :: Lens.Lens' CopyDBClusterSnapshot (Lude.Maybe [Tag])
cdbcsTags = Lens.lens (tags :: CopyDBClusterSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopyDBClusterSnapshot)
{-# DEPRECATED cdbcsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CopyDBClusterSnapshot where
  type Rs CopyDBClusterSnapshot = CopyDBClusterSnapshotResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CopyDBClusterSnapshotResult"
      ( \s h x ->
          CopyDBClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "DBClusterSnapshot")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyDBClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyDBClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyDBClusterSnapshot where
  toQuery CopyDBClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyDBClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "TargetDBClusterSnapshotIdentifier"
          Lude.=: targetDBClusterSnapshotIdentifier,
        "PreSignedUrl" Lude.=: preSignedURL,
        "CopyTags" Lude.=: copyTags,
        "KmsKeyId" Lude.=: kmsKeyId,
        "SourceDBClusterSnapshotIdentifier"
          Lude.=: sourceDBClusterSnapshotIdentifier,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkCopyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Lude.Maybe DBClusterSnapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshot' -
-- * 'responseStatus' - The response status code.
mkCopyDBClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyDBClusterSnapshotResponse
mkCopyDBClusterSnapshotResponse pResponseStatus_ =
  CopyDBClusterSnapshotResponse'
    { dbClusterSnapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcsrsDBClusterSnapshot :: Lens.Lens' CopyDBClusterSnapshotResponse (Lude.Maybe DBClusterSnapshot)
cdcsrsDBClusterSnapshot = Lens.lens (dbClusterSnapshot :: CopyDBClusterSnapshotResponse -> Lude.Maybe DBClusterSnapshot) (\s a -> s {dbClusterSnapshot = a} :: CopyDBClusterSnapshotResponse)
{-# DEPRECATED cdcsrsDBClusterSnapshot "Use generic-lens or generic-optics with 'dbClusterSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcsrsResponseStatus :: Lens.Lens' CopyDBClusterSnapshotResponse Lude.Int
cdcsrsResponseStatus = Lens.lens (responseStatus :: CopyDBClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyDBClusterSnapshotResponse)
{-# DEPRECATED cdcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

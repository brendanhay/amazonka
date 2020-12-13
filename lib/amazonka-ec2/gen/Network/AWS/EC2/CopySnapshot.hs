{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a point-in-time snapshot of an EBS volume and stores it in Amazon S3. You can copy the snapshot within the same Region or from one Region to another. You can use the snapshot to create EBS volumes or Amazon Machine Images (AMIs).
--
-- Copies of encrypted EBS snapshots remain encrypted. Copies of unencrypted snapshots remain unencrypted, unless you enable encryption for the snapshot copy operation. By default, encrypted snapshot copies use the default AWS Key Management Service (AWS KMS) customer master key (CMK); however, you can specify a different CMK.
-- To copy an encrypted snapshot that has been shared from another account, you must have permissions for the CMK used to encrypt the snapshot.
-- Snapshots created by copying another snapshot have an arbitrary volume ID that should not be used for any purpose.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CopySnapshot
  ( -- * Creating a request
    CopySnapshot (..),
    mkCopySnapshot,

    -- ** Request lenses
    csfSourceSnapshotId,
    csfSourceRegion,
    csfPresignedURL,
    csfEncrypted,
    csfTagSpecifications,
    csfDestinationRegion,
    csfKMSKeyId,
    csfDescription,
    csfDryRun,

    -- * Destructuring the response
    CopySnapshotResponse (..),
    mkCopySnapshotResponse,

    -- ** Response lenses
    crsTags,
    crsSnapshotId,
    crsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The ID of the EBS snapshot to copy.
    sourceSnapshotId :: Lude.Text,
    -- | The ID of the Region that contains the snapshot to be copied.
    sourceRegion :: Lude.Text,
    -- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> .
    --
    -- The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
    presignedURL :: Lude.Maybe Lude.Text,
    -- | To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The tags to apply to the new snapshot.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required.
    --
    -- The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
    destinationRegion :: Lude.Maybe Lude.Text,
    -- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
    --
    -- You can specify the CMK using any of the following:
    --
    --     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    --
    --     * Key alias. For example, alias/ExampleAlias.
    --
    --
    --     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    --
    --     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
    --
    --
    -- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A description for the EBS snapshot.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- * 'sourceSnapshotId' - The ID of the EBS snapshot to copy.
-- * 'sourceRegion' - The ID of the Region that contains the snapshot to be copied.
-- * 'presignedURL' - When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> .
--
-- The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
-- * 'encrypted' - To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'tagSpecifications' - The tags to apply to the new snapshot.
-- * 'destinationRegion' - The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required.
--
-- The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
-- * 'kmsKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- * 'description' - A description for the EBS snapshot.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCopySnapshot ::
  -- | 'sourceSnapshotId'
  Lude.Text ->
  -- | 'sourceRegion'
  Lude.Text ->
  CopySnapshot
mkCopySnapshot pSourceSnapshotId_ pSourceRegion_ =
  CopySnapshot'
    { sourceSnapshotId = pSourceSnapshotId_,
      sourceRegion = pSourceRegion_,
      presignedURL = Lude.Nothing,
      encrypted = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      destinationRegion = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the EBS snapshot to copy.
--
-- /Note:/ Consider using 'sourceSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSourceSnapshotId :: Lens.Lens' CopySnapshot Lude.Text
csfSourceSnapshotId = Lens.lens (sourceSnapshotId :: CopySnapshot -> Lude.Text) (\s a -> s {sourceSnapshotId = a} :: CopySnapshot)
{-# DEPRECATED csfSourceSnapshotId "Use generic-lens or generic-optics with 'sourceSnapshotId' instead." #-}

-- | The ID of the Region that contains the snapshot to be copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSourceRegion :: Lens.Lens' CopySnapshot Lude.Text
csfSourceRegion = Lens.lens (sourceRegion :: CopySnapshot -> Lude.Text) (\s a -> s {sourceRegion = a} :: CopySnapshot)
{-# DEPRECATED csfSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> .
--
-- The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
--
-- /Note:/ Consider using 'presignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPresignedURL :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csfPresignedURL = Lens.lens (presignedURL :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {presignedURL = a} :: CopySnapshot)
{-# DEPRECATED csfPresignedURL "Use generic-lens or generic-optics with 'presignedURL' instead." #-}

-- | To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEncrypted :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Bool)
csfEncrypted = Lens.lens (encrypted :: CopySnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CopySnapshot)
{-# DEPRECATED csfEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the new snapshot.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTagSpecifications :: Lens.Lens' CopySnapshot (Lude.Maybe [TagSpecification])
csfTagSpecifications = Lens.lens (tagSpecifications :: CopySnapshot -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CopySnapshot)
{-# DEPRECATED csfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required.
--
-- The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDestinationRegion :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csfDestinationRegion = Lens.lens (destinationRegion :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {destinationRegion = a} :: CopySnapshot)
{-# DEPRECATED csfDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfKMSKeyId :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csfKMSKeyId = Lens.lens (kmsKeyId :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CopySnapshot)
{-# DEPRECATED csfKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A description for the EBS snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDescription :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csfDescription = Lens.lens (description :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CopySnapshot)
{-# DEPRECATED csfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDryRun :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Bool)
csfDryRun = Lens.lens (dryRun :: CopySnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CopySnapshot)
{-# DEPRECATED csfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CopySnapshotResponse'
            Lude.<$> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "snapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopySnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopySnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopySnapshot where
  toQuery CopySnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopySnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "SourceSnapshotId" Lude.=: sourceSnapshotId,
        "SourceRegion" Lude.=: sourceRegion,
        "PresignedUrl" Lude.=: presignedURL,
        "Encrypted" Lude.=: encrypted,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DestinationRegion" Lude.=: destinationRegion,
        "KmsKeyId" Lude.=: kmsKeyId,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | Any tags applied to the new snapshot.
    tags :: Lude.Maybe [Tag],
    -- | The ID of the new snapshot.
    snapshotId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- * 'tags' - Any tags applied to the new snapshot.
-- * 'snapshotId' - The ID of the new snapshot.
-- * 'responseStatus' - The response status code.
mkCopySnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopySnapshotResponse
mkCopySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    { tags = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any tags applied to the new snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTags :: Lens.Lens' CopySnapshotResponse (Lude.Maybe [Tag])
crsTags = Lens.lens (tags :: CopySnapshotResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CopySnapshotResponse)
{-# DEPRECATED crsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the new snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSnapshotId :: Lens.Lens' CopySnapshotResponse (Lude.Maybe Lude.Text)
crsSnapshotId = Lens.lens (snapshotId :: CopySnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CopySnapshotResponse)
{-# DEPRECATED crsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CopySnapshotResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CopySnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopySnapshotResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

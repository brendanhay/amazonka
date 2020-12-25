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
    csfSourceRegion,
    csfSourceSnapshotId,
    csfDescription,
    csfDestinationRegion,
    csfDryRun,
    csfEncrypted,
    csfKmsKeyId,
    csfPresignedUrl,
    csfTagSpecifications,

    -- * Destructuring the response
    CopySnapshotResponse (..),
    mkCopySnapshotResponse,

    -- ** Response lenses
    csrfrsSnapshotId,
    csrfrsTags,
    csrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The ID of the Region that contains the snapshot to be copied.
    sourceRegion :: Types.String,
    -- | The ID of the EBS snapshot to copy.
    sourceSnapshotId :: Types.String,
    -- | A description for the EBS snapshot.
    description :: Core.Maybe Types.String,
    -- | The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required.
    --
    -- The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
    destinationRegion :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Core.Maybe Core.Bool,
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
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> .
    --
    -- The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
    presignedUrl :: Core.Maybe Types.String,
    -- | The tags to apply to the new snapshot.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopySnapshot' value with any optional fields omitted.
mkCopySnapshot ::
  -- | 'sourceRegion'
  Types.String ->
  -- | 'sourceSnapshotId'
  Types.String ->
  CopySnapshot
mkCopySnapshot sourceRegion sourceSnapshotId =
  CopySnapshot'
    { sourceRegion,
      sourceSnapshotId,
      description = Core.Nothing,
      destinationRegion = Core.Nothing,
      dryRun = Core.Nothing,
      encrypted = Core.Nothing,
      kmsKeyId = Core.Nothing,
      presignedUrl = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the Region that contains the snapshot to be copied.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSourceRegion :: Lens.Lens' CopySnapshot Types.String
csfSourceRegion = Lens.field @"sourceRegion"
{-# DEPRECATED csfSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | The ID of the EBS snapshot to copy.
--
-- /Note:/ Consider using 'sourceSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSourceSnapshotId :: Lens.Lens' CopySnapshot Types.String
csfSourceSnapshotId = Lens.field @"sourceSnapshotId"
{-# DEPRECATED csfSourceSnapshotId "Use generic-lens or generic-optics with 'sourceSnapshotId' instead." #-}

-- | A description for the EBS snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDescription :: Lens.Lens' CopySnapshot (Core.Maybe Types.String)
csfDescription = Lens.field @"description"
{-# DEPRECATED csfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required.
--
-- The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDestinationRegion :: Lens.Lens' CopySnapshot (Core.Maybe Types.String)
csfDestinationRegion = Lens.field @"destinationRegion"
{-# DEPRECATED csfDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDryRun :: Lens.Lens' CopySnapshot (Core.Maybe Core.Bool)
csfDryRun = Lens.field @"dryRun"
{-# DEPRECATED csfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEncrypted :: Lens.Lens' CopySnapshot (Core.Maybe Core.Bool)
csfEncrypted = Lens.field @"encrypted"
{-# DEPRECATED csfEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

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
csfKmsKeyId :: Lens.Lens' CopySnapshot (Core.Maybe Types.KmsKeyId)
csfKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED csfKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> .
--
-- The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
--
-- /Note:/ Consider using 'presignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfPresignedUrl :: Lens.Lens' CopySnapshot (Core.Maybe Types.String)
csfPresignedUrl = Lens.field @"presignedUrl"
{-# DEPRECATED csfPresignedUrl "Use generic-lens or generic-optics with 'presignedUrl' instead." #-}

-- | The tags to apply to the new snapshot.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTagSpecifications :: Lens.Lens' CopySnapshot (Core.Maybe [Types.TagSpecification])
csfTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED csfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
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
            ( Core.pure ("Action", "CopySnapshot")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SourceRegion" sourceRegion)
                Core.<> (Core.toQueryValue "SourceSnapshotId" sourceSnapshotId)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DestinationRegion" Core.<$> destinationRegion)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Encrypted" Core.<$> encrypted)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "PresignedUrl" Core.<$> presignedUrl)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CopySnapshotResponse'
            Core.<$> (x Core..@? "snapshotId")
            Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | The ID of the new snapshot.
    snapshotId :: Core.Maybe Types.String,
    -- | Any tags applied to the new snapshot.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopySnapshotResponse' value with any optional fields omitted.
mkCopySnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopySnapshotResponse
mkCopySnapshotResponse responseStatus =
  CopySnapshotResponse'
    { snapshotId = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ID of the new snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsSnapshotId :: Lens.Lens' CopySnapshotResponse (Core.Maybe Types.String)
csrfrsSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED csrfrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | Any tags applied to the new snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsTags :: Lens.Lens' CopySnapshotResponse (Core.Maybe [Types.Tag])
csrfrsTags = Lens.field @"tags"
{-# DEPRECATED csrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsResponseStatus :: Lens.Lens' CopySnapshotResponse Core.Int
csrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

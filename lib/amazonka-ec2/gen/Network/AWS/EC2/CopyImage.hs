{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the copy of an AMI from the specified source Region to the current Region. You specify the destination Region by using its endpoint when making the request.
--
-- Copies of encrypted backing snapshots for the AMI are encrypted. Copies of unencrypted backing snapshots remain unencrypted, unless you set @Encrypted@ during the copy operation. You cannot create an unencrypted copy of an encrypted backing snapshot.
-- For more information about the prerequisites and limits when copying an AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/CopyingAMIs.html Copying an AMI> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CopyImage
  ( -- * Creating a request
    CopyImage (..),
    mkCopyImage,

    -- ** Request lenses
    ciClientToken,
    ciSourceRegion,
    ciEncrypted,
    ciKMSKeyId,
    ciSourceImageId,
    ciName,
    ciDescription,
    ciDryRun,

    -- * Destructuring the response
    CopyImageResponse (..),
    mkCopyImageResponse,

    -- ** Response lenses
    cifrsImageId,
    cifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CopyImage.
--
-- /See:/ 'mkCopyImage' smart constructor.
data CopyImage = CopyImage'
  { -- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The name of the Region that contains the AMI to copy.
    sourceRegion :: Lude.Text,
    -- | Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ .
    --
    -- You can specify a CMK using any of the following:
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
    -- AWS authenticates the CMK asynchronously. Therefore, if you specify an identifier that is not valid, the action can appear to complete, but eventually fails.
    -- The specified CMK must exist in the destination Region.
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The ID of the AMI to copy.
    sourceImageId :: Lude.Text,
    -- | The name of the new AMI in the destination Region.
    name :: Lude.Text,
    -- | A description for the new AMI in the destination Region.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'sourceRegion' - The name of the Region that contains the AMI to copy.
-- * 'encrypted' - Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'kmsKeyId' - The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ .
--
-- You can specify a CMK using any of the following:
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
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an identifier that is not valid, the action can appear to complete, but eventually fails.
-- The specified CMK must exist in the destination Region.
-- Amazon EBS does not support asymmetric CMKs.
-- * 'sourceImageId' - The ID of the AMI to copy.
-- * 'name' - The name of the new AMI in the destination Region.
-- * 'description' - A description for the new AMI in the destination Region.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCopyImage ::
  -- | 'sourceRegion'
  Lude.Text ->
  -- | 'sourceImageId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CopyImage
mkCopyImage pSourceRegion_ pSourceImageId_ pName_ =
  CopyImage'
    { clientToken = Lude.Nothing,
      sourceRegion = pSourceRegion_,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      sourceImageId = pSourceImageId_,
      name = pName_,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciClientToken :: Lens.Lens' CopyImage (Lude.Maybe Lude.Text)
ciClientToken = Lens.lens (clientToken :: CopyImage -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CopyImage)
{-# DEPRECATED ciClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the Region that contains the AMI to copy.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceRegion :: Lens.Lens' CopyImage Lude.Text
ciSourceRegion = Lens.lens (sourceRegion :: CopyImage -> Lude.Text) (\s a -> s {sourceRegion = a} :: CopyImage)
{-# DEPRECATED ciSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

-- | Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEncrypted :: Lens.Lens' CopyImage (Lude.Maybe Lude.Bool)
ciEncrypted = Lens.lens (encrypted :: CopyImage -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CopyImage)
{-# DEPRECATED ciEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ .
--
-- You can specify a CMK using any of the following:
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
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an identifier that is not valid, the action can appear to complete, but eventually fails.
-- The specified CMK must exist in the destination Region.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciKMSKeyId :: Lens.Lens' CopyImage (Lude.Maybe Lude.Text)
ciKMSKeyId = Lens.lens (kmsKeyId :: CopyImage -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CopyImage)
{-# DEPRECATED ciKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ID of the AMI to copy.
--
-- /Note:/ Consider using 'sourceImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceImageId :: Lens.Lens' CopyImage Lude.Text
ciSourceImageId = Lens.lens (sourceImageId :: CopyImage -> Lude.Text) (\s a -> s {sourceImageId = a} :: CopyImage)
{-# DEPRECATED ciSourceImageId "Use generic-lens or generic-optics with 'sourceImageId' instead." #-}

-- | The name of the new AMI in the destination Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' CopyImage Lude.Text
ciName = Lens.lens (name :: CopyImage -> Lude.Text) (\s a -> s {name = a} :: CopyImage)
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description for the new AMI in the destination Region.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDescription :: Lens.Lens' CopyImage (Lude.Maybe Lude.Text)
ciDescription = Lens.lens (description :: CopyImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CopyImage)
{-# DEPRECATED ciDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDryRun :: Lens.Lens' CopyImage (Lude.Maybe Lude.Bool)
ciDryRun = Lens.lens (dryRun :: CopyImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CopyImage)
{-# DEPRECATED ciDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CopyImage where
  type Rs CopyImage = CopyImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CopyImageResponse'
            Lude.<$> (x Lude..@? "imageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyImage where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyImage where
  toQuery CopyImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "SourceRegion" Lude.=: sourceRegion,
        "Encrypted" Lude.=: encrypted,
        "KmsKeyId" Lude.=: kmsKeyId,
        "SourceImageId" Lude.=: sourceImageId,
        "Name" Lude.=: name,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of CopyImage.
--
-- /See:/ 'mkCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { -- | The ID of the new AMI.
    imageId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The ID of the new AMI.
-- * 'responseStatus' - The response status code.
mkCopyImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyImageResponse
mkCopyImageResponse pResponseStatus_ =
  CopyImageResponse'
    { imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the new AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifrsImageId :: Lens.Lens' CopyImageResponse (Lude.Maybe Lude.Text)
cifrsImageId = Lens.lens (imageId :: CopyImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: CopyImageResponse)
{-# DEPRECATED cifrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifrsResponseStatus :: Lens.Lens' CopyImageResponse Lude.Int
cifrsResponseStatus = Lens.lens (responseStatus :: CopyImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyImageResponse)
{-# DEPRECATED cifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

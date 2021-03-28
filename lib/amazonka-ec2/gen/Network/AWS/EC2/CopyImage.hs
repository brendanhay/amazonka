{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CopyImage (..)
    , mkCopyImage
    -- ** Request lenses
    , ciName
    , ciSourceImageId
    , ciSourceRegion
    , ciClientToken
    , ciDescription
    , ciDryRun
    , ciEncrypted
    , ciKmsKeyId

    -- * Destructuring the response
    , CopyImageResponse (..)
    , mkCopyImageResponse
    -- ** Response lenses
    , cirrsImageId
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CopyImage.
--
-- /See:/ 'mkCopyImage' smart constructor.
data CopyImage = CopyImage'
  { name :: Core.Text
    -- ^ The name of the new AMI in the destination Region.
  , sourceImageId :: Core.Text
    -- ^ The ID of the AMI to copy.
  , sourceRegion :: Core.Text
    -- ^ The name of the Region that contains the AMI to copy.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
  , description :: Core.Maybe Core.Text
    -- ^ A description for the new AMI in the destination Region.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The identifier of the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating encrypted volumes. If this parameter is not specified, your AWS managed CMK for EBS is used. If you specify a CMK, you must also set the encrypted state to @true@ .
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImage' value with any optional fields omitted.
mkCopyImage
    :: Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'sourceImageId'
    -> Core.Text -- ^ 'sourceRegion'
    -> CopyImage
mkCopyImage name sourceImageId sourceRegion
  = CopyImage'{name, sourceImageId, sourceRegion,
               clientToken = Core.Nothing, description = Core.Nothing,
               dryRun = Core.Nothing, encrypted = Core.Nothing,
               kmsKeyId = Core.Nothing}

-- | The name of the new AMI in the destination Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' CopyImage Core.Text
ciName = Lens.field @"name"
{-# INLINEABLE ciName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the AMI to copy.
--
-- /Note:/ Consider using 'sourceImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceImageId :: Lens.Lens' CopyImage Core.Text
ciSourceImageId = Lens.field @"sourceImageId"
{-# INLINEABLE ciSourceImageId #-}
{-# DEPRECATED sourceImageId "Use generic-lens or generic-optics with 'sourceImageId' instead"  #-}

-- | The name of the Region that contains the AMI to copy.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSourceRegion :: Lens.Lens' CopyImage Core.Text
ciSourceRegion = Lens.field @"sourceRegion"
{-# INLINEABLE ciSourceRegion #-}
{-# DEPRECATED sourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead"  #-}

-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciClientToken :: Lens.Lens' CopyImage (Core.Maybe Core.Text)
ciClientToken = Lens.field @"clientToken"
{-# INLINEABLE ciClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A description for the new AMI in the destination Region.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDescription :: Lens.Lens' CopyImage (Core.Maybe Core.Text)
ciDescription = Lens.field @"description"
{-# INLINEABLE ciDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDryRun :: Lens.Lens' CopyImage (Core.Maybe Core.Bool)
ciDryRun = Lens.field @"dryRun"
{-# INLINEABLE ciDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specifies whether the destination snapshots of the copied image should be encrypted. You can encrypt a copy of an unencrypted snapshot, but you cannot create an unencrypted copy of an encrypted snapshot. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciEncrypted :: Lens.Lens' CopyImage (Core.Maybe Core.Bool)
ciEncrypted = Lens.field @"encrypted"
{-# INLINEABLE ciEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

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
ciKmsKeyId :: Lens.Lens' CopyImage (Core.Maybe Types.KmsKeyId)
ciKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ciKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.ToQuery CopyImage where
        toQuery CopyImage{..}
          = Core.toQueryPair "Action" ("CopyImage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
              Core.<> Core.toQueryPair "SourceImageId" sourceImageId
              Core.<> Core.toQueryPair "SourceRegion" sourceRegion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId

instance Core.ToHeaders CopyImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CopyImage where
        type Rs CopyImage = CopyImageResponse
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
          = Response.receiveXML
              (\ s h x ->
                 CopyImageResponse' Core.<$>
                   (x Core..@? "imageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CopyImage.
--
-- /See:/ 'mkCopyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { imageId :: Core.Maybe Core.Text
    -- ^ The ID of the new AMI.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyImageResponse' value with any optional fields omitted.
mkCopyImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CopyImageResponse
mkCopyImageResponse responseStatus
  = CopyImageResponse'{imageId = Core.Nothing, responseStatus}

-- | The ID of the new AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsImageId :: Lens.Lens' CopyImageResponse (Core.Maybe Core.Text)
cirrsImageId = Lens.field @"imageId"
{-# INLINEABLE cirrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CopyImageResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

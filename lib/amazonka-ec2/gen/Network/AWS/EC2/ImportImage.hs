{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an Amazon Machine Image (AMI). For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html Importing a VM as an Image Using VM Import/Export> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.ImportImage
    (
    -- * Creating a request
      ImportImage (..)
    , mkImportImage
    -- ** Request lenses
    , iifArchitecture
    , iifClientData
    , iifClientToken
    , iifDescription
    , iifDiskContainers
    , iifDryRun
    , iifEncrypted
    , iifHypervisor
    , iifKmsKeyId
    , iifLicenseSpecifications
    , iifLicenseType
    , iifPlatform
    , iifRoleName
    , iifTagSpecifications

    -- * Destructuring the response
    , ImportImageResponse (..)
    , mkImportImageResponse
    -- ** Response lenses
    , irsArchitecture
    , irsDescription
    , irsEncrypted
    , irsHypervisor
    , irsImageId
    , irsImportTaskId
    , irsKmsKeyId
    , irsLicenseSpecifications
    , irsLicenseType
    , irsPlatform
    , irsProgress
    , irsSnapshotDetails
    , irsStatus
    , irsStatusMessage
    , irsTags
    , irsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportImage' smart constructor.
data ImportImage = ImportImage'
  { architecture :: Core.Maybe Core.Text
    -- ^ The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@ 
  , clientData :: Core.Maybe Types.ClientData
    -- ^ The client-specific data.
  , clientToken :: Core.Maybe Core.Text
    -- ^ The token to enable idempotency for VM import requests.
  , description :: Core.Maybe Core.Text
    -- ^ A description string for the import image task.
  , diskContainers :: Core.Maybe [Types.ImageDiskContainer]
    -- ^ Information about the disk containers.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
  , hypervisor :: Core.Maybe Core.Text
    -- ^ The target hypervisor platform.
--
-- Valid values: @xen@ 
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted AMI. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set. 
--
-- The CMK identifier may be provided in any of the following formats: 
--
--     * Key ID
--
--
--     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
--     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
--
--
--     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ . 
--
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure. 
-- The specified CMK must exist in the Region that the AMI is being copied to.
-- Amazon EBS does not support asymmetric CMKs.
  , licenseSpecifications :: Core.Maybe [Types.ImportImageLicenseConfigurationRequest]
    -- ^ The ARNs of the license configurations.
  , licenseType :: Core.Maybe Core.Text
    -- ^ The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
  , platform :: Core.Maybe Core.Text
    -- ^ The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@ 
  , roleName :: Core.Maybe Core.Text
    -- ^ The name of the role to use when not using the default role, 'vmimport'.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the image being imported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportImage' value with any optional fields omitted.
mkImportImage
    :: ImportImage
mkImportImage
  = ImportImage'{architecture = Core.Nothing,
                 clientData = Core.Nothing, clientToken = Core.Nothing,
                 description = Core.Nothing, diskContainers = Core.Nothing,
                 dryRun = Core.Nothing, encrypted = Core.Nothing,
                 hypervisor = Core.Nothing, kmsKeyId = Core.Nothing,
                 licenseSpecifications = Core.Nothing, licenseType = Core.Nothing,
                 platform = Core.Nothing, roleName = Core.Nothing,
                 tagSpecifications = Core.Nothing}

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@ 
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifArchitecture :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifArchitecture = Lens.field @"architecture"
{-# INLINEABLE iifArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifClientData :: Lens.Lens' ImportImage (Core.Maybe Types.ClientData)
iifClientData = Lens.field @"clientData"
{-# INLINEABLE iifClientData #-}
{-# DEPRECATED clientData "Use generic-lens or generic-optics with 'clientData' instead"  #-}

-- | The token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifClientToken :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifClientToken = Lens.field @"clientToken"
{-# INLINEABLE iifClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | A description string for the import image task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDescription :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifDescription = Lens.field @"description"
{-# INLINEABLE iifDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the disk containers.
--
-- /Note:/ Consider using 'diskContainers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDiskContainers :: Lens.Lens' ImportImage (Core.Maybe [Types.ImageDiskContainer])
iifDiskContainers = Lens.field @"diskContainers"
{-# INLINEABLE iifDiskContainers #-}
{-# DEPRECATED diskContainers "Use generic-lens or generic-optics with 'diskContainers' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDryRun :: Lens.Lens' ImportImage (Core.Maybe Core.Bool)
iifDryRun = Lens.field @"dryRun"
{-# INLINEABLE iifDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifEncrypted :: Lens.Lens' ImportImage (Core.Maybe Core.Bool)
iifEncrypted = Lens.field @"encrypted"
{-# INLINEABLE iifEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The target hypervisor platform.
--
-- Valid values: @xen@ 
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifHypervisor :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifHypervisor = Lens.field @"hypervisor"
{-# INLINEABLE iifHypervisor #-}
{-# DEPRECATED hypervisor "Use generic-lens or generic-optics with 'hypervisor' instead"  #-}

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted AMI. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set. 
--
-- The CMK identifier may be provided in any of the following formats: 
--
--     * Key ID
--
--
--     * Key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .
--
--
--     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .
--
--
--     * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the Region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ . 
--
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. This action will eventually report failure. 
-- The specified CMK must exist in the Region that the AMI is being copied to.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifKmsKeyId :: Lens.Lens' ImportImage (Core.Maybe Types.KmsKeyId)
iifKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE iifKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ARNs of the license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifLicenseSpecifications :: Lens.Lens' ImportImage (Core.Maybe [Types.ImportImageLicenseConfigurationRequest])
iifLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# INLINEABLE iifLicenseSpecifications #-}
{-# DEPRECATED licenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead"  #-}

-- | The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifLicenseType :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifLicenseType = Lens.field @"licenseType"
{-# INLINEABLE iifLicenseType #-}
{-# DEPRECATED licenseType "Use generic-lens or generic-optics with 'licenseType' instead"  #-}

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@ 
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifPlatform :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifPlatform = Lens.field @"platform"
{-# INLINEABLE iifPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifRoleName :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
iifRoleName = Lens.field @"roleName"
{-# INLINEABLE iifRoleName #-}
{-# DEPRECATED roleName "Use generic-lens or generic-optics with 'roleName' instead"  #-}

-- | The tags to apply to the image being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifTagSpecifications :: Lens.Lens' ImportImage (Core.Maybe [Types.TagSpecification])
iifTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE iifTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery ImportImage where
        toQuery ImportImage{..}
          = Core.toQueryPair "Action" ("ImportImage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Architecture")
                architecture
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientData") clientData
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "DiskContainer")
                diskContainers
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Encrypted") encrypted
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Hypervisor") hypervisor
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "LicenseSpecifications")
                licenseSpecifications
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LicenseType") licenseType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Platform") platform
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RoleName") roleName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders ImportImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ImportImage where
        type Rs ImportImage = ImportImageResponse
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
                 ImportImageResponse' Core.<$>
                   (x Core..@? "architecture") Core.<*> x Core..@? "description"
                     Core.<*> x Core..@? "encrypted"
                     Core.<*> x Core..@? "hypervisor"
                     Core.<*> x Core..@? "imageId"
                     Core.<*> x Core..@? "importTaskId"
                     Core.<*> x Core..@? "kmsKeyId"
                     Core.<*>
                     x Core..@? "licenseSpecifications" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> x Core..@? "licenseType"
                     Core.<*> x Core..@? "platform"
                     Core.<*> x Core..@? "progress"
                     Core.<*>
                     x Core..@? "snapshotDetailSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "status"
                     Core.<*> x Core..@? "statusMessage"
                     Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { architecture :: Core.Maybe Core.Text
    -- ^ The architecture of the virtual machine.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the import task.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the AMI is encrypted.
  , hypervisor :: Core.Maybe Core.Text
    -- ^ The target hypervisor of the import task.
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the Amazon Machine Image (AMI) created by the import task.
  , importTaskId :: Core.Maybe Types.ImportImageTaskId
    -- ^ The task ID of the import image task.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
  , licenseSpecifications :: Core.Maybe [Types.ImportImageLicenseConfigurationResponse]
    -- ^ The ARNs of the license configurations.
  , licenseType :: Core.Maybe Core.Text
    -- ^ The license type of the virtual machine.
  , platform :: Core.Maybe Core.Text
    -- ^ The operating system of the virtual machine.
  , progress :: Core.Maybe Core.Text
    -- ^ The progress of the task.
  , snapshotDetails :: Core.Maybe [Types.SnapshotDetail]
    -- ^ Information about the snapshots.
  , status :: Core.Maybe Core.Text
    -- ^ A brief status of the task.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A detailed status message of the import task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the image being imported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportImageResponse' value with any optional fields omitted.
mkImportImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportImageResponse
mkImportImageResponse responseStatus
  = ImportImageResponse'{architecture = Core.Nothing,
                         description = Core.Nothing, encrypted = Core.Nothing,
                         hypervisor = Core.Nothing, imageId = Core.Nothing,
                         importTaskId = Core.Nothing, kmsKeyId = Core.Nothing,
                         licenseSpecifications = Core.Nothing, licenseType = Core.Nothing,
                         platform = Core.Nothing, progress = Core.Nothing,
                         snapshotDetails = Core.Nothing, status = Core.Nothing,
                         statusMessage = Core.Nothing, tags = Core.Nothing, responseStatus}

-- | The architecture of the virtual machine.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsArchitecture :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsArchitecture = Lens.field @"architecture"
{-# INLINEABLE irsArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | A description of the import task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsDescription :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsDescription = Lens.field @"description"
{-# INLINEABLE irsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the AMI is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsEncrypted :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Bool)
irsEncrypted = Lens.field @"encrypted"
{-# INLINEABLE irsEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The target hypervisor of the import task.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsHypervisor :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsHypervisor = Lens.field @"hypervisor"
{-# INLINEABLE irsHypervisor #-}
{-# DEPRECATED hypervisor "Use generic-lens or generic-optics with 'hypervisor' instead"  #-}

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsImageId :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsImageId = Lens.field @"imageId"
{-# INLINEABLE irsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The task ID of the import image task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsImportTaskId :: Lens.Lens' ImportImageResponse (Core.Maybe Types.ImportImageTaskId)
irsImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE irsImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsKmsKeyId :: Lens.Lens' ImportImageResponse (Core.Maybe Types.KmsKeyId)
irsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE irsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ARNs of the license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsLicenseSpecifications :: Lens.Lens' ImportImageResponse (Core.Maybe [Types.ImportImageLicenseConfigurationResponse])
irsLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# INLINEABLE irsLicenseSpecifications #-}
{-# DEPRECATED licenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead"  #-}

-- | The license type of the virtual machine.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsLicenseType :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsLicenseType = Lens.field @"licenseType"
{-# INLINEABLE irsLicenseType #-}
{-# DEPRECATED licenseType "Use generic-lens or generic-optics with 'licenseType' instead"  #-}

-- | The operating system of the virtual machine.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsPlatform :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsPlatform = Lens.field @"platform"
{-# INLINEABLE irsPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The progress of the task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsProgress :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsProgress = Lens.field @"progress"
{-# INLINEABLE irsProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshotDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsSnapshotDetails :: Lens.Lens' ImportImageResponse (Core.Maybe [Types.SnapshotDetail])
irsSnapshotDetails = Lens.field @"snapshotDetails"
{-# INLINEABLE irsSnapshotDetails #-}
{-# DEPRECATED snapshotDetails "Use generic-lens or generic-optics with 'snapshotDetails' instead"  #-}

-- | A brief status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsStatus :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsStatus = Lens.field @"status"
{-# INLINEABLE irsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A detailed status message of the import task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsStatusMessage :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
irsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE irsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Any tags assigned to the image being imported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsTags :: Lens.Lens' ImportImageResponse (Core.Maybe [Types.Tag])
irsTags = Lens.field @"tags"
{-# INLINEABLE irsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsResponseStatus :: Lens.Lens' ImportImageResponse Core.Int
irsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE irsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

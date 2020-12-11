{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ImportImage (..),
    mkImportImage,

    -- ** Request lenses
    impHypervisor,
    impPlatform,
    impClientToken,
    impLicenseSpecifications,
    impLicenseType,
    impRoleName,
    impEncrypted,
    impTagSpecifications,
    impKMSKeyId,
    impArchitecture,
    impDescription,
    impDryRun,
    impClientData,
    impDiskContainers,

    -- * Destructuring the response
    ImportImageResponse (..),
    mkImportImageResponse,

    -- ** Response lenses
    irsStatus,
    irsHypervisor,
    irsPlatform,
    irsProgress,
    irsLicenseSpecifications,
    irsLicenseType,
    irsSnapshotDetails,
    irsEncrypted,
    irsKMSKeyId,
    irsStatusMessage,
    irsImageId,
    irsImportTaskId,
    irsArchitecture,
    irsDescription,
    irsTags,
    irsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportImage' smart constructor.
data ImportImage = ImportImage'
  { hypervisor :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe Lude.Text,
    clientToken :: Lude.Maybe Lude.Text,
    licenseSpecifications ::
      Lude.Maybe [ImportImageLicenseConfigurationRequest],
    licenseType :: Lude.Maybe Lude.Text,
    roleName :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    kmsKeyId :: Lude.Maybe Lude.Text,
    architecture :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientData :: Lude.Maybe ClientData,
    diskContainers :: Lude.Maybe [ImageDiskContainer]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportImage' with the minimum fields required to make a request.
--
-- * 'architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
-- * 'clientData' - The client-specific data.
-- * 'clientToken' - The token to enable idempotency for VM import requests.
-- * 'description' - A description string for the import image task.
-- * 'diskContainers' - Information about the disk containers.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'encrypted' - Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'hypervisor' - The target hypervisor platform.
--
-- Valid values: @xen@
-- * 'kmsKeyId' - An identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted AMI. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
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
-- * 'licenseSpecifications' - The ARNs of the license configurations.
-- * 'licenseType' - The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
-- * 'platform' - The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
-- * 'roleName' - The name of the role to use when not using the default role, 'vmimport'.
-- * 'tagSpecifications' - The tags to apply to the image being imported.
mkImportImage ::
  ImportImage
mkImportImage =
  ImportImage'
    { hypervisor = Lude.Nothing,
      platform = Lude.Nothing,
      clientToken = Lude.Nothing,
      licenseSpecifications = Lude.Nothing,
      licenseType = Lude.Nothing,
      roleName = Lude.Nothing,
      encrypted = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      architecture = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      clientData = Lude.Nothing,
      diskContainers = Lude.Nothing
    }

-- | The target hypervisor platform.
--
-- Valid values: @xen@
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impHypervisor :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impHypervisor = Lens.lens (hypervisor :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {hypervisor = a} :: ImportImage)
{-# DEPRECATED impHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impPlatform :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impPlatform = Lens.lens (platform :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ImportImage)
{-# DEPRECATED impPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impClientToken :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impClientToken = Lens.lens (clientToken :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ImportImage)
{-# DEPRECATED impClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ARNs of the license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impLicenseSpecifications :: Lens.Lens' ImportImage (Lude.Maybe [ImportImageLicenseConfigurationRequest])
impLicenseSpecifications = Lens.lens (licenseSpecifications :: ImportImage -> Lude.Maybe [ImportImageLicenseConfigurationRequest]) (\s a -> s {licenseSpecifications = a} :: ImportImage)
{-# DEPRECATED impLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impLicenseType :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impLicenseType = Lens.lens (licenseType :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {licenseType = a} :: ImportImage)
{-# DEPRECATED impLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impRoleName :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impRoleName = Lens.lens (roleName :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ImportImage)
{-# DEPRECATED impRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impEncrypted :: Lens.Lens' ImportImage (Lude.Maybe Lude.Bool)
impEncrypted = Lens.lens (encrypted :: ImportImage -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ImportImage)
{-# DEPRECATED impEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the image being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impTagSpecifications :: Lens.Lens' ImportImage (Lude.Maybe [TagSpecification])
impTagSpecifications = Lens.lens (tagSpecifications :: ImportImage -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: ImportImage)
{-# DEPRECATED impTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

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
impKMSKeyId :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impKMSKeyId = Lens.lens (kmsKeyId :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ImportImage)
{-# DEPRECATED impKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impArchitecture :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impArchitecture = Lens.lens (architecture :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {architecture = a} :: ImportImage)
{-# DEPRECATED impArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description string for the import image task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impDescription :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
impDescription = Lens.lens (description :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportImage)
{-# DEPRECATED impDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impDryRun :: Lens.Lens' ImportImage (Lude.Maybe Lude.Bool)
impDryRun = Lens.lens (dryRun :: ImportImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportImage)
{-# DEPRECATED impDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impClientData :: Lens.Lens' ImportImage (Lude.Maybe ClientData)
impClientData = Lens.lens (clientData :: ImportImage -> Lude.Maybe ClientData) (\s a -> s {clientData = a} :: ImportImage)
{-# DEPRECATED impClientData "Use generic-lens or generic-optics with 'clientData' instead." #-}

-- | Information about the disk containers.
--
-- /Note:/ Consider using 'diskContainers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
impDiskContainers :: Lens.Lens' ImportImage (Lude.Maybe [ImageDiskContainer])
impDiskContainers = Lens.lens (diskContainers :: ImportImage -> Lude.Maybe [ImageDiskContainer]) (\s a -> s {diskContainers = a} :: ImportImage)
{-# DEPRECATED impDiskContainers "Use generic-lens or generic-optics with 'diskContainers' instead." #-}

instance Lude.AWSRequest ImportImage where
  type Rs ImportImage = ImportImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ImportImageResponse'
            Lude.<$> (x Lude..@? "status")
            Lude.<*> (x Lude..@? "hypervisor")
            Lude.<*> (x Lude..@? "platform")
            Lude.<*> (x Lude..@? "progress")
            Lude.<*> ( x Lude..@? "licenseSpecifications" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "licenseType")
            Lude.<*> ( x Lude..@? "snapshotDetailSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "encrypted")
            Lude.<*> (x Lude..@? "kmsKeyId")
            Lude.<*> (x Lude..@? "statusMessage")
            Lude.<*> (x Lude..@? "imageId")
            Lude.<*> (x Lude..@? "importTaskId")
            Lude.<*> (x Lude..@? "architecture")
            Lude.<*> (x Lude..@? "description")
            Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportImage where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportImage where
  toQuery ImportImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Hypervisor" Lude.=: hypervisor,
        "Platform" Lude.=: platform,
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          ( Lude.toQueryList "LicenseSpecifications"
              Lude.<$> licenseSpecifications
          ),
        "LicenseType" Lude.=: licenseType,
        "RoleName" Lude.=: roleName,
        "Encrypted" Lude.=: encrypted,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "KmsKeyId" Lude.=: kmsKeyId,
        "Architecture" Lude.=: architecture,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "ClientData" Lude.=: clientData,
        Lude.toQuery
          (Lude.toQueryList "DiskContainer" Lude.<$> diskContainers)
      ]

-- | /See:/ 'mkImportImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { status ::
      Lude.Maybe Lude.Text,
    hypervisor :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe Lude.Text,
    progress :: Lude.Maybe Lude.Text,
    licenseSpecifications ::
      Lude.Maybe
        [ImportImageLicenseConfigurationResponse],
    licenseType :: Lude.Maybe Lude.Text,
    snapshotDetails :: Lude.Maybe [SnapshotDetail],
    encrypted :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe Lude.Text,
    importTaskId :: Lude.Maybe Lude.Text,
    architecture :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'ImportImageResponse' with the minimum fields required to make a request.
--
-- * 'architecture' - The architecture of the virtual machine.
-- * 'description' - A description of the import task.
-- * 'encrypted' - Indicates whether the AMI is encrypted.
-- * 'hypervisor' - The target hypervisor of the import task.
-- * 'imageId' - The ID of the Amazon Machine Image (AMI) created by the import task.
-- * 'importTaskId' - The task ID of the import image task.
-- * 'kmsKeyId' - The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
-- * 'licenseSpecifications' - The ARNs of the license configurations.
-- * 'licenseType' - The license type of the virtual machine.
-- * 'platform' - The operating system of the virtual machine.
-- * 'progress' - The progress of the task.
-- * 'responseStatus' - The response status code.
-- * 'snapshotDetails' - Information about the snapshots.
-- * 'status' - A brief status of the task.
-- * 'statusMessage' - A detailed status message of the import task.
-- * 'tags' - Any tags assigned to the image being imported.
mkImportImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportImageResponse
mkImportImageResponse pResponseStatus_ =
  ImportImageResponse'
    { status = Lude.Nothing,
      hypervisor = Lude.Nothing,
      platform = Lude.Nothing,
      progress = Lude.Nothing,
      licenseSpecifications = Lude.Nothing,
      licenseType = Lude.Nothing,
      snapshotDetails = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      statusMessage = Lude.Nothing,
      imageId = Lude.Nothing,
      importTaskId = Lude.Nothing,
      architecture = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A brief status of the task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsStatus :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsStatus = Lens.lens (status :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ImportImageResponse)
{-# DEPRECATED irsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The target hypervisor of the import task.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsHypervisor :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsHypervisor = Lens.lens (hypervisor :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {hypervisor = a} :: ImportImageResponse)
{-# DEPRECATED irsHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The operating system of the virtual machine.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsPlatform :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsPlatform = Lens.lens (platform :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ImportImageResponse)
{-# DEPRECATED irsPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The progress of the task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsProgress :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsProgress = Lens.lens (progress :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: ImportImageResponse)
{-# DEPRECATED irsProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The ARNs of the license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsLicenseSpecifications :: Lens.Lens' ImportImageResponse (Lude.Maybe [ImportImageLicenseConfigurationResponse])
irsLicenseSpecifications = Lens.lens (licenseSpecifications :: ImportImageResponse -> Lude.Maybe [ImportImageLicenseConfigurationResponse]) (\s a -> s {licenseSpecifications = a} :: ImportImageResponse)
{-# DEPRECATED irsLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The license type of the virtual machine.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsLicenseType :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsLicenseType = Lens.lens (licenseType :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseType = a} :: ImportImageResponse)
{-# DEPRECATED irsLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshotDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsSnapshotDetails :: Lens.Lens' ImportImageResponse (Lude.Maybe [SnapshotDetail])
irsSnapshotDetails = Lens.lens (snapshotDetails :: ImportImageResponse -> Lude.Maybe [SnapshotDetail]) (\s a -> s {snapshotDetails = a} :: ImportImageResponse)
{-# DEPRECATED irsSnapshotDetails "Use generic-lens or generic-optics with 'snapshotDetails' instead." #-}

-- | Indicates whether the AMI is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsEncrypted :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Bool)
irsEncrypted = Lens.lens (encrypted :: ImportImageResponse -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ImportImageResponse)
{-# DEPRECATED irsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsKMSKeyId :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsKMSKeyId = Lens.lens (kmsKeyId :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ImportImageResponse)
{-# DEPRECATED irsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A detailed status message of the import task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsStatusMessage :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsStatusMessage = Lens.lens (statusMessage :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ImportImageResponse)
{-# DEPRECATED irsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsImageId :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsImageId = Lens.lens (imageId :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ImportImageResponse)
{-# DEPRECATED irsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The task ID of the import image task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsImportTaskId :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsImportTaskId = Lens.lens (importTaskId :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: ImportImageResponse)
{-# DEPRECATED irsImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The architecture of the virtual machine.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsArchitecture :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsArchitecture = Lens.lens (architecture :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {architecture = a} :: ImportImageResponse)
{-# DEPRECATED irsArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description of the import task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsDescription :: Lens.Lens' ImportImageResponse (Lude.Maybe Lude.Text)
irsDescription = Lens.lens (description :: ImportImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportImageResponse)
{-# DEPRECATED irsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags assigned to the image being imported.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsTags :: Lens.Lens' ImportImageResponse (Lude.Maybe [Tag])
irsTags = Lens.lens (tags :: ImportImageResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportImageResponse)
{-# DEPRECATED irsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irsResponseStatus :: Lens.Lens' ImportImageResponse Lude.Int
irsResponseStatus = Lens.lens (responseStatus :: ImportImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportImageResponse)
{-# DEPRECATED irsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

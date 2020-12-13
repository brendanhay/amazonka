{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    iifHypervisor,
    iifPlatform,
    iifClientToken,
    iifLicenseSpecifications,
    iifLicenseType,
    iifRoleName,
    iifEncrypted,
    iifTagSpecifications,
    iifKMSKeyId,
    iifArchitecture,
    iifDescription,
    iifDryRun,
    iifClientData,
    iifDiskContainers,

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
  { -- | The target hypervisor platform.
    --
    -- Valid values: @xen@
    hypervisor :: Lude.Maybe Lude.Text,
    -- | The operating system of the virtual machine.
    --
    -- Valid values: @Windows@ | @Linux@
    platform :: Lude.Maybe Lude.Text,
    -- | The token to enable idempotency for VM import requests.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Lude.Maybe [ImportImageLicenseConfigurationRequest],
    -- | The license type to be used for the Amazon Machine Image (AMI) after importing.
    --
    -- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
    -- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
    licenseType :: Lude.Maybe Lude.Text,
    -- | The name of the role to use when not using the default role, 'vmimport'.
    roleName :: Lude.Maybe Lude.Text,
    -- | Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The tags to apply to the image being imported.
    tagSpecifications :: Lude.Maybe [TagSpecification],
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
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Lude.Maybe Lude.Text,
    -- | A description string for the import image task.
    description :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The client-specific data.
    clientData :: Lude.Maybe ClientData,
    -- | Information about the disk containers.
    diskContainers :: Lude.Maybe [ImageDiskContainer]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportImage' with the minimum fields required to make a request.
--
-- * 'hypervisor' - The target hypervisor platform.
--
-- Valid values: @xen@
-- * 'platform' - The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
-- * 'clientToken' - The token to enable idempotency for VM import requests.
-- * 'licenseSpecifications' - The ARNs of the license configurations.
-- * 'licenseType' - The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
-- * 'roleName' - The name of the role to use when not using the default role, 'vmimport'.
-- * 'encrypted' - Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'tagSpecifications' - The tags to apply to the image being imported.
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
-- * 'architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
-- * 'description' - A description string for the import image task.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'clientData' - The client-specific data.
-- * 'diskContainers' - Information about the disk containers.
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
iifHypervisor :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifHypervisor = Lens.lens (hypervisor :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {hypervisor = a} :: ImportImage)
{-# DEPRECATED iifHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifPlatform :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifPlatform = Lens.lens (platform :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ImportImage)
{-# DEPRECATED iifPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The token to enable idempotency for VM import requests.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifClientToken :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifClientToken = Lens.lens (clientToken :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ImportImage)
{-# DEPRECATED iifClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ARNs of the license configurations.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifLicenseSpecifications :: Lens.Lens' ImportImage (Lude.Maybe [ImportImageLicenseConfigurationRequest])
iifLicenseSpecifications = Lens.lens (licenseSpecifications :: ImportImage -> Lude.Maybe [ImportImageLicenseConfigurationRequest]) (\s a -> s {licenseSpecifications = a} :: ImportImage)
{-# DEPRECATED iifLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The license type to be used for the Amazon Machine Image (AMI) after importing.
--
-- By default, we detect the source-system operating system (OS) and apply the appropriate license. Specify @AWS@ to replace the source-system license with an AWS license, if appropriate. Specify @BYOL@ to retain the source-system license, if appropriate.
-- To use @BYOL@ , you must have existing licenses with rights to use these licenses in a third party cloud, such as AWS. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifLicenseType :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifLicenseType = Lens.lens (licenseType :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {licenseType = a} :: ImportImage)
{-# DEPRECATED iifLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The name of the role to use when not using the default role, 'vmimport'.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifRoleName :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifRoleName = Lens.lens (roleName :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: ImportImage)
{-# DEPRECATED iifRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Specifies whether the destination AMI of the imported image should be encrypted. The default CMK for EBS is used unless you specify a non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifEncrypted :: Lens.Lens' ImportImage (Lude.Maybe Lude.Bool)
iifEncrypted = Lens.lens (encrypted :: ImportImage -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ImportImage)
{-# DEPRECATED iifEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the image being imported.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifTagSpecifications :: Lens.Lens' ImportImage (Lude.Maybe [TagSpecification])
iifTagSpecifications = Lens.lens (tagSpecifications :: ImportImage -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: ImportImage)
{-# DEPRECATED iifTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

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
iifKMSKeyId :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifKMSKeyId = Lens.lens (kmsKeyId :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ImportImage)
{-# DEPRECATED iifKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifArchitecture :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifArchitecture = Lens.lens (architecture :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {architecture = a} :: ImportImage)
{-# DEPRECATED iifArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description string for the import image task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDescription :: Lens.Lens' ImportImage (Lude.Maybe Lude.Text)
iifDescription = Lens.lens (description :: ImportImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportImage)
{-# DEPRECATED iifDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDryRun :: Lens.Lens' ImportImage (Lude.Maybe Lude.Bool)
iifDryRun = Lens.lens (dryRun :: ImportImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ImportImage)
{-# DEPRECATED iifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The client-specific data.
--
-- /Note:/ Consider using 'clientData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifClientData :: Lens.Lens' ImportImage (Lude.Maybe ClientData)
iifClientData = Lens.lens (clientData :: ImportImage -> Lude.Maybe ClientData) (\s a -> s {clientData = a} :: ImportImage)
{-# DEPRECATED iifClientData "Use generic-lens or generic-optics with 'clientData' instead." #-}

-- | Information about the disk containers.
--
-- /Note:/ Consider using 'diskContainers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iifDiskContainers :: Lens.Lens' ImportImage (Lude.Maybe [ImageDiskContainer])
iifDiskContainers = Lens.lens (diskContainers :: ImportImage -> Lude.Maybe [ImageDiskContainer]) (\s a -> s {diskContainers = a} :: ImportImage)
{-# DEPRECATED iifDiskContainers "Use generic-lens or generic-optics with 'diskContainers' instead." #-}

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
  { -- | A brief status of the task.
    status :: Lude.Maybe Lude.Text,
    -- | The target hypervisor of the import task.
    hypervisor :: Lude.Maybe Lude.Text,
    -- | The operating system of the virtual machine.
    platform :: Lude.Maybe Lude.Text,
    -- | The progress of the task.
    progress :: Lude.Maybe Lude.Text,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Lude.Maybe [ImportImageLicenseConfigurationResponse],
    -- | The license type of the virtual machine.
    licenseType :: Lude.Maybe Lude.Text,
    -- | Information about the snapshots.
    snapshotDetails :: Lude.Maybe [SnapshotDetail],
    -- | Indicates whether the AMI is encrypted.
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A detailed status message of the import task.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Machine Image (AMI) created by the import task.
    imageId :: Lude.Maybe Lude.Text,
    -- | The task ID of the import image task.
    importTaskId :: Lude.Maybe Lude.Text,
    -- | The architecture of the virtual machine.
    architecture :: Lude.Maybe Lude.Text,
    -- | A description of the import task.
    description :: Lude.Maybe Lude.Text,
    -- | Any tags assigned to the image being imported.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportImageResponse' with the minimum fields required to make a request.
--
-- * 'status' - A brief status of the task.
-- * 'hypervisor' - The target hypervisor of the import task.
-- * 'platform' - The operating system of the virtual machine.
-- * 'progress' - The progress of the task.
-- * 'licenseSpecifications' - The ARNs of the license configurations.
-- * 'licenseType' - The license type of the virtual machine.
-- * 'snapshotDetails' - Information about the snapshots.
-- * 'encrypted' - Indicates whether the AMI is encrypted.
-- * 'kmsKeyId' - The identifier for the symmetric AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted AMI.
-- * 'statusMessage' - A detailed status message of the import task.
-- * 'imageId' - The ID of the Amazon Machine Image (AMI) created by the import task.
-- * 'importTaskId' - The task ID of the import image task.
-- * 'architecture' - The architecture of the virtual machine.
-- * 'description' - A description of the import task.
-- * 'tags' - Any tags assigned to the image being imported.
-- * 'responseStatus' - The response status code.
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

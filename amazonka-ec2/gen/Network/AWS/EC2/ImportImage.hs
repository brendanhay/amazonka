{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an
-- Amazon Machine Image (AMI). For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html Importing a VM as an Image Using VM Import\/Export>
-- in the /VM Import\/Export User Guide/.
module Network.AWS.EC2.ImportImage
  ( -- * Creating a Request
    ImportImage (..),
    newImportImage,

    -- * Request Lenses
    importImage_hypervisor,
    importImage_platform,
    importImage_tagSpecifications,
    importImage_dryRun,
    importImage_encrypted,
    importImage_roleName,
    importImage_licenseSpecifications,
    importImage_architecture,
    importImage_kmsKeyId,
    importImage_diskContainers,
    importImage_clientData,
    importImage_description,
    importImage_licenseType,
    importImage_clientToken,

    -- * Destructuring the Response
    ImportImageResponse (..),
    newImportImageResponse,

    -- * Response Lenses
    importImageResponse_hypervisor,
    importImageResponse_platform,
    importImageResponse_statusMessage,
    importImageResponse_status,
    importImageResponse_snapshotDetails,
    importImageResponse_encrypted,
    importImageResponse_importTaskId,
    importImageResponse_licenseSpecifications,
    importImageResponse_architecture,
    importImageResponse_imageId,
    importImageResponse_kmsKeyId,
    importImageResponse_tags,
    importImageResponse_description,
    importImageResponse_licenseType,
    importImageResponse_progress,
    importImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportImage' smart constructor.
data ImportImage = ImportImage'
  { -- | The target hypervisor platform.
    --
    -- Valid values: @xen@
    hypervisor :: Core.Maybe Core.Text,
    -- | The operating system of the virtual machine.
    --
    -- Valid values: @Windows@ | @Linux@
    platform :: Core.Maybe Core.Text,
    -- | The tags to apply to the import image task during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies whether the destination AMI of the imported image should be
    -- encrypted. The default CMK for EBS is used unless you specify a
    -- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Core.Maybe Core.Bool,
    -- | The name of the role to use when not using the default role,
    -- \'vmimport\'.
    roleName :: Core.Maybe Core.Text,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Core.Maybe [ImportImageLicenseConfigurationRequest],
    -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Core.Maybe Core.Text,
    -- | An identifier for the symmetric AWS Key Management Service (AWS KMS)
    -- customer master key (CMK) to use when creating the encrypted AMI. This
    -- parameter is only required if you want to use a non-default CMK; if this
    -- parameter is not specified, the default CMK for EBS is used. If a
    -- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
    --
    -- The CMK identifier may be provided in any of the following formats:
    --
    -- -   Key ID
    --
    -- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the CMK, the AWS account ID of the CMK
    --     owner, the @alias@ namespace, and then the CMK alias. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the CMK, the AWS account ID of the CMK
    --     owner, the @key@ namespace, and then the CMK ID. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
    --
    -- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
    --     namespace, followed by the Region of the CMK, the AWS account ID of
    --     the CMK owner, the @alias@ namespace, and then the CMK alias. For
    --     example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
    -- may appear to complete even though you provided an invalid identifier.
    -- This action will eventually report failure.
    --
    -- The specified CMK must exist in the Region that the AMI is being copied
    -- to.
    --
    -- Amazon EBS does not support asymmetric CMKs.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Information about the disk containers.
    diskContainers :: Core.Maybe [ImageDiskContainer],
    -- | The client-specific data.
    clientData :: Core.Maybe ClientData,
    -- | A description string for the import image task.
    description :: Core.Maybe Core.Text,
    -- | The license type to be used for the Amazon Machine Image (AMI) after
    -- importing.
    --
    -- By default, we detect the source-system operating system (OS) and apply
    -- the appropriate license. Specify @AWS@ to replace the source-system
    -- license with an AWS license, if appropriate. Specify @BYOL@ to retain
    -- the source-system license, if appropriate.
    --
    -- To use @BYOL@, you must have existing licenses with rights to use these
    -- licenses in a third party cloud, such as AWS. For more information, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
    -- in the VM Import\/Export User Guide.
    licenseType :: Core.Maybe Core.Text,
    -- | The token to enable idempotency for VM import requests.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisor', 'importImage_hypervisor' - The target hypervisor platform.
--
-- Valid values: @xen@
--
-- 'platform', 'importImage_platform' - The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
--
-- 'tagSpecifications', 'importImage_tagSpecifications' - The tags to apply to the import image task during creation.
--
-- 'dryRun', 'importImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'encrypted', 'importImage_encrypted' - Specifies whether the destination AMI of the imported image should be
-- encrypted. The default CMK for EBS is used unless you specify a
-- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'roleName', 'importImage_roleName' - The name of the role to use when not using the default role,
-- \'vmimport\'.
--
-- 'licenseSpecifications', 'importImage_licenseSpecifications' - The ARNs of the license configurations.
--
-- 'architecture', 'importImage_architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- 'kmsKeyId', 'importImage_kmsKeyId' - An identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) to use when creating the encrypted AMI. This
-- parameter is only required if you want to use a non-default CMK; if this
-- parameter is not specified, the default CMK for EBS is used. If a
-- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @alias@ namespace, and then the CMK alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @key@ namespace, and then the CMK ID. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the CMK, the AWS account ID of
--     the CMK owner, the @alias@ namespace, and then the CMK alias. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
-- may appear to complete even though you provided an invalid identifier.
-- This action will eventually report failure.
--
-- The specified CMK must exist in the Region that the AMI is being copied
-- to.
--
-- Amazon EBS does not support asymmetric CMKs.
--
-- 'diskContainers', 'importImage_diskContainers' - Information about the disk containers.
--
-- 'clientData', 'importImage_clientData' - The client-specific data.
--
-- 'description', 'importImage_description' - A description string for the import image task.
--
-- 'licenseType', 'importImage_licenseType' - The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- By default, we detect the source-system operating system (OS) and apply
-- the appropriate license. Specify @AWS@ to replace the source-system
-- license with an AWS license, if appropriate. Specify @BYOL@ to retain
-- the source-system license, if appropriate.
--
-- To use @BYOL@, you must have existing licenses with rights to use these
-- licenses in a third party cloud, such as AWS. For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
-- in the VM Import\/Export User Guide.
--
-- 'clientToken', 'importImage_clientToken' - The token to enable idempotency for VM import requests.
newImportImage ::
  ImportImage
newImportImage =
  ImportImage'
    { hypervisor = Core.Nothing,
      platform = Core.Nothing,
      tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      encrypted = Core.Nothing,
      roleName = Core.Nothing,
      licenseSpecifications = Core.Nothing,
      architecture = Core.Nothing,
      kmsKeyId = Core.Nothing,
      diskContainers = Core.Nothing,
      clientData = Core.Nothing,
      description = Core.Nothing,
      licenseType = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The target hypervisor platform.
--
-- Valid values: @xen@
importImage_hypervisor :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_hypervisor = Lens.lens (\ImportImage' {hypervisor} -> hypervisor) (\s@ImportImage' {} a -> s {hypervisor = a} :: ImportImage)

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
importImage_platform :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_platform = Lens.lens (\ImportImage' {platform} -> platform) (\s@ImportImage' {} a -> s {platform = a} :: ImportImage)

-- | The tags to apply to the import image task during creation.
importImage_tagSpecifications :: Lens.Lens' ImportImage (Core.Maybe [TagSpecification])
importImage_tagSpecifications = Lens.lens (\ImportImage' {tagSpecifications} -> tagSpecifications) (\s@ImportImage' {} a -> s {tagSpecifications = a} :: ImportImage) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importImage_dryRun :: Lens.Lens' ImportImage (Core.Maybe Core.Bool)
importImage_dryRun = Lens.lens (\ImportImage' {dryRun} -> dryRun) (\s@ImportImage' {} a -> s {dryRun = a} :: ImportImage)

-- | Specifies whether the destination AMI of the imported image should be
-- encrypted. The default CMK for EBS is used unless you specify a
-- non-default AWS Key Management Service (AWS KMS) CMK using @KmsKeyId@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
importImage_encrypted :: Lens.Lens' ImportImage (Core.Maybe Core.Bool)
importImage_encrypted = Lens.lens (\ImportImage' {encrypted} -> encrypted) (\s@ImportImage' {} a -> s {encrypted = a} :: ImportImage)

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
importImage_roleName :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_roleName = Lens.lens (\ImportImage' {roleName} -> roleName) (\s@ImportImage' {} a -> s {roleName = a} :: ImportImage)

-- | The ARNs of the license configurations.
importImage_licenseSpecifications :: Lens.Lens' ImportImage (Core.Maybe [ImportImageLicenseConfigurationRequest])
importImage_licenseSpecifications = Lens.lens (\ImportImage' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImage' {} a -> s {licenseSpecifications = a} :: ImportImage) Core.. Lens.mapping Lens._Coerce

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
importImage_architecture :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_architecture = Lens.lens (\ImportImage' {architecture} -> architecture) (\s@ImportImage' {} a -> s {architecture = a} :: ImportImage)

-- | An identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) to use when creating the encrypted AMI. This
-- parameter is only required if you want to use a non-default CMK; if this
-- parameter is not specified, the default CMK for EBS is used. If a
-- @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- The CMK identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @alias@ namespace, and then the CMK alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the CMK, the AWS account ID of the CMK
--     owner, the @key@ namespace, and then the CMK ID. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the CMK, the AWS account ID of
--     the CMK owner, the @alias@ namespace, and then the CMK alias. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- AWS parses @KmsKeyId@ asynchronously, meaning that the action you call
-- may appear to complete even though you provided an invalid identifier.
-- This action will eventually report failure.
--
-- The specified CMK must exist in the Region that the AMI is being copied
-- to.
--
-- Amazon EBS does not support asymmetric CMKs.
importImage_kmsKeyId :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_kmsKeyId = Lens.lens (\ImportImage' {kmsKeyId} -> kmsKeyId) (\s@ImportImage' {} a -> s {kmsKeyId = a} :: ImportImage)

-- | Information about the disk containers.
importImage_diskContainers :: Lens.Lens' ImportImage (Core.Maybe [ImageDiskContainer])
importImage_diskContainers = Lens.lens (\ImportImage' {diskContainers} -> diskContainers) (\s@ImportImage' {} a -> s {diskContainers = a} :: ImportImage) Core.. Lens.mapping Lens._Coerce

-- | The client-specific data.
importImage_clientData :: Lens.Lens' ImportImage (Core.Maybe ClientData)
importImage_clientData = Lens.lens (\ImportImage' {clientData} -> clientData) (\s@ImportImage' {} a -> s {clientData = a} :: ImportImage)

-- | A description string for the import image task.
importImage_description :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_description = Lens.lens (\ImportImage' {description} -> description) (\s@ImportImage' {} a -> s {description = a} :: ImportImage)

-- | The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- By default, we detect the source-system operating system (OS) and apply
-- the appropriate license. Specify @AWS@ to replace the source-system
-- license with an AWS license, if appropriate. Specify @BYOL@ to retain
-- the source-system license, if appropriate.
--
-- To use @BYOL@, you must have existing licenses with rights to use these
-- licenses in a third party cloud, such as AWS. For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
-- in the VM Import\/Export User Guide.
importImage_licenseType :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_licenseType = Lens.lens (\ImportImage' {licenseType} -> licenseType) (\s@ImportImage' {} a -> s {licenseType = a} :: ImportImage)

-- | The token to enable idempotency for VM import requests.
importImage_clientToken :: Lens.Lens' ImportImage (Core.Maybe Core.Text)
importImage_clientToken = Lens.lens (\ImportImage' {clientToken} -> clientToken) (\s@ImportImage' {} a -> s {clientToken = a} :: ImportImage)

instance Core.AWSRequest ImportImage where
  type AWSResponse ImportImage = ImportImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportImageResponse'
            Core.<$> (x Core..@? "hypervisor")
            Core.<*> (x Core..@? "platform")
            Core.<*> (x Core..@? "statusMessage")
            Core.<*> (x Core..@? "status")
            Core.<*> ( x Core..@? "snapshotDetailSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "encrypted")
            Core.<*> (x Core..@? "importTaskId")
            Core.<*> ( x Core..@? "licenseSpecifications"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "architecture")
            Core.<*> (x Core..@? "imageId")
            Core.<*> (x Core..@? "kmsKeyId")
            Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "description")
            Core.<*> (x Core..@? "licenseType")
            Core.<*> (x Core..@? "progress")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ImportImage

instance Core.NFData ImportImage

instance Core.ToHeaders ImportImage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ImportImage where
  toPath = Core.const "/"

instance Core.ToQuery ImportImage where
  toQuery ImportImage' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ImportImage" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "Hypervisor" Core.=: hypervisor,
        "Platform" Core.=: platform,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Encrypted" Core.=: encrypted,
        "RoleName" Core.=: roleName,
        Core.toQuery
          ( Core.toQueryList "LicenseSpecifications"
              Core.<$> licenseSpecifications
          ),
        "Architecture" Core.=: architecture,
        "KmsKeyId" Core.=: kmsKeyId,
        Core.toQuery
          ( Core.toQueryList "DiskContainer"
              Core.<$> diskContainers
          ),
        "ClientData" Core.=: clientData,
        "Description" Core.=: description,
        "LicenseType" Core.=: licenseType,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newImportImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { -- | The target hypervisor of the import task.
    hypervisor :: Core.Maybe Core.Text,
    -- | The operating system of the virtual machine.
    platform :: Core.Maybe Core.Text,
    -- | A detailed status message of the import task.
    statusMessage :: Core.Maybe Core.Text,
    -- | A brief status of the task.
    status :: Core.Maybe Core.Text,
    -- | Information about the snapshots.
    snapshotDetails :: Core.Maybe [SnapshotDetail],
    -- | Indicates whether the AMI is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The task ID of the import image task.
    importTaskId :: Core.Maybe Core.Text,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Core.Maybe [ImportImageLicenseConfigurationResponse],
    -- | The architecture of the virtual machine.
    architecture :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Machine Image (AMI) created by the import task.
    imageId :: Core.Maybe Core.Text,
    -- | The identifier for the symmetric AWS Key Management Service (AWS KMS)
    -- customer master key (CMK) that was used to create the encrypted AMI.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the import image task.
    tags :: Core.Maybe [Tag],
    -- | A description of the import task.
    description :: Core.Maybe Core.Text,
    -- | The license type of the virtual machine.
    licenseType :: Core.Maybe Core.Text,
    -- | The progress of the task.
    progress :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisor', 'importImageResponse_hypervisor' - The target hypervisor of the import task.
--
-- 'platform', 'importImageResponse_platform' - The operating system of the virtual machine.
--
-- 'statusMessage', 'importImageResponse_statusMessage' - A detailed status message of the import task.
--
-- 'status', 'importImageResponse_status' - A brief status of the task.
--
-- 'snapshotDetails', 'importImageResponse_snapshotDetails' - Information about the snapshots.
--
-- 'encrypted', 'importImageResponse_encrypted' - Indicates whether the AMI is encrypted.
--
-- 'importTaskId', 'importImageResponse_importTaskId' - The task ID of the import image task.
--
-- 'licenseSpecifications', 'importImageResponse_licenseSpecifications' - The ARNs of the license configurations.
--
-- 'architecture', 'importImageResponse_architecture' - The architecture of the virtual machine.
--
-- 'imageId', 'importImageResponse_imageId' - The ID of the Amazon Machine Image (AMI) created by the import task.
--
-- 'kmsKeyId', 'importImageResponse_kmsKeyId' - The identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) that was used to create the encrypted AMI.
--
-- 'tags', 'importImageResponse_tags' - Any tags assigned to the import image task.
--
-- 'description', 'importImageResponse_description' - A description of the import task.
--
-- 'licenseType', 'importImageResponse_licenseType' - The license type of the virtual machine.
--
-- 'progress', 'importImageResponse_progress' - The progress of the task.
--
-- 'httpStatus', 'importImageResponse_httpStatus' - The response's http status code.
newImportImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ImportImageResponse
newImportImageResponse pHttpStatus_ =
  ImportImageResponse'
    { hypervisor = Core.Nothing,
      platform = Core.Nothing,
      statusMessage = Core.Nothing,
      status = Core.Nothing,
      snapshotDetails = Core.Nothing,
      encrypted = Core.Nothing,
      importTaskId = Core.Nothing,
      licenseSpecifications = Core.Nothing,
      architecture = Core.Nothing,
      imageId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      licenseType = Core.Nothing,
      progress = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The target hypervisor of the import task.
importImageResponse_hypervisor :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_hypervisor = Lens.lens (\ImportImageResponse' {hypervisor} -> hypervisor) (\s@ImportImageResponse' {} a -> s {hypervisor = a} :: ImportImageResponse)

-- | The operating system of the virtual machine.
importImageResponse_platform :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_platform = Lens.lens (\ImportImageResponse' {platform} -> platform) (\s@ImportImageResponse' {} a -> s {platform = a} :: ImportImageResponse)

-- | A detailed status message of the import task.
importImageResponse_statusMessage :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_statusMessage = Lens.lens (\ImportImageResponse' {statusMessage} -> statusMessage) (\s@ImportImageResponse' {} a -> s {statusMessage = a} :: ImportImageResponse)

-- | A brief status of the task.
importImageResponse_status :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_status = Lens.lens (\ImportImageResponse' {status} -> status) (\s@ImportImageResponse' {} a -> s {status = a} :: ImportImageResponse)

-- | Information about the snapshots.
importImageResponse_snapshotDetails :: Lens.Lens' ImportImageResponse (Core.Maybe [SnapshotDetail])
importImageResponse_snapshotDetails = Lens.lens (\ImportImageResponse' {snapshotDetails} -> snapshotDetails) (\s@ImportImageResponse' {} a -> s {snapshotDetails = a} :: ImportImageResponse) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the AMI is encrypted.
importImageResponse_encrypted :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Bool)
importImageResponse_encrypted = Lens.lens (\ImportImageResponse' {encrypted} -> encrypted) (\s@ImportImageResponse' {} a -> s {encrypted = a} :: ImportImageResponse)

-- | The task ID of the import image task.
importImageResponse_importTaskId :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_importTaskId = Lens.lens (\ImportImageResponse' {importTaskId} -> importTaskId) (\s@ImportImageResponse' {} a -> s {importTaskId = a} :: ImportImageResponse)

-- | The ARNs of the license configurations.
importImageResponse_licenseSpecifications :: Lens.Lens' ImportImageResponse (Core.Maybe [ImportImageLicenseConfigurationResponse])
importImageResponse_licenseSpecifications = Lens.lens (\ImportImageResponse' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImageResponse' {} a -> s {licenseSpecifications = a} :: ImportImageResponse) Core.. Lens.mapping Lens._Coerce

-- | The architecture of the virtual machine.
importImageResponse_architecture :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_architecture = Lens.lens (\ImportImageResponse' {architecture} -> architecture) (\s@ImportImageResponse' {} a -> s {architecture = a} :: ImportImageResponse)

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
importImageResponse_imageId :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_imageId = Lens.lens (\ImportImageResponse' {imageId} -> imageId) (\s@ImportImageResponse' {} a -> s {imageId = a} :: ImportImageResponse)

-- | The identifier for the symmetric AWS Key Management Service (AWS KMS)
-- customer master key (CMK) that was used to create the encrypted AMI.
importImageResponse_kmsKeyId :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_kmsKeyId = Lens.lens (\ImportImageResponse' {kmsKeyId} -> kmsKeyId) (\s@ImportImageResponse' {} a -> s {kmsKeyId = a} :: ImportImageResponse)

-- | Any tags assigned to the import image task.
importImageResponse_tags :: Lens.Lens' ImportImageResponse (Core.Maybe [Tag])
importImageResponse_tags = Lens.lens (\ImportImageResponse' {tags} -> tags) (\s@ImportImageResponse' {} a -> s {tags = a} :: ImportImageResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the import task.
importImageResponse_description :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_description = Lens.lens (\ImportImageResponse' {description} -> description) (\s@ImportImageResponse' {} a -> s {description = a} :: ImportImageResponse)

-- | The license type of the virtual machine.
importImageResponse_licenseType :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_licenseType = Lens.lens (\ImportImageResponse' {licenseType} -> licenseType) (\s@ImportImageResponse' {} a -> s {licenseType = a} :: ImportImageResponse)

-- | The progress of the task.
importImageResponse_progress :: Lens.Lens' ImportImageResponse (Core.Maybe Core.Text)
importImageResponse_progress = Lens.lens (\ImportImageResponse' {progress} -> progress) (\s@ImportImageResponse' {} a -> s {progress = a} :: ImportImageResponse)

-- | The response's http status code.
importImageResponse_httpStatus :: Lens.Lens' ImportImageResponse Core.Int
importImageResponse_httpStatus = Lens.lens (\ImportImageResponse' {httpStatus} -> httpStatus) (\s@ImportImageResponse' {} a -> s {httpStatus = a} :: ImportImageResponse)

instance Core.NFData ImportImageResponse

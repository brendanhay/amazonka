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
-- Module      : Amazonka.EC2.ImportImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import single or multi-volume disk images or EBS snapshots into an
-- Amazon Machine Image (AMI).
--
-- Amazon Web Services VM Import\/Export strongly recommends specifying a
-- value for either the @--license-type@ or @--usage-operation@ parameter
-- when you create a new VM Import task. This ensures your operating system
-- is licensed appropriately and your billing is optimized.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html Importing a VM as an image using VM Import\/Export>
-- in the /VM Import\/Export User Guide/.
module Amazonka.EC2.ImportImage
  ( -- * Creating a Request
    ImportImage (..),
    newImportImage,

    -- * Request Lenses
    importImage_hypervisor,
    importImage_roleName,
    importImage_clientToken,
    importImage_licenseType,
    importImage_diskContainers,
    importImage_platform,
    importImage_description,
    importImage_dryRun,
    importImage_usageOperation,
    importImage_encrypted,
    importImage_kmsKeyId,
    importImage_bootMode,
    importImage_licenseSpecifications,
    importImage_tagSpecifications,
    importImage_clientData,
    importImage_architecture,

    -- * Destructuring the Response
    ImportImageResponse (..),
    newImportImageResponse,

    -- * Response Lenses
    importImageResponse_tags,
    importImageResponse_hypervisor,
    importImageResponse_progress,
    importImageResponse_licenseType,
    importImageResponse_importTaskId,
    importImageResponse_status,
    importImageResponse_platform,
    importImageResponse_description,
    importImageResponse_usageOperation,
    importImageResponse_encrypted,
    importImageResponse_kmsKeyId,
    importImageResponse_licenseSpecifications,
    importImageResponse_snapshotDetails,
    importImageResponse_statusMessage,
    importImageResponse_architecture,
    importImageResponse_imageId,
    importImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportImage' smart constructor.
data ImportImage = ImportImage'
  { -- | The target hypervisor platform.
    --
    -- Valid values: @xen@
    hypervisor :: Prelude.Maybe Prelude.Text,
    -- | The name of the role to use when not using the default role,
    -- \'vmimport\'.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The token to enable idempotency for VM import requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The license type to be used for the Amazon Machine Image (AMI) after
    -- importing.
    --
    -- Specify @AWS@ to replace the source-system license with an Amazon Web
    -- Services license or @BYOL@ to retain the source-system license. Leaving
    -- this parameter undefined is the same as choosing @AWS@ when importing a
    -- Windows Server operating system, and the same as choosing @BYOL@ when
    -- importing a Windows client operating system (such as Windows 10) or a
    -- Linux operating system.
    --
    -- To use @BYOL@, you must have existing licenses with rights to use these
    -- licenses in a third party cloud, such as Amazon Web Services. For more
    -- information, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
    -- in the VM Import\/Export User Guide.
    licenseType :: Prelude.Maybe Prelude.Text,
    -- | Information about the disk containers.
    diskContainers :: Prelude.Maybe [ImageDiskContainer],
    -- | The operating system of the virtual machine.
    --
    -- Valid values: @Windows@ | @Linux@
    platform :: Prelude.Maybe Prelude.Text,
    -- | A description string for the import image task.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The usage operation value. For more information, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#prerequisites Licensing options>
    -- in the /VM Import\/Export User Guide/.
    usageOperation :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the destination AMI of the imported image should be
    -- encrypted. The default KMS key for EBS is used unless you specify a
    -- non-default KMS key using @KmsKeyId@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | An identifier for the symmetric KMS key to use when creating the
    -- encrypted AMI. This parameter is only required if you want to use a
    -- non-default KMS key; if this parameter is not specified, the default KMS
    -- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
    -- must also be set.
    --
    -- The KMS key identifier may be provided in any of the following formats:
    --
    -- -   Key ID
    --
    -- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the key, the Amazon Web Services account
    --     ID of the key owner, the @alias@ namespace, and then the key alias.
    --     For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
    --     followed by the Region of the key, the Amazon Web Services account
    --     ID of the key owner, the @key@ namespace, and then the key ID. For
    --     example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
    --
    -- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
    --     namespace, followed by the Region of the key, the Amazon Web
    --     Services account ID of the key owner, the @alias@ namespace, and
    --     then the key alias. For example,
    --     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
    --
    -- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
    -- action you call may appear to complete even though you provided an
    -- invalid identifier. This action will eventually report failure.
    --
    -- The specified KMS key must exist in the Region that the AMI is being
    -- copied to.
    --
    -- Amazon EBS does not support asymmetric KMS keys.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The boot mode of the virtual machine.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Prelude.Maybe [ImportImageLicenseConfigurationRequest],
    -- | The tags to apply to the import image task during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The client-specific data.
    clientData :: Prelude.Maybe ClientData,
    -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@
    architecture :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'roleName', 'importImage_roleName' - The name of the role to use when not using the default role,
-- \'vmimport\'.
--
-- 'clientToken', 'importImage_clientToken' - The token to enable idempotency for VM import requests.
--
-- 'licenseType', 'importImage_licenseType' - The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- Specify @AWS@ to replace the source-system license with an Amazon Web
-- Services license or @BYOL@ to retain the source-system license. Leaving
-- this parameter undefined is the same as choosing @AWS@ when importing a
-- Windows Server operating system, and the same as choosing @BYOL@ when
-- importing a Windows client operating system (such as Windows 10) or a
-- Linux operating system.
--
-- To use @BYOL@, you must have existing licenses with rights to use these
-- licenses in a third party cloud, such as Amazon Web Services. For more
-- information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
-- in the VM Import\/Export User Guide.
--
-- 'diskContainers', 'importImage_diskContainers' - Information about the disk containers.
--
-- 'platform', 'importImage_platform' - The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
--
-- 'description', 'importImage_description' - A description string for the import image task.
--
-- 'dryRun', 'importImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'usageOperation', 'importImage_usageOperation' - The usage operation value. For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#prerequisites Licensing options>
-- in the /VM Import\/Export User Guide/.
--
-- 'encrypted', 'importImage_encrypted' - Specifies whether the destination AMI of the imported image should be
-- encrypted. The default KMS key for EBS is used unless you specify a
-- non-default KMS key using @KmsKeyId@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kmsKeyId', 'importImage_kmsKeyId' - An identifier for the symmetric KMS key to use when creating the
-- encrypted AMI. This parameter is only required if you want to use a
-- non-default KMS key; if this parameter is not specified, the default KMS
-- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
-- must also be set.
--
-- The KMS key identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @alias@ namespace, and then the key alias.
--     For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @key@ namespace, and then the key ID. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the key, the Amazon Web
--     Services account ID of the key owner, the @alias@ namespace, and
--     then the key alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
-- action you call may appear to complete even though you provided an
-- invalid identifier. This action will eventually report failure.
--
-- The specified KMS key must exist in the Region that the AMI is being
-- copied to.
--
-- Amazon EBS does not support asymmetric KMS keys.
--
-- 'bootMode', 'importImage_bootMode' - The boot mode of the virtual machine.
--
-- 'licenseSpecifications', 'importImage_licenseSpecifications' - The ARNs of the license configurations.
--
-- 'tagSpecifications', 'importImage_tagSpecifications' - The tags to apply to the import image task during creation.
--
-- 'clientData', 'importImage_clientData' - The client-specific data.
--
-- 'architecture', 'importImage_architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@
newImportImage ::
  ImportImage
newImportImage =
  ImportImage'
    { hypervisor = Prelude.Nothing,
      roleName = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      diskContainers = Prelude.Nothing,
      platform = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      usageOperation = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      bootMode = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      clientData = Prelude.Nothing,
      architecture = Prelude.Nothing
    }

-- | The target hypervisor platform.
--
-- Valid values: @xen@
importImage_hypervisor :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_hypervisor = Lens.lens (\ImportImage' {hypervisor} -> hypervisor) (\s@ImportImage' {} a -> s {hypervisor = a} :: ImportImage)

-- | The name of the role to use when not using the default role,
-- \'vmimport\'.
importImage_roleName :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_roleName = Lens.lens (\ImportImage' {roleName} -> roleName) (\s@ImportImage' {} a -> s {roleName = a} :: ImportImage)

-- | The token to enable idempotency for VM import requests.
importImage_clientToken :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_clientToken = Lens.lens (\ImportImage' {clientToken} -> clientToken) (\s@ImportImage' {} a -> s {clientToken = a} :: ImportImage)

-- | The license type to be used for the Amazon Machine Image (AMI) after
-- importing.
--
-- Specify @AWS@ to replace the source-system license with an Amazon Web
-- Services license or @BYOL@ to retain the source-system license. Leaving
-- this parameter undefined is the same as choosing @AWS@ when importing a
-- Windows Server operating system, and the same as choosing @BYOL@ when
-- importing a Windows client operating system (such as Windows 10) or a
-- Linux operating system.
--
-- To use @BYOL@, you must have existing licenses with rights to use these
-- licenses in a third party cloud, such as Amazon Web Services. For more
-- information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmimport-image-import.html#prerequisites-image Prerequisites>
-- in the VM Import\/Export User Guide.
importImage_licenseType :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_licenseType = Lens.lens (\ImportImage' {licenseType} -> licenseType) (\s@ImportImage' {} a -> s {licenseType = a} :: ImportImage)

-- | Information about the disk containers.
importImage_diskContainers :: Lens.Lens' ImportImage (Prelude.Maybe [ImageDiskContainer])
importImage_diskContainers = Lens.lens (\ImportImage' {diskContainers} -> diskContainers) (\s@ImportImage' {} a -> s {diskContainers = a} :: ImportImage) Prelude.. Lens.mapping Lens.coerced

-- | The operating system of the virtual machine.
--
-- Valid values: @Windows@ | @Linux@
importImage_platform :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_platform = Lens.lens (\ImportImage' {platform} -> platform) (\s@ImportImage' {} a -> s {platform = a} :: ImportImage)

-- | A description string for the import image task.
importImage_description :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_description = Lens.lens (\ImportImage' {description} -> description) (\s@ImportImage' {} a -> s {description = a} :: ImportImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importImage_dryRun :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Bool)
importImage_dryRun = Lens.lens (\ImportImage' {dryRun} -> dryRun) (\s@ImportImage' {} a -> s {dryRun = a} :: ImportImage)

-- | The usage operation value. For more information, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#prerequisites Licensing options>
-- in the /VM Import\/Export User Guide/.
importImage_usageOperation :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_usageOperation = Lens.lens (\ImportImage' {usageOperation} -> usageOperation) (\s@ImportImage' {} a -> s {usageOperation = a} :: ImportImage)

-- | Specifies whether the destination AMI of the imported image should be
-- encrypted. The default KMS key for EBS is used unless you specify a
-- non-default KMS key using @KmsKeyId@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
importImage_encrypted :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Bool)
importImage_encrypted = Lens.lens (\ImportImage' {encrypted} -> encrypted) (\s@ImportImage' {} a -> s {encrypted = a} :: ImportImage)

-- | An identifier for the symmetric KMS key to use when creating the
-- encrypted AMI. This parameter is only required if you want to use a
-- non-default KMS key; if this parameter is not specified, the default KMS
-- key for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag
-- must also be set.
--
-- The KMS key identifier may be provided in any of the following formats:
--
-- -   Key ID
--
-- -   Key alias. The alias ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @alias@ namespace, and then the key alias.
--     For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- -   ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace,
--     followed by the Region of the key, the Amazon Web Services account
--     ID of the key owner, the @key@ namespace, and then the key ID. For
--     example,
--     arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
--
-- -   ARN using key alias. The alias ARN contains the @arn:aws:kms@
--     namespace, followed by the Region of the key, the Amazon Web
--     Services account ID of the key owner, the @alias@ namespace, and
--     then the key alias. For example,
--     arn:aws:kms:/us-east-1/:/012345678910/:alias\//ExampleAlias/.
--
-- Amazon Web Services parses @KmsKeyId@ asynchronously, meaning that the
-- action you call may appear to complete even though you provided an
-- invalid identifier. This action will eventually report failure.
--
-- The specified KMS key must exist in the Region that the AMI is being
-- copied to.
--
-- Amazon EBS does not support asymmetric KMS keys.
importImage_kmsKeyId :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_kmsKeyId = Lens.lens (\ImportImage' {kmsKeyId} -> kmsKeyId) (\s@ImportImage' {} a -> s {kmsKeyId = a} :: ImportImage)

-- | The boot mode of the virtual machine.
importImage_bootMode :: Lens.Lens' ImportImage (Prelude.Maybe BootModeValues)
importImage_bootMode = Lens.lens (\ImportImage' {bootMode} -> bootMode) (\s@ImportImage' {} a -> s {bootMode = a} :: ImportImage)

-- | The ARNs of the license configurations.
importImage_licenseSpecifications :: Lens.Lens' ImportImage (Prelude.Maybe [ImportImageLicenseConfigurationRequest])
importImage_licenseSpecifications = Lens.lens (\ImportImage' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImage' {} a -> s {licenseSpecifications = a} :: ImportImage) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to the import image task during creation.
importImage_tagSpecifications :: Lens.Lens' ImportImage (Prelude.Maybe [TagSpecification])
importImage_tagSpecifications = Lens.lens (\ImportImage' {tagSpecifications} -> tagSpecifications) (\s@ImportImage' {} a -> s {tagSpecifications = a} :: ImportImage) Prelude.. Lens.mapping Lens.coerced

-- | The client-specific data.
importImage_clientData :: Lens.Lens' ImportImage (Prelude.Maybe ClientData)
importImage_clientData = Lens.lens (\ImportImage' {clientData} -> clientData) (\s@ImportImage' {} a -> s {clientData = a} :: ImportImage)

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@
importImage_architecture :: Lens.Lens' ImportImage (Prelude.Maybe Prelude.Text)
importImage_architecture = Lens.lens (\ImportImage' {architecture} -> architecture) (\s@ImportImage' {} a -> s {architecture = a} :: ImportImage)

instance Core.AWSRequest ImportImage where
  type AWSResponse ImportImage = ImportImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ImportImageResponse'
            Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "hypervisor")
            Prelude.<*> (x Core..@? "progress")
            Prelude.<*> (x Core..@? "licenseType")
            Prelude.<*> (x Core..@? "importTaskId")
            Prelude.<*> (x Core..@? "status")
            Prelude.<*> (x Core..@? "platform")
            Prelude.<*> (x Core..@? "description")
            Prelude.<*> (x Core..@? "usageOperation")
            Prelude.<*> (x Core..@? "encrypted")
            Prelude.<*> (x Core..@? "kmsKeyId")
            Prelude.<*> ( x Core..@? "licenseSpecifications"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "snapshotDetailSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "statusMessage")
            Prelude.<*> (x Core..@? "architecture")
            Prelude.<*> (x Core..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportImage where
  hashWithSalt _salt ImportImage' {..} =
    _salt `Prelude.hashWithSalt` hypervisor
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` diskContainers
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` usageOperation
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` licenseSpecifications
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` clientData
      `Prelude.hashWithSalt` architecture

instance Prelude.NFData ImportImage where
  rnf ImportImage' {..} =
    Prelude.rnf hypervisor
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf diskContainers
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf usageOperation
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf licenseSpecifications
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf clientData
      `Prelude.seq` Prelude.rnf architecture

instance Core.ToHeaders ImportImage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ImportImage where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportImage where
  toQuery ImportImage' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ImportImage" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Hypervisor" Core.=: hypervisor,
        "RoleName" Core.=: roleName,
        "ClientToken" Core.=: clientToken,
        "LicenseType" Core.=: licenseType,
        Core.toQuery
          ( Core.toQueryList "DiskContainer"
              Prelude.<$> diskContainers
          ),
        "Platform" Core.=: platform,
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        "UsageOperation" Core.=: usageOperation,
        "Encrypted" Core.=: encrypted,
        "KmsKeyId" Core.=: kmsKeyId,
        "BootMode" Core.=: bootMode,
        Core.toQuery
          ( Core.toQueryList "LicenseSpecifications"
              Prelude.<$> licenseSpecifications
          ),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "ClientData" Core.=: clientData,
        "Architecture" Core.=: architecture
      ]

-- | /See:/ 'newImportImageResponse' smart constructor.
data ImportImageResponse = ImportImageResponse'
  { -- | Any tags assigned to the import image task.
    tags :: Prelude.Maybe [Tag],
    -- | The target hypervisor of the import task.
    hypervisor :: Prelude.Maybe Prelude.Text,
    -- | The progress of the task.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The license type of the virtual machine.
    licenseType :: Prelude.Maybe Prelude.Text,
    -- | The task ID of the import image task.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | A brief status of the task.
    status :: Prelude.Maybe Prelude.Text,
    -- | The operating system of the virtual machine.
    platform :: Prelude.Maybe Prelude.Text,
    -- | A description of the import task.
    description :: Prelude.Maybe Prelude.Text,
    -- | The usage operation value.
    usageOperation :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the AMI is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the symmetric KMS key that was used to create the
    -- encrypted AMI.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the license configurations.
    licenseSpecifications :: Prelude.Maybe [ImportImageLicenseConfigurationResponse],
    -- | Information about the snapshots.
    snapshotDetails :: Prelude.Maybe [SnapshotDetail],
    -- | A detailed status message of the import task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the virtual machine.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Machine Image (AMI) created by the import task.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'importImageResponse_tags' - Any tags assigned to the import image task.
--
-- 'hypervisor', 'importImageResponse_hypervisor' - The target hypervisor of the import task.
--
-- 'progress', 'importImageResponse_progress' - The progress of the task.
--
-- 'licenseType', 'importImageResponse_licenseType' - The license type of the virtual machine.
--
-- 'importTaskId', 'importImageResponse_importTaskId' - The task ID of the import image task.
--
-- 'status', 'importImageResponse_status' - A brief status of the task.
--
-- 'platform', 'importImageResponse_platform' - The operating system of the virtual machine.
--
-- 'description', 'importImageResponse_description' - A description of the import task.
--
-- 'usageOperation', 'importImageResponse_usageOperation' - The usage operation value.
--
-- 'encrypted', 'importImageResponse_encrypted' - Indicates whether the AMI is encrypted.
--
-- 'kmsKeyId', 'importImageResponse_kmsKeyId' - The identifier for the symmetric KMS key that was used to create the
-- encrypted AMI.
--
-- 'licenseSpecifications', 'importImageResponse_licenseSpecifications' - The ARNs of the license configurations.
--
-- 'snapshotDetails', 'importImageResponse_snapshotDetails' - Information about the snapshots.
--
-- 'statusMessage', 'importImageResponse_statusMessage' - A detailed status message of the import task.
--
-- 'architecture', 'importImageResponse_architecture' - The architecture of the virtual machine.
--
-- 'imageId', 'importImageResponse_imageId' - The ID of the Amazon Machine Image (AMI) created by the import task.
--
-- 'httpStatus', 'importImageResponse_httpStatus' - The response's http status code.
newImportImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportImageResponse
newImportImageResponse pHttpStatus_ =
  ImportImageResponse'
    { tags = Prelude.Nothing,
      hypervisor = Prelude.Nothing,
      progress = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      importTaskId = Prelude.Nothing,
      status = Prelude.Nothing,
      platform = Prelude.Nothing,
      description = Prelude.Nothing,
      usageOperation = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      snapshotDetails = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      architecture = Prelude.Nothing,
      imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any tags assigned to the import image task.
importImageResponse_tags :: Lens.Lens' ImportImageResponse (Prelude.Maybe [Tag])
importImageResponse_tags = Lens.lens (\ImportImageResponse' {tags} -> tags) (\s@ImportImageResponse' {} a -> s {tags = a} :: ImportImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The target hypervisor of the import task.
importImageResponse_hypervisor :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_hypervisor = Lens.lens (\ImportImageResponse' {hypervisor} -> hypervisor) (\s@ImportImageResponse' {} a -> s {hypervisor = a} :: ImportImageResponse)

-- | The progress of the task.
importImageResponse_progress :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_progress = Lens.lens (\ImportImageResponse' {progress} -> progress) (\s@ImportImageResponse' {} a -> s {progress = a} :: ImportImageResponse)

-- | The license type of the virtual machine.
importImageResponse_licenseType :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_licenseType = Lens.lens (\ImportImageResponse' {licenseType} -> licenseType) (\s@ImportImageResponse' {} a -> s {licenseType = a} :: ImportImageResponse)

-- | The task ID of the import image task.
importImageResponse_importTaskId :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_importTaskId = Lens.lens (\ImportImageResponse' {importTaskId} -> importTaskId) (\s@ImportImageResponse' {} a -> s {importTaskId = a} :: ImportImageResponse)

-- | A brief status of the task.
importImageResponse_status :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_status = Lens.lens (\ImportImageResponse' {status} -> status) (\s@ImportImageResponse' {} a -> s {status = a} :: ImportImageResponse)

-- | The operating system of the virtual machine.
importImageResponse_platform :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_platform = Lens.lens (\ImportImageResponse' {platform} -> platform) (\s@ImportImageResponse' {} a -> s {platform = a} :: ImportImageResponse)

-- | A description of the import task.
importImageResponse_description :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_description = Lens.lens (\ImportImageResponse' {description} -> description) (\s@ImportImageResponse' {} a -> s {description = a} :: ImportImageResponse)

-- | The usage operation value.
importImageResponse_usageOperation :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_usageOperation = Lens.lens (\ImportImageResponse' {usageOperation} -> usageOperation) (\s@ImportImageResponse' {} a -> s {usageOperation = a} :: ImportImageResponse)

-- | Indicates whether the AMI is encrypted.
importImageResponse_encrypted :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Bool)
importImageResponse_encrypted = Lens.lens (\ImportImageResponse' {encrypted} -> encrypted) (\s@ImportImageResponse' {} a -> s {encrypted = a} :: ImportImageResponse)

-- | The identifier for the symmetric KMS key that was used to create the
-- encrypted AMI.
importImageResponse_kmsKeyId :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_kmsKeyId = Lens.lens (\ImportImageResponse' {kmsKeyId} -> kmsKeyId) (\s@ImportImageResponse' {} a -> s {kmsKeyId = a} :: ImportImageResponse)

-- | The ARNs of the license configurations.
importImageResponse_licenseSpecifications :: Lens.Lens' ImportImageResponse (Prelude.Maybe [ImportImageLicenseConfigurationResponse])
importImageResponse_licenseSpecifications = Lens.lens (\ImportImageResponse' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImageResponse' {} a -> s {licenseSpecifications = a} :: ImportImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the snapshots.
importImageResponse_snapshotDetails :: Lens.Lens' ImportImageResponse (Prelude.Maybe [SnapshotDetail])
importImageResponse_snapshotDetails = Lens.lens (\ImportImageResponse' {snapshotDetails} -> snapshotDetails) (\s@ImportImageResponse' {} a -> s {snapshotDetails = a} :: ImportImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | A detailed status message of the import task.
importImageResponse_statusMessage :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_statusMessage = Lens.lens (\ImportImageResponse' {statusMessage} -> statusMessage) (\s@ImportImageResponse' {} a -> s {statusMessage = a} :: ImportImageResponse)

-- | The architecture of the virtual machine.
importImageResponse_architecture :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_architecture = Lens.lens (\ImportImageResponse' {architecture} -> architecture) (\s@ImportImageResponse' {} a -> s {architecture = a} :: ImportImageResponse)

-- | The ID of the Amazon Machine Image (AMI) created by the import task.
importImageResponse_imageId :: Lens.Lens' ImportImageResponse (Prelude.Maybe Prelude.Text)
importImageResponse_imageId = Lens.lens (\ImportImageResponse' {imageId} -> imageId) (\s@ImportImageResponse' {} a -> s {imageId = a} :: ImportImageResponse)

-- | The response's http status code.
importImageResponse_httpStatus :: Lens.Lens' ImportImageResponse Prelude.Int
importImageResponse_httpStatus = Lens.lens (\ImportImageResponse' {httpStatus} -> httpStatus) (\s@ImportImageResponse' {} a -> s {httpStatus = a} :: ImportImageResponse)

instance Prelude.NFData ImportImageResponse where
  rnf ImportImageResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hypervisor
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf importTaskId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf usageOperation
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseSpecifications
      `Prelude.seq` Prelude.rnf snapshotDetails
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus

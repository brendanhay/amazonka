{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.ImportImageTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImportImageTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.ImportImageLicenseConfigurationResponse
import Amazonka.EC2.Types.SnapshotDetail
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an import image task.
--
-- /See:/ 'newImportImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The boot mode of the virtual machine.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | A description of the import task.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the image is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The target hypervisor for the import task.
    --
    -- Valid values: @xen@
    hypervisor :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Machine Image (AMI) of the imported virtual
    -- machine.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the import image task.
    importTaskId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the KMS key that was used to create the encrypted
    -- image.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the license configurations that are associated with the
    -- import image task.
    licenseSpecifications :: Prelude.Maybe [ImportImageLicenseConfigurationResponse],
    -- | The license type of the virtual machine.
    licenseType :: Prelude.Maybe Prelude.Text,
    -- | The description string for the import image task.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The percentage of progress of the import image task.
    progress :: Prelude.Maybe Prelude.Text,
    -- | Information about the snapshots.
    snapshotDetails :: Prelude.Maybe [SnapshotDetail],
    -- | A brief status for the import image task.
    status :: Prelude.Maybe Prelude.Text,
    -- | A descriptive status message for the import image task.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The tags for the import image task.
    tags :: Prelude.Maybe [Tag],
    -- | The usage operation value.
    usageOperation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportImageTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'importImageTask_architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- 'bootMode', 'importImageTask_bootMode' - The boot mode of the virtual machine.
--
-- 'description', 'importImageTask_description' - A description of the import task.
--
-- 'encrypted', 'importImageTask_encrypted' - Indicates whether the image is encrypted.
--
-- 'hypervisor', 'importImageTask_hypervisor' - The target hypervisor for the import task.
--
-- Valid values: @xen@
--
-- 'imageId', 'importImageTask_imageId' - The ID of the Amazon Machine Image (AMI) of the imported virtual
-- machine.
--
-- 'importTaskId', 'importImageTask_importTaskId' - The ID of the import image task.
--
-- 'kmsKeyId', 'importImageTask_kmsKeyId' - The identifier for the KMS key that was used to create the encrypted
-- image.
--
-- 'licenseSpecifications', 'importImageTask_licenseSpecifications' - The ARNs of the license configurations that are associated with the
-- import image task.
--
-- 'licenseType', 'importImageTask_licenseType' - The license type of the virtual machine.
--
-- 'platform', 'importImageTask_platform' - The description string for the import image task.
--
-- 'progress', 'importImageTask_progress' - The percentage of progress of the import image task.
--
-- 'snapshotDetails', 'importImageTask_snapshotDetails' - Information about the snapshots.
--
-- 'status', 'importImageTask_status' - A brief status for the import image task.
--
-- 'statusMessage', 'importImageTask_statusMessage' - A descriptive status message for the import image task.
--
-- 'tags', 'importImageTask_tags' - The tags for the import image task.
--
-- 'usageOperation', 'importImageTask_usageOperation' - The usage operation value.
newImportImageTask ::
  ImportImageTask
newImportImageTask =
  ImportImageTask'
    { architecture = Prelude.Nothing,
      bootMode = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      hypervisor = Prelude.Nothing,
      imageId = Prelude.Nothing,
      importTaskId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      licenseSpecifications = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      platform = Prelude.Nothing,
      progress = Prelude.Nothing,
      snapshotDetails = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      usageOperation = Prelude.Nothing
    }

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
importImageTask_architecture :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_architecture = Lens.lens (\ImportImageTask' {architecture} -> architecture) (\s@ImportImageTask' {} a -> s {architecture = a} :: ImportImageTask)

-- | The boot mode of the virtual machine.
importImageTask_bootMode :: Lens.Lens' ImportImageTask (Prelude.Maybe BootModeValues)
importImageTask_bootMode = Lens.lens (\ImportImageTask' {bootMode} -> bootMode) (\s@ImportImageTask' {} a -> s {bootMode = a} :: ImportImageTask)

-- | A description of the import task.
importImageTask_description :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_description = Lens.lens (\ImportImageTask' {description} -> description) (\s@ImportImageTask' {} a -> s {description = a} :: ImportImageTask)

-- | Indicates whether the image is encrypted.
importImageTask_encrypted :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Bool)
importImageTask_encrypted = Lens.lens (\ImportImageTask' {encrypted} -> encrypted) (\s@ImportImageTask' {} a -> s {encrypted = a} :: ImportImageTask)

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@
importImageTask_hypervisor :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_hypervisor = Lens.lens (\ImportImageTask' {hypervisor} -> hypervisor) (\s@ImportImageTask' {} a -> s {hypervisor = a} :: ImportImageTask)

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual
-- machine.
importImageTask_imageId :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_imageId = Lens.lens (\ImportImageTask' {imageId} -> imageId) (\s@ImportImageTask' {} a -> s {imageId = a} :: ImportImageTask)

-- | The ID of the import image task.
importImageTask_importTaskId :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_importTaskId = Lens.lens (\ImportImageTask' {importTaskId} -> importTaskId) (\s@ImportImageTask' {} a -> s {importTaskId = a} :: ImportImageTask)

-- | The identifier for the KMS key that was used to create the encrypted
-- image.
importImageTask_kmsKeyId :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_kmsKeyId = Lens.lens (\ImportImageTask' {kmsKeyId} -> kmsKeyId) (\s@ImportImageTask' {} a -> s {kmsKeyId = a} :: ImportImageTask)

-- | The ARNs of the license configurations that are associated with the
-- import image task.
importImageTask_licenseSpecifications :: Lens.Lens' ImportImageTask (Prelude.Maybe [ImportImageLicenseConfigurationResponse])
importImageTask_licenseSpecifications = Lens.lens (\ImportImageTask' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImageTask' {} a -> s {licenseSpecifications = a} :: ImportImageTask) Prelude.. Lens.mapping Lens.coerced

-- | The license type of the virtual machine.
importImageTask_licenseType :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_licenseType = Lens.lens (\ImportImageTask' {licenseType} -> licenseType) (\s@ImportImageTask' {} a -> s {licenseType = a} :: ImportImageTask)

-- | The description string for the import image task.
importImageTask_platform :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_platform = Lens.lens (\ImportImageTask' {platform} -> platform) (\s@ImportImageTask' {} a -> s {platform = a} :: ImportImageTask)

-- | The percentage of progress of the import image task.
importImageTask_progress :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_progress = Lens.lens (\ImportImageTask' {progress} -> progress) (\s@ImportImageTask' {} a -> s {progress = a} :: ImportImageTask)

-- | Information about the snapshots.
importImageTask_snapshotDetails :: Lens.Lens' ImportImageTask (Prelude.Maybe [SnapshotDetail])
importImageTask_snapshotDetails = Lens.lens (\ImportImageTask' {snapshotDetails} -> snapshotDetails) (\s@ImportImageTask' {} a -> s {snapshotDetails = a} :: ImportImageTask) Prelude.. Lens.mapping Lens.coerced

-- | A brief status for the import image task.
importImageTask_status :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_status = Lens.lens (\ImportImageTask' {status} -> status) (\s@ImportImageTask' {} a -> s {status = a} :: ImportImageTask)

-- | A descriptive status message for the import image task.
importImageTask_statusMessage :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_statusMessage = Lens.lens (\ImportImageTask' {statusMessage} -> statusMessage) (\s@ImportImageTask' {} a -> s {statusMessage = a} :: ImportImageTask)

-- | The tags for the import image task.
importImageTask_tags :: Lens.Lens' ImportImageTask (Prelude.Maybe [Tag])
importImageTask_tags = Lens.lens (\ImportImageTask' {tags} -> tags) (\s@ImportImageTask' {} a -> s {tags = a} :: ImportImageTask) Prelude.. Lens.mapping Lens.coerced

-- | The usage operation value.
importImageTask_usageOperation :: Lens.Lens' ImportImageTask (Prelude.Maybe Prelude.Text)
importImageTask_usageOperation = Lens.lens (\ImportImageTask' {usageOperation} -> usageOperation) (\s@ImportImageTask' {} a -> s {usageOperation = a} :: ImportImageTask)

instance Data.FromXML ImportImageTask where
  parseXML x =
    ImportImageTask'
      Prelude.<$> (x Data..@? "architecture")
      Prelude.<*> (x Data..@? "bootMode")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "encrypted")
      Prelude.<*> (x Data..@? "hypervisor")
      Prelude.<*> (x Data..@? "imageId")
      Prelude.<*> (x Data..@? "importTaskId")
      Prelude.<*> (x Data..@? "kmsKeyId")
      Prelude.<*> ( x
                      Data..@? "licenseSpecifications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "licenseType")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "progress")
      Prelude.<*> ( x
                      Data..@? "snapshotDetailSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "usageOperation")

instance Prelude.Hashable ImportImageTask where
  hashWithSalt _salt ImportImageTask' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` hypervisor
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` importTaskId
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseSpecifications
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` snapshotDetails
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` usageOperation

instance Prelude.NFData ImportImageTask where
  rnf ImportImageTask' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf hypervisor
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf importTaskId
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseSpecifications
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf snapshotDetails
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf usageOperation

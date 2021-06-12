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
-- Module      : Network.AWS.EC2.Types.ImportImageTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageTask where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.SnapshotDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an import image task.
--
-- /See:/ 'newImportImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { -- | The target hypervisor for the import task.
    --
    -- Valid values: @xen@
    hypervisor :: Core.Maybe Core.Text,
    -- | The description string for the import image task.
    platform :: Core.Maybe Core.Text,
    -- | A descriptive status message for the import image task.
    statusMessage :: Core.Maybe Core.Text,
    -- | A brief status for the import image task.
    status :: Core.Maybe Core.Text,
    -- | Information about the snapshots.
    snapshotDetails :: Core.Maybe [SnapshotDetail],
    -- | Indicates whether the image is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The ID of the import image task.
    importTaskId :: Core.Maybe Core.Text,
    -- | The ARNs of the license configurations that are associated with the
    -- import image task.
    licenseSpecifications :: Core.Maybe [ImportImageLicenseConfigurationResponse],
    -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Machine Image (AMI) of the imported virtual
    -- machine.
    imageId :: Core.Maybe Core.Text,
    -- | The identifier for the AWS Key Management Service (AWS KMS) customer
    -- master key (CMK) that was used to create the encrypted image.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The tags for the import image task.
    tags :: Core.Maybe [Tag],
    -- | A description of the import task.
    description :: Core.Maybe Core.Text,
    -- | The license type of the virtual machine.
    licenseType :: Core.Maybe Core.Text,
    -- | The percentage of progress of the import image task.
    progress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportImageTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisor', 'importImageTask_hypervisor' - The target hypervisor for the import task.
--
-- Valid values: @xen@
--
-- 'platform', 'importImageTask_platform' - The description string for the import image task.
--
-- 'statusMessage', 'importImageTask_statusMessage' - A descriptive status message for the import image task.
--
-- 'status', 'importImageTask_status' - A brief status for the import image task.
--
-- 'snapshotDetails', 'importImageTask_snapshotDetails' - Information about the snapshots.
--
-- 'encrypted', 'importImageTask_encrypted' - Indicates whether the image is encrypted.
--
-- 'importTaskId', 'importImageTask_importTaskId' - The ID of the import image task.
--
-- 'licenseSpecifications', 'importImageTask_licenseSpecifications' - The ARNs of the license configurations that are associated with the
-- import image task.
--
-- 'architecture', 'importImageTask_architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- 'imageId', 'importImageTask_imageId' - The ID of the Amazon Machine Image (AMI) of the imported virtual
-- machine.
--
-- 'kmsKeyId', 'importImageTask_kmsKeyId' - The identifier for the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) that was used to create the encrypted image.
--
-- 'tags', 'importImageTask_tags' - The tags for the import image task.
--
-- 'description', 'importImageTask_description' - A description of the import task.
--
-- 'licenseType', 'importImageTask_licenseType' - The license type of the virtual machine.
--
-- 'progress', 'importImageTask_progress' - The percentage of progress of the import image task.
newImportImageTask ::
  ImportImageTask
newImportImageTask =
  ImportImageTask'
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
      progress = Core.Nothing
    }

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@
importImageTask_hypervisor :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_hypervisor = Lens.lens (\ImportImageTask' {hypervisor} -> hypervisor) (\s@ImportImageTask' {} a -> s {hypervisor = a} :: ImportImageTask)

-- | The description string for the import image task.
importImageTask_platform :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_platform = Lens.lens (\ImportImageTask' {platform} -> platform) (\s@ImportImageTask' {} a -> s {platform = a} :: ImportImageTask)

-- | A descriptive status message for the import image task.
importImageTask_statusMessage :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_statusMessage = Lens.lens (\ImportImageTask' {statusMessage} -> statusMessage) (\s@ImportImageTask' {} a -> s {statusMessage = a} :: ImportImageTask)

-- | A brief status for the import image task.
importImageTask_status :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_status = Lens.lens (\ImportImageTask' {status} -> status) (\s@ImportImageTask' {} a -> s {status = a} :: ImportImageTask)

-- | Information about the snapshots.
importImageTask_snapshotDetails :: Lens.Lens' ImportImageTask (Core.Maybe [SnapshotDetail])
importImageTask_snapshotDetails = Lens.lens (\ImportImageTask' {snapshotDetails} -> snapshotDetails) (\s@ImportImageTask' {} a -> s {snapshotDetails = a} :: ImportImageTask) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the image is encrypted.
importImageTask_encrypted :: Lens.Lens' ImportImageTask (Core.Maybe Core.Bool)
importImageTask_encrypted = Lens.lens (\ImportImageTask' {encrypted} -> encrypted) (\s@ImportImageTask' {} a -> s {encrypted = a} :: ImportImageTask)

-- | The ID of the import image task.
importImageTask_importTaskId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_importTaskId = Lens.lens (\ImportImageTask' {importTaskId} -> importTaskId) (\s@ImportImageTask' {} a -> s {importTaskId = a} :: ImportImageTask)

-- | The ARNs of the license configurations that are associated with the
-- import image task.
importImageTask_licenseSpecifications :: Lens.Lens' ImportImageTask (Core.Maybe [ImportImageLicenseConfigurationResponse])
importImageTask_licenseSpecifications = Lens.lens (\ImportImageTask' {licenseSpecifications} -> licenseSpecifications) (\s@ImportImageTask' {} a -> s {licenseSpecifications = a} :: ImportImageTask) Core.. Lens.mapping Lens._Coerce

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
importImageTask_architecture :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_architecture = Lens.lens (\ImportImageTask' {architecture} -> architecture) (\s@ImportImageTask' {} a -> s {architecture = a} :: ImportImageTask)

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual
-- machine.
importImageTask_imageId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_imageId = Lens.lens (\ImportImageTask' {imageId} -> imageId) (\s@ImportImageTask' {} a -> s {imageId = a} :: ImportImageTask)

-- | The identifier for the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) that was used to create the encrypted image.
importImageTask_kmsKeyId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_kmsKeyId = Lens.lens (\ImportImageTask' {kmsKeyId} -> kmsKeyId) (\s@ImportImageTask' {} a -> s {kmsKeyId = a} :: ImportImageTask)

-- | The tags for the import image task.
importImageTask_tags :: Lens.Lens' ImportImageTask (Core.Maybe [Tag])
importImageTask_tags = Lens.lens (\ImportImageTask' {tags} -> tags) (\s@ImportImageTask' {} a -> s {tags = a} :: ImportImageTask) Core.. Lens.mapping Lens._Coerce

-- | A description of the import task.
importImageTask_description :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_description = Lens.lens (\ImportImageTask' {description} -> description) (\s@ImportImageTask' {} a -> s {description = a} :: ImportImageTask)

-- | The license type of the virtual machine.
importImageTask_licenseType :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_licenseType = Lens.lens (\ImportImageTask' {licenseType} -> licenseType) (\s@ImportImageTask' {} a -> s {licenseType = a} :: ImportImageTask)

-- | The percentage of progress of the import image task.
importImageTask_progress :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
importImageTask_progress = Lens.lens (\ImportImageTask' {progress} -> progress) (\s@ImportImageTask' {} a -> s {progress = a} :: ImportImageTask)

instance Core.FromXML ImportImageTask where
  parseXML x =
    ImportImageTask'
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

instance Core.Hashable ImportImageTask

instance Core.NFData ImportImageTask

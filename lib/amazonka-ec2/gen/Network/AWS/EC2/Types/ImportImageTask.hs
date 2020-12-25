{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportImageTask
  ( ImportImageTask (..),

    -- * Smart constructor
    mkImportImageTask,

    -- * Lenses
    iitArchitecture,
    iitDescription,
    iitEncrypted,
    iitHypervisor,
    iitImageId,
    iitImportTaskId,
    iitKmsKeyId,
    iitLicenseSpecifications,
    iitLicenseType,
    iitPlatform,
    iitProgress,
    iitSnapshotDetails,
    iitStatus,
    iitStatusMessage,
    iitTags,
  )
where

import qualified Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse as Types
import qualified Network.AWS.EC2.Types.SnapshotDetail as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import image task.
--
-- /See:/ 'mkImportImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Core.Maybe Types.String,
    -- | A description of the import task.
    description :: Core.Maybe Types.String,
    -- | Indicates whether the image is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The target hypervisor for the import task.
    --
    -- Valid values: @xen@
    hypervisor :: Core.Maybe Types.String,
    -- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
    imageId :: Core.Maybe Types.String,
    -- | The ID of the import image task.
    importTaskId :: Core.Maybe Types.String,
    -- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The ARNs of the license configurations that are associated with the import image task.
    licenseSpecifications :: Core.Maybe [Types.ImportImageLicenseConfigurationResponse],
    -- | The license type of the virtual machine.
    licenseType :: Core.Maybe Types.String,
    -- | The description string for the import image task.
    platform :: Core.Maybe Types.String,
    -- | The percentage of progress of the import image task.
    progress :: Core.Maybe Types.String,
    -- | Information about the snapshots.
    snapshotDetails :: Core.Maybe [Types.SnapshotDetail],
    -- | A brief status for the import image task.
    status :: Core.Maybe Types.String,
    -- | A descriptive status message for the import image task.
    statusMessage :: Core.Maybe Types.String,
    -- | The tags for the import image task.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportImageTask' value with any optional fields omitted.
mkImportImageTask ::
  ImportImageTask
mkImportImageTask =
  ImportImageTask'
    { architecture = Core.Nothing,
      description = Core.Nothing,
      encrypted = Core.Nothing,
      hypervisor = Core.Nothing,
      imageId = Core.Nothing,
      importTaskId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      licenseSpecifications = Core.Nothing,
      licenseType = Core.Nothing,
      platform = Core.Nothing,
      progress = Core.Nothing,
      snapshotDetails = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      tags = Core.Nothing
    }

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitArchitecture :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitArchitecture = Lens.field @"architecture"
{-# DEPRECATED iitArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description of the import task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitDescription :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitDescription = Lens.field @"description"
{-# DEPRECATED iitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the image is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitEncrypted :: Lens.Lens' ImportImageTask (Core.Maybe Core.Bool)
iitEncrypted = Lens.field @"encrypted"
{-# DEPRECATED iitEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitHypervisor :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitHypervisor = Lens.field @"hypervisor"
{-# DEPRECATED iitHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImageId :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitImageId = Lens.field @"imageId"
{-# DEPRECATED iitImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The ID of the import image task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImportTaskId :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitImportTaskId = Lens.field @"importTaskId"
{-# DEPRECATED iitImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitKmsKeyId :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED iitKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The ARNs of the license configurations that are associated with the import image task.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseSpecifications :: Lens.Lens' ImportImageTask (Core.Maybe [Types.ImportImageLicenseConfigurationResponse])
iitLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# DEPRECATED iitLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The license type of the virtual machine.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseType :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitLicenseType = Lens.field @"licenseType"
{-# DEPRECATED iitLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | The description string for the import image task.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitPlatform :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitPlatform = Lens.field @"platform"
{-# DEPRECATED iitPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The percentage of progress of the import image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitProgress :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitProgress = Lens.field @"progress"
{-# DEPRECATED iitProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshotDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitSnapshotDetails :: Lens.Lens' ImportImageTask (Core.Maybe [Types.SnapshotDetail])
iitSnapshotDetails = Lens.field @"snapshotDetails"
{-# DEPRECATED iitSnapshotDetails "Use generic-lens or generic-optics with 'snapshotDetails' instead." #-}

-- | A brief status for the import image task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatus :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitStatus = Lens.field @"status"
{-# DEPRECATED iitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A descriptive status message for the import image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatusMessage :: Lens.Lens' ImportImageTask (Core.Maybe Types.String)
iitStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED iitStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The tags for the import image task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitTags :: Lens.Lens' ImportImageTask (Core.Maybe [Types.Tag])
iitTags = Lens.field @"tags"
{-# DEPRECATED iitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ImportImageTask where
  parseXML x =
    ImportImageTask'
      Core.<$> (x Core..@? "architecture")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "encrypted")
      Core.<*> (x Core..@? "hypervisor")
      Core.<*> (x Core..@? "imageId")
      Core.<*> (x Core..@? "importTaskId")
      Core.<*> (x Core..@? "kmsKeyId")
      Core.<*> ( x Core..@? "licenseSpecifications"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "licenseType")
      Core.<*> (x Core..@? "platform")
      Core.<*> (x Core..@? "progress")
      Core.<*> (x Core..@? "snapshotDetailSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")

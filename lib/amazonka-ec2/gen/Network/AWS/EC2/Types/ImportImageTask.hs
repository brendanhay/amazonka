{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportImageTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImportImageTask
  ( ImportImageTask (..)
  -- * Smart constructor
  , mkImportImageTask
  -- * Lenses
  , iitArchitecture
  , iitDescription
  , iitEncrypted
  , iitHypervisor
  , iitImageId
  , iitImportTaskId
  , iitKmsKeyId
  , iitLicenseSpecifications
  , iitLicenseType
  , iitPlatform
  , iitProgress
  , iitSnapshotDetails
  , iitStatus
  , iitStatusMessage
  , iitTags
  ) where

import qualified Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse as Types
import qualified Network.AWS.EC2.Types.SnapshotDetail as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import image task.
--
-- /See:/ 'mkImportImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { architecture :: Core.Maybe Core.Text
    -- ^ The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@ 
  , description :: Core.Maybe Core.Text
    -- ^ A description of the import task.
  , encrypted :: Core.Maybe Core.Bool
    -- ^ Indicates whether the image is encrypted.
  , hypervisor :: Core.Maybe Core.Text
    -- ^ The target hypervisor for the import task.
--
-- Valid values: @xen@ 
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
  , importTaskId :: Core.Maybe Core.Text
    -- ^ The ID of the import image task.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
  , licenseSpecifications :: Core.Maybe [Types.ImportImageLicenseConfigurationResponse]
    -- ^ The ARNs of the license configurations that are associated with the import image task.
  , licenseType :: Core.Maybe Core.Text
    -- ^ The license type of the virtual machine.
  , platform :: Core.Maybe Core.Text
    -- ^ The description string for the import image task.
  , progress :: Core.Maybe Core.Text
    -- ^ The percentage of progress of the import image task.
  , snapshotDetails :: Core.Maybe [Types.SnapshotDetail]
    -- ^ Information about the snapshots.
  , status :: Core.Maybe Core.Text
    -- ^ A brief status for the import image task.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A descriptive status message for the import image task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the import image task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportImageTask' value with any optional fields omitted.
mkImportImageTask
    :: ImportImageTask
mkImportImageTask
  = ImportImageTask'{architecture = Core.Nothing,
                     description = Core.Nothing, encrypted = Core.Nothing,
                     hypervisor = Core.Nothing, imageId = Core.Nothing,
                     importTaskId = Core.Nothing, kmsKeyId = Core.Nothing,
                     licenseSpecifications = Core.Nothing, licenseType = Core.Nothing,
                     platform = Core.Nothing, progress = Core.Nothing,
                     snapshotDetails = Core.Nothing, status = Core.Nothing,
                     statusMessage = Core.Nothing, tags = Core.Nothing}

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@ 
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitArchitecture :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitArchitecture = Lens.field @"architecture"
{-# INLINEABLE iitArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | A description of the import task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitDescription :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitDescription = Lens.field @"description"
{-# INLINEABLE iitDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Indicates whether the image is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitEncrypted :: Lens.Lens' ImportImageTask (Core.Maybe Core.Bool)
iitEncrypted = Lens.field @"encrypted"
{-# INLINEABLE iitEncrypted #-}
{-# DEPRECATED encrypted "Use generic-lens or generic-optics with 'encrypted' instead"  #-}

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@ 
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitHypervisor :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitHypervisor = Lens.field @"hypervisor"
{-# INLINEABLE iitHypervisor #-}
{-# DEPRECATED hypervisor "Use generic-lens or generic-optics with 'hypervisor' instead"  #-}

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImageId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitImageId = Lens.field @"imageId"
{-# INLINEABLE iitImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The ID of the import image task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImportTaskId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE iitImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitKmsKeyId :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE iitKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The ARNs of the license configurations that are associated with the import image task.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseSpecifications :: Lens.Lens' ImportImageTask (Core.Maybe [Types.ImportImageLicenseConfigurationResponse])
iitLicenseSpecifications = Lens.field @"licenseSpecifications"
{-# INLINEABLE iitLicenseSpecifications #-}
{-# DEPRECATED licenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead"  #-}

-- | The license type of the virtual machine.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseType :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitLicenseType = Lens.field @"licenseType"
{-# INLINEABLE iitLicenseType #-}
{-# DEPRECATED licenseType "Use generic-lens or generic-optics with 'licenseType' instead"  #-}

-- | The description string for the import image task.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitPlatform :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitPlatform = Lens.field @"platform"
{-# INLINEABLE iitPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The percentage of progress of the import image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitProgress :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitProgress = Lens.field @"progress"
{-# INLINEABLE iitProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshotDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitSnapshotDetails :: Lens.Lens' ImportImageTask (Core.Maybe [Types.SnapshotDetail])
iitSnapshotDetails = Lens.field @"snapshotDetails"
{-# INLINEABLE iitSnapshotDetails #-}
{-# DEPRECATED snapshotDetails "Use generic-lens or generic-optics with 'snapshotDetails' instead"  #-}

-- | A brief status for the import image task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatus :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitStatus = Lens.field @"status"
{-# INLINEABLE iitStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A descriptive status message for the import image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatusMessage :: Lens.Lens' ImportImageTask (Core.Maybe Core.Text)
iitStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE iitStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The tags for the import image task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitTags :: Lens.Lens' ImportImageTask (Core.Maybe [Types.Tag])
iitTags = Lens.field @"tags"
{-# INLINEABLE iitTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML ImportImageTask where
        parseXML x
          = ImportImageTask' Core.<$>
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

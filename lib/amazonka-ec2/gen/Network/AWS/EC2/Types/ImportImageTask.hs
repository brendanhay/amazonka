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
    iitStatus,
    iitHypervisor,
    iitPlatform,
    iitProgress,
    iitLicenseSpecifications,
    iitLicenseType,
    iitSnapshotDetails,
    iitEncrypted,
    iitKMSKeyId,
    iitStatusMessage,
    iitImageId,
    iitImportTaskId,
    iitArchitecture,
    iitDescription,
    iitTags,
  )
where

import Network.AWS.EC2.Types.ImportImageLicenseConfigurationResponse
import Network.AWS.EC2.Types.SnapshotDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an import image task.
--
-- /See:/ 'mkImportImageTask' smart constructor.
data ImportImageTask = ImportImageTask'
  { -- | A brief status for the import image task.
    status :: Lude.Maybe Lude.Text,
    -- | The target hypervisor for the import task.
    --
    -- Valid values: @xen@
    hypervisor :: Lude.Maybe Lude.Text,
    -- | The description string for the import image task.
    platform :: Lude.Maybe Lude.Text,
    -- | The percentage of progress of the import image task.
    progress :: Lude.Maybe Lude.Text,
    -- | The ARNs of the license configurations that are associated with the import image task.
    licenseSpecifications :: Lude.Maybe [ImportImageLicenseConfigurationResponse],
    -- | The license type of the virtual machine.
    licenseType :: Lude.Maybe Lude.Text,
    -- | Information about the snapshots.
    snapshotDetails :: Lude.Maybe [SnapshotDetail],
    -- | Indicates whether the image is encrypted.
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A descriptive status message for the import image task.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
    imageId :: Lude.Maybe Lude.Text,
    -- | The ID of the import image task.
    importTaskId :: Lude.Maybe Lude.Text,
    -- | The architecture of the virtual machine.
    --
    -- Valid values: @i386@ | @x86_64@ | @arm64@
    architecture :: Lude.Maybe Lude.Text,
    -- | A description of the import task.
    description :: Lude.Maybe Lude.Text,
    -- | The tags for the import image task.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportImageTask' with the minimum fields required to make a request.
--
-- * 'status' - A brief status for the import image task.
-- * 'hypervisor' - The target hypervisor for the import task.
--
-- Valid values: @xen@
-- * 'platform' - The description string for the import image task.
-- * 'progress' - The percentage of progress of the import image task.
-- * 'licenseSpecifications' - The ARNs of the license configurations that are associated with the import image task.
-- * 'licenseType' - The license type of the virtual machine.
-- * 'snapshotDetails' - Information about the snapshots.
-- * 'encrypted' - Indicates whether the image is encrypted.
-- * 'kmsKeyId' - The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
-- * 'statusMessage' - A descriptive status message for the import image task.
-- * 'imageId' - The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
-- * 'importTaskId' - The ID of the import image task.
-- * 'architecture' - The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
-- * 'description' - A description of the import task.
-- * 'tags' - The tags for the import image task.
mkImportImageTask ::
  ImportImageTask
mkImportImageTask =
  ImportImageTask'
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
      tags = Lude.Nothing
    }

-- | A brief status for the import image task.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatus :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitStatus = Lens.lens (status :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ImportImageTask)
{-# DEPRECATED iitStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The target hypervisor for the import task.
--
-- Valid values: @xen@
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitHypervisor :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitHypervisor = Lens.lens (hypervisor :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {hypervisor = a} :: ImportImageTask)
{-# DEPRECATED iitHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The description string for the import image task.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitPlatform :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitPlatform = Lens.lens (platform :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ImportImageTask)
{-# DEPRECATED iitPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The percentage of progress of the import image task.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitProgress :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitProgress = Lens.lens (progress :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: ImportImageTask)
{-# DEPRECATED iitProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The ARNs of the license configurations that are associated with the import image task.
--
-- /Note:/ Consider using 'licenseSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseSpecifications :: Lens.Lens' ImportImageTask (Lude.Maybe [ImportImageLicenseConfigurationResponse])
iitLicenseSpecifications = Lens.lens (licenseSpecifications :: ImportImageTask -> Lude.Maybe [ImportImageLicenseConfigurationResponse]) (\s a -> s {licenseSpecifications = a} :: ImportImageTask)
{-# DEPRECATED iitLicenseSpecifications "Use generic-lens or generic-optics with 'licenseSpecifications' instead." #-}

-- | The license type of the virtual machine.
--
-- /Note:/ Consider using 'licenseType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitLicenseType :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitLicenseType = Lens.lens (licenseType :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {licenseType = a} :: ImportImageTask)
{-# DEPRECATED iitLicenseType "Use generic-lens or generic-optics with 'licenseType' instead." #-}

-- | Information about the snapshots.
--
-- /Note:/ Consider using 'snapshotDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitSnapshotDetails :: Lens.Lens' ImportImageTask (Lude.Maybe [SnapshotDetail])
iitSnapshotDetails = Lens.lens (snapshotDetails :: ImportImageTask -> Lude.Maybe [SnapshotDetail]) (\s a -> s {snapshotDetails = a} :: ImportImageTask)
{-# DEPRECATED iitSnapshotDetails "Use generic-lens or generic-optics with 'snapshotDetails' instead." #-}

-- | Indicates whether the image is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitEncrypted :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Bool)
iitEncrypted = Lens.lens (encrypted :: ImportImageTask -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: ImportImageTask)
{-# DEPRECATED iitEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to create the encrypted image.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitKMSKeyId :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitKMSKeyId = Lens.lens (kmsKeyId :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ImportImageTask)
{-# DEPRECATED iitKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A descriptive status message for the import image task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitStatusMessage :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitStatusMessage = Lens.lens (statusMessage :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ImportImageTask)
{-# DEPRECATED iitStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) of the imported virtual machine.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImageId :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitImageId = Lens.lens (imageId :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: ImportImageTask)
{-# DEPRECATED iitImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The ID of the import image task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitImportTaskId :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitImportTaskId = Lens.lens (importTaskId :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: ImportImageTask)
{-# DEPRECATED iitImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The architecture of the virtual machine.
--
-- Valid values: @i386@ | @x86_64@ | @arm64@
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitArchitecture :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitArchitecture = Lens.lens (architecture :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {architecture = a} :: ImportImageTask)
{-# DEPRECATED iitArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description of the import task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitDescription :: Lens.Lens' ImportImageTask (Lude.Maybe Lude.Text)
iitDescription = Lens.lens (description :: ImportImageTask -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportImageTask)
{-# DEPRECATED iitDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the import image task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitTags :: Lens.Lens' ImportImageTask (Lude.Maybe [Tag])
iitTags = Lens.lens (tags :: ImportImageTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ImportImageTask)
{-# DEPRECATED iitTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ImportImageTask where
  parseXML x =
    ImportImageTask'
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

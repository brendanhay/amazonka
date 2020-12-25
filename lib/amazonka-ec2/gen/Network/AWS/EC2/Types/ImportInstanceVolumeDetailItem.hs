{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
  ( ImportInstanceVolumeDetailItem (..),

    -- * Smart constructor
    mkImportInstanceVolumeDetailItem,

    -- * Lenses
    iivdiAvailabilityZone,
    iivdiBytesConverted,
    iivdiDescription,
    iivdiImage,
    iivdiStatus,
    iivdiStatusMessage,
    iivdiVolume,
  )
where

import qualified Network.AWS.EC2.Types.DiskImageDescription as Types
import qualified Network.AWS.EC2.Types.DiskImageVolumeDescription as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import volume task.
--
-- /See:/ 'mkImportInstanceVolumeDetailItem' smart constructor.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem'
  { -- | The Availability Zone where the resulting instance will reside.
    availabilityZone :: Core.Maybe Types.String,
    -- | The number of bytes converted so far.
    bytesConverted :: Core.Maybe Core.Integer,
    -- | A description of the task.
    description :: Core.Maybe Types.String,
    -- | The image.
    image :: Core.Maybe Types.DiskImageDescription,
    -- | The status of the import of this particular disk image.
    status :: Core.Maybe Types.String,
    -- | The status information or errors related to the disk image.
    statusMessage :: Core.Maybe Types.String,
    -- | The volume.
    volume :: Core.Maybe Types.DiskImageVolumeDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstanceVolumeDetailItem' value with any optional fields omitted.
mkImportInstanceVolumeDetailItem ::
  ImportInstanceVolumeDetailItem
mkImportInstanceVolumeDetailItem =
  ImportInstanceVolumeDetailItem'
    { availabilityZone = Core.Nothing,
      bytesConverted = Core.Nothing,
      description = Core.Nothing,
      image = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing,
      volume = Core.Nothing
    }

-- | The Availability Zone where the resulting instance will reside.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiAvailabilityZone :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.String)
iivdiAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED iivdiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of bytes converted so far.
--
-- /Note:/ Consider using 'bytesConverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiBytesConverted :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Core.Integer)
iivdiBytesConverted = Lens.field @"bytesConverted"
{-# DEPRECATED iivdiBytesConverted "Use generic-lens or generic-optics with 'bytesConverted' instead." #-}

-- | A description of the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiDescription :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.String)
iivdiDescription = Lens.field @"description"
{-# DEPRECATED iivdiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiImage :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.DiskImageDescription)
iivdiImage = Lens.field @"image"
{-# DEPRECATED iivdiImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The status of the import of this particular disk image.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiStatus :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.String)
iivdiStatus = Lens.field @"status"
{-# DEPRECATED iivdiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status information or errors related to the disk image.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiStatusMessage :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.String)
iivdiStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED iivdiStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiVolume :: Lens.Lens' ImportInstanceVolumeDetailItem (Core.Maybe Types.DiskImageVolumeDescription)
iivdiVolume = Lens.field @"volume"
{-# DEPRECATED iivdiVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

instance Core.FromXML ImportInstanceVolumeDetailItem where
  parseXML x =
    ImportInstanceVolumeDetailItem'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "bytesConverted")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "image")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "volume")

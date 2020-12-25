{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportVolumeTaskDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportVolumeTaskDetails
  ( ImportVolumeTaskDetails (..),

    -- * Smart constructor
    mkImportVolumeTaskDetails,

    -- * Lenses
    ivtdAvailabilityZone,
    ivtdBytesConverted,
    ivtdDescription,
    ivtdImage,
    ivtdVolume,
  )
where

import qualified Network.AWS.EC2.Types.DiskImageDescription as Types
import qualified Network.AWS.EC2.Types.DiskImageVolumeDescription as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import volume task.
--
-- /See:/ 'mkImportVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { -- | The Availability Zone where the resulting volume will reside.
    availabilityZone :: Core.Maybe Types.String,
    -- | The number of bytes converted so far.
    bytesConverted :: Core.Maybe Core.Integer,
    -- | The description you provided when starting the import volume task.
    description :: Core.Maybe Types.String,
    -- | The image.
    image :: Core.Maybe Types.DiskImageDescription,
    -- | The volume.
    volume :: Core.Maybe Types.DiskImageVolumeDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportVolumeTaskDetails' value with any optional fields omitted.
mkImportVolumeTaskDetails ::
  ImportVolumeTaskDetails
mkImportVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { availabilityZone = Core.Nothing,
      bytesConverted = Core.Nothing,
      description = Core.Nothing,
      image = Core.Nothing,
      volume = Core.Nothing
    }

-- | The Availability Zone where the resulting volume will reside.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdAvailabilityZone :: Lens.Lens' ImportVolumeTaskDetails (Core.Maybe Types.String)
ivtdAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ivtdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of bytes converted so far.
--
-- /Note:/ Consider using 'bytesConverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdBytesConverted :: Lens.Lens' ImportVolumeTaskDetails (Core.Maybe Core.Integer)
ivtdBytesConverted = Lens.field @"bytesConverted"
{-# DEPRECATED ivtdBytesConverted "Use generic-lens or generic-optics with 'bytesConverted' instead." #-}

-- | The description you provided when starting the import volume task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdDescription :: Lens.Lens' ImportVolumeTaskDetails (Core.Maybe Types.String)
ivtdDescription = Lens.field @"description"
{-# DEPRECATED ivtdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdImage :: Lens.Lens' ImportVolumeTaskDetails (Core.Maybe Types.DiskImageDescription)
ivtdImage = Lens.field @"image"
{-# DEPRECATED ivtdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdVolume :: Lens.Lens' ImportVolumeTaskDetails (Core.Maybe Types.DiskImageVolumeDescription)
ivtdVolume = Lens.field @"volume"
{-# DEPRECATED ivtdVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

instance Core.FromXML ImportVolumeTaskDetails where
  parseXML x =
    ImportVolumeTaskDetails'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "bytesConverted")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "image")
      Core.<*> (x Core..@? "volume")

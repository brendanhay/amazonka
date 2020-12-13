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
    ivtdBytesConverted,
    ivtdImage,
    ivtdVolume,
    ivtdAvailabilityZone,
    ivtdDescription,
  )
where

import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an import volume task.
--
-- /See:/ 'mkImportVolumeTaskDetails' smart constructor.
data ImportVolumeTaskDetails = ImportVolumeTaskDetails'
  { -- | The number of bytes converted so far.
    bytesConverted :: Lude.Maybe Lude.Integer,
    -- | The image.
    image :: Lude.Maybe DiskImageDescription,
    -- | The volume.
    volume :: Lude.Maybe DiskImageVolumeDescription,
    -- | The Availability Zone where the resulting volume will reside.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The description you provided when starting the import volume task.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportVolumeTaskDetails' with the minimum fields required to make a request.
--
-- * 'bytesConverted' - The number of bytes converted so far.
-- * 'image' - The image.
-- * 'volume' - The volume.
-- * 'availabilityZone' - The Availability Zone where the resulting volume will reside.
-- * 'description' - The description you provided when starting the import volume task.
mkImportVolumeTaskDetails ::
  ImportVolumeTaskDetails
mkImportVolumeTaskDetails =
  ImportVolumeTaskDetails'
    { bytesConverted = Lude.Nothing,
      image = Lude.Nothing,
      volume = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The number of bytes converted so far.
--
-- /Note:/ Consider using 'bytesConverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdBytesConverted :: Lens.Lens' ImportVolumeTaskDetails (Lude.Maybe Lude.Integer)
ivtdBytesConverted = Lens.lens (bytesConverted :: ImportVolumeTaskDetails -> Lude.Maybe Lude.Integer) (\s a -> s {bytesConverted = a} :: ImportVolumeTaskDetails)
{-# DEPRECATED ivtdBytesConverted "Use generic-lens or generic-optics with 'bytesConverted' instead." #-}

-- | The image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdImage :: Lens.Lens' ImportVolumeTaskDetails (Lude.Maybe DiskImageDescription)
ivtdImage = Lens.lens (image :: ImportVolumeTaskDetails -> Lude.Maybe DiskImageDescription) (\s a -> s {image = a} :: ImportVolumeTaskDetails)
{-# DEPRECATED ivtdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdVolume :: Lens.Lens' ImportVolumeTaskDetails (Lude.Maybe DiskImageVolumeDescription)
ivtdVolume = Lens.lens (volume :: ImportVolumeTaskDetails -> Lude.Maybe DiskImageVolumeDescription) (\s a -> s {volume = a} :: ImportVolumeTaskDetails)
{-# DEPRECATED ivtdVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

-- | The Availability Zone where the resulting volume will reside.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdAvailabilityZone :: Lens.Lens' ImportVolumeTaskDetails (Lude.Maybe Lude.Text)
ivtdAvailabilityZone = Lens.lens (availabilityZone :: ImportVolumeTaskDetails -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ImportVolumeTaskDetails)
{-# DEPRECATED ivtdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The description you provided when starting the import volume task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivtdDescription :: Lens.Lens' ImportVolumeTaskDetails (Lude.Maybe Lude.Text)
ivtdDescription = Lens.lens (description :: ImportVolumeTaskDetails -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportVolumeTaskDetails)
{-# DEPRECATED ivtdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ImportVolumeTaskDetails where
  parseXML x =
    ImportVolumeTaskDetails'
      Lude.<$> (x Lude..@? "bytesConverted")
      Lude.<*> (x Lude..@? "image")
      Lude.<*> (x Lude..@? "volume")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "description")

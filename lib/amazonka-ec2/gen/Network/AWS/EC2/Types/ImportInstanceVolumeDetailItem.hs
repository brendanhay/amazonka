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
    iivdiStatus,
    iivdiBytesConverted,
    iivdiImage,
    iivdiVolume,
    iivdiAvailabilityZone,
    iivdiStatusMessage,
    iivdiDescription,
  )
where

import Network.AWS.EC2.Types.DiskImageDescription
import Network.AWS.EC2.Types.DiskImageVolumeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an import volume task.
--
-- /See:/ 'mkImportInstanceVolumeDetailItem' smart constructor.
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem'
  { status ::
      Lude.Maybe Lude.Text,
    bytesConverted ::
      Lude.Maybe Lude.Integer,
    image ::
      Lude.Maybe
        DiskImageDescription,
    volume ::
      Lude.Maybe
        DiskImageVolumeDescription,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    statusMessage ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstanceVolumeDetailItem' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone where the resulting instance will reside.
-- * 'bytesConverted' - The number of bytes converted so far.
-- * 'description' - A description of the task.
-- * 'image' - The image.
-- * 'status' - The status of the import of this particular disk image.
-- * 'statusMessage' - The status information or errors related to the disk image.
-- * 'volume' - The volume.
mkImportInstanceVolumeDetailItem ::
  ImportInstanceVolumeDetailItem
mkImportInstanceVolumeDetailItem =
  ImportInstanceVolumeDetailItem'
    { status = Lude.Nothing,
      bytesConverted = Lude.Nothing,
      image = Lude.Nothing,
      volume = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      statusMessage = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The status of the import of this particular disk image.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiStatus :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe Lude.Text)
iivdiStatus = Lens.lens (status :: ImportInstanceVolumeDetailItem -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of bytes converted so far.
--
-- /Note:/ Consider using 'bytesConverted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiBytesConverted :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe Lude.Integer)
iivdiBytesConverted = Lens.lens (bytesConverted :: ImportInstanceVolumeDetailItem -> Lude.Maybe Lude.Integer) (\s a -> s {bytesConverted = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiBytesConverted "Use generic-lens or generic-optics with 'bytesConverted' instead." #-}

-- | The image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiImage :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe DiskImageDescription)
iivdiImage = Lens.lens (image :: ImportInstanceVolumeDetailItem -> Lude.Maybe DiskImageDescription) (\s a -> s {image = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiVolume :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe DiskImageVolumeDescription)
iivdiVolume = Lens.lens (volume :: ImportInstanceVolumeDetailItem -> Lude.Maybe DiskImageVolumeDescription) (\s a -> s {volume = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

-- | The Availability Zone where the resulting instance will reside.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiAvailabilityZone :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe Lude.Text)
iivdiAvailabilityZone = Lens.lens (availabilityZone :: ImportInstanceVolumeDetailItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The status information or errors related to the disk image.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiStatusMessage :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe Lude.Text)
iivdiStatusMessage = Lens.lens (statusMessage :: ImportInstanceVolumeDetailItem -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | A description of the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iivdiDescription :: Lens.Lens' ImportInstanceVolumeDetailItem (Lude.Maybe Lude.Text)
iivdiDescription = Lens.lens (description :: ImportInstanceVolumeDetailItem -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportInstanceVolumeDetailItem)
{-# DEPRECATED iivdiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ImportInstanceVolumeDetailItem where
  parseXML x =
    ImportInstanceVolumeDetailItem'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "bytesConverted")
      Lude.<*> (x Lude..@? "image")
      Lude.<*> (x Lude..@? "volume")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "description")

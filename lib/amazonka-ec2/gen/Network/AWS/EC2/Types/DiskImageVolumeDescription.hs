{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageVolumeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageVolumeDescription
  ( DiskImageVolumeDescription (..),

    -- * Smart constructor
    mkDiskImageVolumeDescription,

    -- * Lenses
    divdSize,
    divdId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk image volume.
--
-- /See:/ 'mkDiskImageVolumeDescription' smart constructor.
data DiskImageVolumeDescription = DiskImageVolumeDescription'
  { -- | The size of the volume, in GiB.
    size :: Lude.Maybe Lude.Integer,
    -- | The volume identifier.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskImageVolumeDescription' with the minimum fields required to make a request.
--
-- * 'size' - The size of the volume, in GiB.
-- * 'id' - The volume identifier.
mkDiskImageVolumeDescription ::
  DiskImageVolumeDescription
mkDiskImageVolumeDescription =
  DiskImageVolumeDescription'
    { size = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divdSize :: Lens.Lens' DiskImageVolumeDescription (Lude.Maybe Lude.Integer)
divdSize = Lens.lens (size :: DiskImageVolumeDescription -> Lude.Maybe Lude.Integer) (\s a -> s {size = a} :: DiskImageVolumeDescription)
{-# DEPRECATED divdSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The volume identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divdId :: Lens.Lens' DiskImageVolumeDescription (Lude.Maybe Lude.Text)
divdId = Lens.lens (id :: DiskImageVolumeDescription -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DiskImageVolumeDescription)
{-# DEPRECATED divdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML DiskImageVolumeDescription where
  parseXML x =
    DiskImageVolumeDescription'
      Lude.<$> (x Lude..@? "size") Lude.<*> (x Lude..@? "id")

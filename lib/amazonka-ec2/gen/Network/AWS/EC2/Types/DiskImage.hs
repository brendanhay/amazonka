{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImage
  ( DiskImage (..),

    -- * Smart constructor
    mkDiskImage,

    -- * Lenses
    diImage,
    diVolume,
    diDescription,
  )
where

import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.VolumeDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImage' smart constructor.
data DiskImage = DiskImage'
  { image :: Lude.Maybe DiskImageDetail,
    volume :: Lude.Maybe VolumeDetail,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskImage' with the minimum fields required to make a request.
--
-- * 'description' - A description of the disk image.
-- * 'image' - Information about the disk image.
-- * 'volume' - Information about the volume.
mkDiskImage ::
  DiskImage
mkDiskImage =
  DiskImage'
    { image = Lude.Nothing,
      volume = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Information about the disk image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImage :: Lens.Lens' DiskImage (Lude.Maybe DiskImageDetail)
diImage = Lens.lens (image :: DiskImage -> Lude.Maybe DiskImageDetail) (\s a -> s {image = a} :: DiskImage)
{-# DEPRECATED diImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Information about the volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diVolume :: Lens.Lens' DiskImage (Lude.Maybe VolumeDetail)
diVolume = Lens.lens (volume :: DiskImage -> Lude.Maybe VolumeDetail) (\s a -> s {volume = a} :: DiskImage)
{-# DEPRECATED diVolume "Use generic-lens or generic-optics with 'volume' instead." #-}

-- | A description of the disk image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DiskImage (Lude.Maybe Lude.Text)
diDescription = Lens.lens (description :: DiskImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DiskImage)
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToQuery DiskImage where
  toQuery DiskImage' {..} =
    Lude.mconcat
      [ "Image" Lude.=: image,
        "Volume" Lude.=: volume,
        "Description" Lude.=: description
      ]

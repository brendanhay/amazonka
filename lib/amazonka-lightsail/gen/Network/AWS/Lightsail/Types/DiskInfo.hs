{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskInfo
  ( DiskInfo (..),

    -- * Smart constructor
    mkDiskInfo,

    -- * Lenses
    diPath,
    diName,
    diSizeInGb,
    diIsSystemDisk,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a disk.
--
-- /See:/ 'mkDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { path :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int,
    isSystemDisk :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskInfo' with the minimum fields required to make a request.
--
-- * 'isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
-- * 'name' - The disk name.
-- * 'path' - The disk path.
-- * 'sizeInGb' - The size of the disk in GB (e.g., @32@ ).
mkDiskInfo ::
  DiskInfo
mkDiskInfo =
  DiskInfo'
    { path = Lude.Nothing,
      name = Lude.Nothing,
      sizeInGb = Lude.Nothing,
      isSystemDisk = Lude.Nothing
    }

-- | The disk path.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPath :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Text)
diPath = Lens.lens (path :: DiskInfo -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: DiskInfo)
{-# DEPRECATED diPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The disk name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Text)
diName = Lens.lens (name :: DiskInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DiskInfo)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSizeInGb :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Int)
diSizeInGb = Lens.lens (sizeInGb :: DiskInfo -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: DiskInfo)
{-# DEPRECATED diSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- /Note:/ Consider using 'isSystemDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIsSystemDisk :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Bool)
diIsSystemDisk = Lens.lens (isSystemDisk :: DiskInfo -> Lude.Maybe Lude.Bool) (\s a -> s {isSystemDisk = a} :: DiskInfo)
{-# DEPRECATED diIsSystemDisk "Use generic-lens or generic-optics with 'isSystemDisk' instead." #-}

instance Lude.FromJSON DiskInfo where
  parseJSON =
    Lude.withObject
      "DiskInfo"
      ( \x ->
          DiskInfo'
            Lude.<$> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sizeInGb")
            Lude.<*> (x Lude..:? "isSystemDisk")
      )

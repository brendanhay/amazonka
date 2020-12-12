{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskInfo
  ( DiskInfo (..),

    -- * Smart constructor
    mkDiskInfo,

    -- * Lenses
    diCount,
    diSizeInGB,
    diType,
  )
where

import Network.AWS.EC2.Types.DiskType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the disk.
--
-- /See:/ 'mkDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { count :: Lude.Maybe Lude.Int,
    sizeInGB :: Lude.Maybe Lude.Integer,
    type' :: Lude.Maybe DiskType
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
-- * 'count' - The number of disks with this configuration.
-- * 'sizeInGB' - The size of the disk in GB.
-- * 'type'' - The type of disk.
mkDiskInfo ::
  DiskInfo
mkDiskInfo =
  DiskInfo'
    { count = Lude.Nothing,
      sizeInGB = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The number of disks with this configuration.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCount :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Int)
diCount = Lens.lens (count :: DiskInfo -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: DiskInfo)
{-# DEPRECATED diCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSizeInGB :: Lens.Lens' DiskInfo (Lude.Maybe Lude.Integer)
diSizeInGB = Lens.lens (sizeInGB :: DiskInfo -> Lude.Maybe Lude.Integer) (\s a -> s {sizeInGB = a} :: DiskInfo)
{-# DEPRECATED diSizeInGB "Use generic-lens or generic-optics with 'sizeInGB' instead." #-}

-- | The type of disk.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diType :: Lens.Lens' DiskInfo (Lude.Maybe DiskType)
diType = Lens.lens (type' :: DiskInfo -> Lude.Maybe DiskType) (\s a -> s {type' = a} :: DiskInfo)
{-# DEPRECATED diType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML DiskInfo where
  parseXML x =
    DiskInfo'
      Lude.<$> (x Lude..@? "count")
      Lude.<*> (x Lude..@? "sizeInGB")
      Lude.<*> (x Lude..@? "type")

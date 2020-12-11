-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskMap
  ( DiskMap (..),

    -- * Smart constructor
    mkDiskMap,

    -- * Lenses
    dmNewDiskName,
    dmOriginalDiskPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block storage disk mapping.
--
-- /See:/ 'mkDiskMap' smart constructor.
data DiskMap = DiskMap'
  { newDiskName :: Lude.Maybe Lude.Text,
    originalDiskPath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiskMap' with the minimum fields required to make a request.
--
-- * 'newDiskName' - The new disk name (e.g., @my-new-disk@ ).
-- * 'originalDiskPath' - The original disk path exposed to the instance (for example, @/dev/sdh@ ).
mkDiskMap ::
  DiskMap
mkDiskMap =
  DiskMap'
    { newDiskName = Lude.Nothing,
      originalDiskPath = Lude.Nothing
    }

-- | The new disk name (e.g., @my-new-disk@ ).
--
-- /Note:/ Consider using 'newDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmNewDiskName :: Lens.Lens' DiskMap (Lude.Maybe Lude.Text)
dmNewDiskName = Lens.lens (newDiskName :: DiskMap -> Lude.Maybe Lude.Text) (\s a -> s {newDiskName = a} :: DiskMap)
{-# DEPRECATED dmNewDiskName "Use generic-lens or generic-optics with 'newDiskName' instead." #-}

-- | The original disk path exposed to the instance (for example, @/dev/sdh@ ).
--
-- /Note:/ Consider using 'originalDiskPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmOriginalDiskPath :: Lens.Lens' DiskMap (Lude.Maybe Lude.Text)
dmOriginalDiskPath = Lens.lens (originalDiskPath :: DiskMap -> Lude.Maybe Lude.Text) (\s a -> s {originalDiskPath = a} :: DiskMap)
{-# DEPRECATED dmOriginalDiskPath "Use generic-lens or generic-optics with 'originalDiskPath' instead." #-}

instance Lude.ToJSON DiskMap where
  toJSON DiskMap' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("newDiskName" Lude..=) Lude.<$> newDiskName,
            ("originalDiskPath" Lude..=) Lude.<$> originalDiskPath
          ]
      )

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
    diIsSystemDisk,
    diName,
    diPath,
    diSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Path as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a disk.
--
-- /See:/ 'mkDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { -- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
    isSystemDisk :: Core.Maybe Core.Bool,
    -- | The disk name.
    name :: Core.Maybe Types.String,
    -- | The disk path.
    path :: Core.Maybe Types.Path,
    -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskInfo' value with any optional fields omitted.
mkDiskInfo ::
  DiskInfo
mkDiskInfo =
  DiskInfo'
    { isSystemDisk = Core.Nothing,
      name = Core.Nothing,
      path = Core.Nothing,
      sizeInGb = Core.Nothing
    }

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- /Note:/ Consider using 'isSystemDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIsSystemDisk :: Lens.Lens' DiskInfo (Core.Maybe Core.Bool)
diIsSystemDisk = Lens.field @"isSystemDisk"
{-# DEPRECATED diIsSystemDisk "Use generic-lens or generic-optics with 'isSystemDisk' instead." #-}

-- | The disk name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DiskInfo (Core.Maybe Types.String)
diName = Lens.field @"name"
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The disk path.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPath :: Lens.Lens' DiskInfo (Core.Maybe Types.Path)
diPath = Lens.field @"path"
{-# DEPRECATED diPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSizeInGb :: Lens.Lens' DiskInfo (Core.Maybe Core.Int)
diSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED diSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Core.FromJSON DiskInfo where
  parseJSON =
    Core.withObject "DiskInfo" Core.$
      \x ->
        DiskInfo'
          Core.<$> (x Core..:? "isSystemDisk")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "path")
          Core.<*> (x Core..:? "sizeInGb")

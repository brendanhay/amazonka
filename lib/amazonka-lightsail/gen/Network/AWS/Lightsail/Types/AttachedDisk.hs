{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AttachedDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AttachedDisk
  ( AttachedDisk (..),

    -- * Smart constructor
    mkAttachedDisk,

    -- * Lenses
    adPath,
    adSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a block storage disk that is attached to an instance, and is included in an automatic snapshot.
--
-- /See:/ 'mkAttachedDisk' smart constructor.
data AttachedDisk = AttachedDisk'
  { -- | The path of the disk (e.g., @/dev/xvdf@ ).
    path :: Core.Maybe Types.String,
    -- | The size of the disk in GB.
    sizeInGb :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachedDisk' value with any optional fields omitted.
mkAttachedDisk ::
  AttachedDisk
mkAttachedDisk =
  AttachedDisk' {path = Core.Nothing, sizeInGb = Core.Nothing}

-- | The path of the disk (e.g., @/dev/xvdf@ ).
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adPath :: Lens.Lens' AttachedDisk (Core.Maybe Types.String)
adPath = Lens.field @"path"
{-# DEPRECATED adPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSizeInGb :: Lens.Lens' AttachedDisk (Core.Maybe Core.Int)
adSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED adSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Core.FromJSON AttachedDisk where
  parseJSON =
    Core.withObject "AttachedDisk" Core.$
      \x ->
        AttachedDisk'
          Core.<$> (x Core..:? "path") Core.<*> (x Core..:? "sizeInGb")

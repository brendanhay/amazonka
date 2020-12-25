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
    divdId,
    divdSize,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a disk image volume.
--
-- /See:/ 'mkDiskImageVolumeDescription' smart constructor.
data DiskImageVolumeDescription = DiskImageVolumeDescription'
  { -- | The volume identifier.
    id :: Core.Maybe Types.String,
    -- | The size of the volume, in GiB.
    size :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskImageVolumeDescription' value with any optional fields omitted.
mkDiskImageVolumeDescription ::
  DiskImageVolumeDescription
mkDiskImageVolumeDescription =
  DiskImageVolumeDescription'
    { id = Core.Nothing,
      size = Core.Nothing
    }

-- | The volume identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divdId :: Lens.Lens' DiskImageVolumeDescription (Core.Maybe Types.String)
divdId = Lens.field @"id"
{-# DEPRECATED divdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divdSize :: Lens.Lens' DiskImageVolumeDescription (Core.Maybe Core.Integer)
divdSize = Lens.field @"size"
{-# DEPRECATED divdSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Core.FromXML DiskImageVolumeDescription where
  parseXML x =
    DiskImageVolumeDescription'
      Core.<$> (x Core..@? "id") Core.<*> (x Core..@? "size")

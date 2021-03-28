{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskImage
  ( DiskImage (..)
  -- * Smart constructor
  , mkDiskImage
  -- * Lenses
  , diDescription
  , diImage
  , diVolume
  ) where

import qualified Network.AWS.EC2.Types.DiskImageDetail as Types
import qualified Network.AWS.EC2.Types.VolumeDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a disk image.
--
-- /See:/ 'mkDiskImage' smart constructor.
data DiskImage = DiskImage'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the disk image.
  , image :: Core.Maybe Types.DiskImageDetail
    -- ^ Information about the disk image.
  , volume :: Core.Maybe Types.VolumeDetail
    -- ^ Information about the volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskImage' value with any optional fields omitted.
mkDiskImage
    :: DiskImage
mkDiskImage
  = DiskImage'{description = Core.Nothing, image = Core.Nothing,
               volume = Core.Nothing}

-- | A description of the disk image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DiskImage (Core.Maybe Core.Text)
diDescription = Lens.field @"description"
{-# INLINEABLE diDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the disk image.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImage :: Lens.Lens' DiskImage (Core.Maybe Types.DiskImageDetail)
diImage = Lens.field @"image"
{-# INLINEABLE diImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | Information about the volume.
--
-- /Note:/ Consider using 'volume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diVolume :: Lens.Lens' DiskImage (Core.Maybe Types.VolumeDetail)
diVolume = Lens.field @"volume"
{-# INLINEABLE diVolume #-}
{-# DEPRECATED volume "Use generic-lens or generic-optics with 'volume' instead"  #-}

instance Core.ToQuery DiskImage where
        toQuery DiskImage{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Description")
              description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Image") image
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Volume") volume

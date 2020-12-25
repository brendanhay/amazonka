{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImageInserter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImageInserter
  ( ImageInserter (..),

    -- * Smart constructor
    mkImageInserter,

    -- * Lenses
    iiInsertableImages,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.InsertableImage as Types
import qualified Network.AWS.Prelude as Core

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input or output individually. This setting is disabled by default.
--
-- /See:/ 'mkImageInserter' smart constructor.
newtype ImageInserter = ImageInserter'
  { -- | Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
    insertableImages :: Core.Maybe [Types.InsertableImage]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImageInserter' value with any optional fields omitted.
mkImageInserter ::
  ImageInserter
mkImageInserter = ImageInserter' {insertableImages = Core.Nothing}

-- | Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
--
-- /Note:/ Consider using 'insertableImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInsertableImages :: Lens.Lens' ImageInserter (Core.Maybe [Types.InsertableImage])
iiInsertableImages = Lens.field @"insertableImages"
{-# DEPRECATED iiInsertableImages "Use generic-lens or generic-optics with 'insertableImages' instead." #-}

instance Core.FromJSON ImageInserter where
  toJSON ImageInserter {..} =
    Core.object
      ( Core.catMaybes
          [("insertableImages" Core..=) Core.<$> insertableImages]
      )

instance Core.FromJSON ImageInserter where
  parseJSON =
    Core.withObject "ImageInserter" Core.$
      \x -> ImageInserter' Core.<$> (x Core..:? "insertableImages")

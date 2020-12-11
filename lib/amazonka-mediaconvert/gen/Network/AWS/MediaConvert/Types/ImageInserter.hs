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
import Network.AWS.MediaConvert.Types.InsertableImage
import qualified Network.AWS.Prelude as Lude

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input or output individually. This setting is disabled by default.
--
-- /See:/ 'mkImageInserter' smart constructor.
newtype ImageInserter = ImageInserter'
  { insertableImages ::
      Lude.Maybe [InsertableImage]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageInserter' with the minimum fields required to make a request.
--
-- * 'insertableImages' - Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
mkImageInserter ::
  ImageInserter
mkImageInserter = ImageInserter' {insertableImages = Lude.Nothing}

-- | Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
--
-- /Note:/ Consider using 'insertableImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInsertableImages :: Lens.Lens' ImageInserter (Lude.Maybe [InsertableImage])
iiInsertableImages = Lens.lens (insertableImages :: ImageInserter -> Lude.Maybe [InsertableImage]) (\s a -> s {insertableImages = a} :: ImageInserter)
{-# DEPRECATED iiInsertableImages "Use generic-lens or generic-optics with 'insertableImages' instead." #-}

instance Lude.FromJSON ImageInserter where
  parseJSON =
    Lude.withObject
      "ImageInserter"
      ( \x ->
          ImageInserter'
            Lude.<$> (x Lude..:? "insertableImages" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ImageInserter where
  toJSON ImageInserter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("insertableImages" Lude..=) Lude.<$> insertableImages]
      )

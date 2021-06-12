{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImageInserter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImageInserter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.InsertableImage

-- | Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input or output
-- individually. This setting is disabled by default.
--
-- /See:/ 'newImageInserter' smart constructor.
data ImageInserter = ImageInserter'
  { -- | Specify the images that you want to overlay on your video. The images
    -- must be PNG or TGA files.
    insertableImages :: Core.Maybe [InsertableImage]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageInserter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insertableImages', 'imageInserter_insertableImages' - Specify the images that you want to overlay on your video. The images
-- must be PNG or TGA files.
newImageInserter ::
  ImageInserter
newImageInserter =
  ImageInserter' {insertableImages = Core.Nothing}

-- | Specify the images that you want to overlay on your video. The images
-- must be PNG or TGA files.
imageInserter_insertableImages :: Lens.Lens' ImageInserter (Core.Maybe [InsertableImage])
imageInserter_insertableImages = Lens.lens (\ImageInserter' {insertableImages} -> insertableImages) (\s@ImageInserter' {} a -> s {insertableImages = a} :: ImageInserter) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ImageInserter where
  parseJSON =
    Core.withObject
      "ImageInserter"
      ( \x ->
          ImageInserter'
            Core.<$> (x Core..:? "insertableImages" Core..!= Core.mempty)
      )

instance Core.Hashable ImageInserter

instance Core.NFData ImageInserter

instance Core.ToJSON ImageInserter where
  toJSON ImageInserter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("insertableImages" Core..=)
              Core.<$> insertableImages
          ]
      )

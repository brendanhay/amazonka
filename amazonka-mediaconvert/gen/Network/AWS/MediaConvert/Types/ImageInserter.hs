{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.InsertableImage
import qualified Network.AWS.Prelude as Prelude

-- | Enable the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input or output
-- individually. This setting is disabled by default.
--
-- /See:/ 'newImageInserter' smart constructor.
data ImageInserter = ImageInserter'
  { -- | Specify the images that you want to overlay on your video. The images
    -- must be PNG or TGA files.
    insertableImages :: Prelude.Maybe [InsertableImage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ImageInserter' {insertableImages = Prelude.Nothing}

-- | Specify the images that you want to overlay on your video. The images
-- must be PNG or TGA files.
imageInserter_insertableImages :: Lens.Lens' ImageInserter (Prelude.Maybe [InsertableImage])
imageInserter_insertableImages = Lens.lens (\ImageInserter' {insertableImages} -> insertableImages) (\s@ImageInserter' {} a -> s {insertableImages = a} :: ImageInserter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ImageInserter where
  parseJSON =
    Prelude.withObject
      "ImageInserter"
      ( \x ->
          ImageInserter'
            Prelude.<$> ( x Prelude..:? "insertableImages"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ImageInserter

instance Prelude.NFData ImageInserter

instance Prelude.ToJSON ImageInserter where
  toJSON ImageInserter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("insertableImages" Prelude..=)
              Prelude.<$> insertableImages
          ]
      )

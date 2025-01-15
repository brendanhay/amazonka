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
-- Module      : Amazonka.MediaConvert.Types.ImageInserter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ImageInserter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.InsertableImage
import qualified Amazonka.Prelude as Prelude

-- | Use the image inserter feature to include a graphic overlay on your
-- video. Enable or disable this feature for each input or output
-- individually. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/graphic-overlay.html.
-- This setting is disabled by default.
--
-- /See:/ 'newImageInserter' smart constructor.
data ImageInserter = ImageInserter'
  { -- | Specify the images that you want to overlay on your video. The images
    -- must be PNG or TGA files.
    insertableImages :: Prelude.Maybe [InsertableImage],
    -- | Specify the reference white level, in nits, for all of your image
    -- inserter images. Use to correct brightness levels within HDR10 outputs.
    -- For 1,000 nit peak brightness displays, we recommend that you set SDR
    -- reference white level to 203 (according to ITU-R BT.2408). Leave blank
    -- to use the default value of 100, or specify an integer from 100 to 1000.
    sdrReferenceWhiteLevel :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- 'sdrReferenceWhiteLevel', 'imageInserter_sdrReferenceWhiteLevel' - Specify the reference white level, in nits, for all of your image
-- inserter images. Use to correct brightness levels within HDR10 outputs.
-- For 1,000 nit peak brightness displays, we recommend that you set SDR
-- reference white level to 203 (according to ITU-R BT.2408). Leave blank
-- to use the default value of 100, or specify an integer from 100 to 1000.
newImageInserter ::
  ImageInserter
newImageInserter =
  ImageInserter'
    { insertableImages = Prelude.Nothing,
      sdrReferenceWhiteLevel = Prelude.Nothing
    }

-- | Specify the images that you want to overlay on your video. The images
-- must be PNG or TGA files.
imageInserter_insertableImages :: Lens.Lens' ImageInserter (Prelude.Maybe [InsertableImage])
imageInserter_insertableImages = Lens.lens (\ImageInserter' {insertableImages} -> insertableImages) (\s@ImageInserter' {} a -> s {insertableImages = a} :: ImageInserter) Prelude.. Lens.mapping Lens.coerced

-- | Specify the reference white level, in nits, for all of your image
-- inserter images. Use to correct brightness levels within HDR10 outputs.
-- For 1,000 nit peak brightness displays, we recommend that you set SDR
-- reference white level to 203 (according to ITU-R BT.2408). Leave blank
-- to use the default value of 100, or specify an integer from 100 to 1000.
imageInserter_sdrReferenceWhiteLevel :: Lens.Lens' ImageInserter (Prelude.Maybe Prelude.Natural)
imageInserter_sdrReferenceWhiteLevel = Lens.lens (\ImageInserter' {sdrReferenceWhiteLevel} -> sdrReferenceWhiteLevel) (\s@ImageInserter' {} a -> s {sdrReferenceWhiteLevel = a} :: ImageInserter)

instance Data.FromJSON ImageInserter where
  parseJSON =
    Data.withObject
      "ImageInserter"
      ( \x ->
          ImageInserter'
            Prelude.<$> ( x
                            Data..:? "insertableImages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "sdrReferenceWhiteLevel")
      )

instance Prelude.Hashable ImageInserter where
  hashWithSalt _salt ImageInserter' {..} =
    _salt
      `Prelude.hashWithSalt` insertableImages
      `Prelude.hashWithSalt` sdrReferenceWhiteLevel

instance Prelude.NFData ImageInserter where
  rnf ImageInserter' {..} =
    Prelude.rnf insertableImages `Prelude.seq`
      Prelude.rnf sdrReferenceWhiteLevel

instance Data.ToJSON ImageInserter where
  toJSON ImageInserter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("insertableImages" Data..=)
              Prelude.<$> insertableImages,
            ("sdrReferenceWhiteLevel" Data..=)
              Prelude.<$> sdrReferenceWhiteLevel
          ]
      )

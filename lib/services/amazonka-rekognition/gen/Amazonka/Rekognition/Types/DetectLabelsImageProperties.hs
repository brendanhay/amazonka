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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsImageProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsImageProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DetectLabelsImageBackground
import Amazonka.Rekognition.Types.DetectLabelsImageForeground
import Amazonka.Rekognition.Types.DetectLabelsImageQuality
import Amazonka.Rekognition.Types.DominantColor

-- | Information about the quality and dominant colors of an input image.
-- Quality and color information is returned for the entire image,
-- foreground, and background.
--
-- /See:/ 'newDetectLabelsImageProperties' smart constructor.
data DetectLabelsImageProperties = DetectLabelsImageProperties'
  { -- | Information about the properties of an image’s foreground, including the
    -- foreground’s quality and dominant colors, including the quality and
    -- dominant colors of the image.
    foreground :: Prelude.Maybe DetectLabelsImageForeground,
    -- | Information about the quality of the image foreground as defined by
    -- brightness, sharpness, and contrast. The higher the value the greater
    -- the brightness, sharpness, and contrast respectively.
    quality :: Prelude.Maybe DetectLabelsImageQuality,
    -- | Information about the properties of an image’s background, including the
    -- background’s quality and dominant colors, including the quality and
    -- dominant colors of the image.
    background :: Prelude.Maybe DetectLabelsImageBackground,
    -- | Information about the dominant colors found in an image, described with
    -- RGB values, CSS color name, simplified color name, and PixelPercentage
    -- (the percentage of image pixels that have a particular color).
    dominantColors :: Prelude.Maybe [DominantColor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsImageProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'foreground', 'detectLabelsImageProperties_foreground' - Information about the properties of an image’s foreground, including the
-- foreground’s quality and dominant colors, including the quality and
-- dominant colors of the image.
--
-- 'quality', 'detectLabelsImageProperties_quality' - Information about the quality of the image foreground as defined by
-- brightness, sharpness, and contrast. The higher the value the greater
-- the brightness, sharpness, and contrast respectively.
--
-- 'background', 'detectLabelsImageProperties_background' - Information about the properties of an image’s background, including the
-- background’s quality and dominant colors, including the quality and
-- dominant colors of the image.
--
-- 'dominantColors', 'detectLabelsImageProperties_dominantColors' - Information about the dominant colors found in an image, described with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
newDetectLabelsImageProperties ::
  DetectLabelsImageProperties
newDetectLabelsImageProperties =
  DetectLabelsImageProperties'
    { foreground =
        Prelude.Nothing,
      quality = Prelude.Nothing,
      background = Prelude.Nothing,
      dominantColors = Prelude.Nothing
    }

-- | Information about the properties of an image’s foreground, including the
-- foreground’s quality and dominant colors, including the quality and
-- dominant colors of the image.
detectLabelsImageProperties_foreground :: Lens.Lens' DetectLabelsImageProperties (Prelude.Maybe DetectLabelsImageForeground)
detectLabelsImageProperties_foreground = Lens.lens (\DetectLabelsImageProperties' {foreground} -> foreground) (\s@DetectLabelsImageProperties' {} a -> s {foreground = a} :: DetectLabelsImageProperties)

-- | Information about the quality of the image foreground as defined by
-- brightness, sharpness, and contrast. The higher the value the greater
-- the brightness, sharpness, and contrast respectively.
detectLabelsImageProperties_quality :: Lens.Lens' DetectLabelsImageProperties (Prelude.Maybe DetectLabelsImageQuality)
detectLabelsImageProperties_quality = Lens.lens (\DetectLabelsImageProperties' {quality} -> quality) (\s@DetectLabelsImageProperties' {} a -> s {quality = a} :: DetectLabelsImageProperties)

-- | Information about the properties of an image’s background, including the
-- background’s quality and dominant colors, including the quality and
-- dominant colors of the image.
detectLabelsImageProperties_background :: Lens.Lens' DetectLabelsImageProperties (Prelude.Maybe DetectLabelsImageBackground)
detectLabelsImageProperties_background = Lens.lens (\DetectLabelsImageProperties' {background} -> background) (\s@DetectLabelsImageProperties' {} a -> s {background = a} :: DetectLabelsImageProperties)

-- | Information about the dominant colors found in an image, described with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
detectLabelsImageProperties_dominantColors :: Lens.Lens' DetectLabelsImageProperties (Prelude.Maybe [DominantColor])
detectLabelsImageProperties_dominantColors = Lens.lens (\DetectLabelsImageProperties' {dominantColors} -> dominantColors) (\s@DetectLabelsImageProperties' {} a -> s {dominantColors = a} :: DetectLabelsImageProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DetectLabelsImageProperties where
  parseJSON =
    Data.withObject
      "DetectLabelsImageProperties"
      ( \x ->
          DetectLabelsImageProperties'
            Prelude.<$> (x Data..:? "Foreground")
            Prelude.<*> (x Data..:? "Quality")
            Prelude.<*> (x Data..:? "Background")
            Prelude.<*> ( x Data..:? "DominantColors"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DetectLabelsImageProperties where
  hashWithSalt _salt DetectLabelsImageProperties' {..} =
    _salt `Prelude.hashWithSalt` foreground
      `Prelude.hashWithSalt` quality
      `Prelude.hashWithSalt` background
      `Prelude.hashWithSalt` dominantColors

instance Prelude.NFData DetectLabelsImageProperties where
  rnf DetectLabelsImageProperties' {..} =
    Prelude.rnf foreground
      `Prelude.seq` Prelude.rnf quality
      `Prelude.seq` Prelude.rnf background
      `Prelude.seq` Prelude.rnf dominantColors

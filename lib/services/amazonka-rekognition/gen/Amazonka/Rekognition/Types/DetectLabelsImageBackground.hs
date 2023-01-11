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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsImageBackground
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsImageBackground where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DetectLabelsImageQuality
import Amazonka.Rekognition.Types.DominantColor

-- | The background of the image with regard to image quality and dominant
-- colors.
--
-- /See:/ 'newDetectLabelsImageBackground' smart constructor.
data DetectLabelsImageBackground = DetectLabelsImageBackground'
  { -- | The dominant colors found in the background of an image, defined with
    -- RGB values, CSS color name, simplified color name, and PixelPercentage
    -- (the percentage of image pixels that have a particular color).
    dominantColors :: Prelude.Maybe [DominantColor],
    -- | The quality of the image background as defined by brightness and
    -- sharpness.
    quality :: Prelude.Maybe DetectLabelsImageQuality
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsImageBackground' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dominantColors', 'detectLabelsImageBackground_dominantColors' - The dominant colors found in the background of an image, defined with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
--
-- 'quality', 'detectLabelsImageBackground_quality' - The quality of the image background as defined by brightness and
-- sharpness.
newDetectLabelsImageBackground ::
  DetectLabelsImageBackground
newDetectLabelsImageBackground =
  DetectLabelsImageBackground'
    { dominantColors =
        Prelude.Nothing,
      quality = Prelude.Nothing
    }

-- | The dominant colors found in the background of an image, defined with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
detectLabelsImageBackground_dominantColors :: Lens.Lens' DetectLabelsImageBackground (Prelude.Maybe [DominantColor])
detectLabelsImageBackground_dominantColors = Lens.lens (\DetectLabelsImageBackground' {dominantColors} -> dominantColors) (\s@DetectLabelsImageBackground' {} a -> s {dominantColors = a} :: DetectLabelsImageBackground) Prelude.. Lens.mapping Lens.coerced

-- | The quality of the image background as defined by brightness and
-- sharpness.
detectLabelsImageBackground_quality :: Lens.Lens' DetectLabelsImageBackground (Prelude.Maybe DetectLabelsImageQuality)
detectLabelsImageBackground_quality = Lens.lens (\DetectLabelsImageBackground' {quality} -> quality) (\s@DetectLabelsImageBackground' {} a -> s {quality = a} :: DetectLabelsImageBackground)

instance Data.FromJSON DetectLabelsImageBackground where
  parseJSON =
    Data.withObject
      "DetectLabelsImageBackground"
      ( \x ->
          DetectLabelsImageBackground'
            Prelude.<$> (x Data..:? "DominantColors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Quality")
      )

instance Prelude.Hashable DetectLabelsImageBackground where
  hashWithSalt _salt DetectLabelsImageBackground' {..} =
    _salt `Prelude.hashWithSalt` dominantColors
      `Prelude.hashWithSalt` quality

instance Prelude.NFData DetectLabelsImageBackground where
  rnf DetectLabelsImageBackground' {..} =
    Prelude.rnf dominantColors
      `Prelude.seq` Prelude.rnf quality

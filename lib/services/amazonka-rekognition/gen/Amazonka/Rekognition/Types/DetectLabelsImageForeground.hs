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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsImageForeground
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsImageForeground where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DetectLabelsImageQuality
import Amazonka.Rekognition.Types.DominantColor

-- | The foreground of the image with regard to image quality and dominant
-- colors.
--
-- /See:/ 'newDetectLabelsImageForeground' smart constructor.
data DetectLabelsImageForeground = DetectLabelsImageForeground'
  { -- | The dominant colors found in the foreground of an image, defined with
    -- RGB values, CSS color name, simplified color name, and PixelPercentage
    -- (the percentage of image pixels that have a particular color).
    dominantColors :: Prelude.Maybe [DominantColor],
    -- | The quality of the image foreground as defined by brightness and
    -- sharpness.
    quality :: Prelude.Maybe DetectLabelsImageQuality
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsImageForeground' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dominantColors', 'detectLabelsImageForeground_dominantColors' - The dominant colors found in the foreground of an image, defined with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
--
-- 'quality', 'detectLabelsImageForeground_quality' - The quality of the image foreground as defined by brightness and
-- sharpness.
newDetectLabelsImageForeground ::
  DetectLabelsImageForeground
newDetectLabelsImageForeground =
  DetectLabelsImageForeground'
    { dominantColors =
        Prelude.Nothing,
      quality = Prelude.Nothing
    }

-- | The dominant colors found in the foreground of an image, defined with
-- RGB values, CSS color name, simplified color name, and PixelPercentage
-- (the percentage of image pixels that have a particular color).
detectLabelsImageForeground_dominantColors :: Lens.Lens' DetectLabelsImageForeground (Prelude.Maybe [DominantColor])
detectLabelsImageForeground_dominantColors = Lens.lens (\DetectLabelsImageForeground' {dominantColors} -> dominantColors) (\s@DetectLabelsImageForeground' {} a -> s {dominantColors = a} :: DetectLabelsImageForeground) Prelude.. Lens.mapping Lens.coerced

-- | The quality of the image foreground as defined by brightness and
-- sharpness.
detectLabelsImageForeground_quality :: Lens.Lens' DetectLabelsImageForeground (Prelude.Maybe DetectLabelsImageQuality)
detectLabelsImageForeground_quality = Lens.lens (\DetectLabelsImageForeground' {quality} -> quality) (\s@DetectLabelsImageForeground' {} a -> s {quality = a} :: DetectLabelsImageForeground)

instance Data.FromJSON DetectLabelsImageForeground where
  parseJSON =
    Data.withObject
      "DetectLabelsImageForeground"
      ( \x ->
          DetectLabelsImageForeground'
            Prelude.<$> (x Data..:? "DominantColors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Quality")
      )

instance Prelude.Hashable DetectLabelsImageForeground where
  hashWithSalt _salt DetectLabelsImageForeground' {..} =
    _salt `Prelude.hashWithSalt` dominantColors
      `Prelude.hashWithSalt` quality

instance Prelude.NFData DetectLabelsImageForeground where
  rnf DetectLabelsImageForeground' {..} =
    Prelude.rnf dominantColors
      `Prelude.seq` Prelude.rnf quality

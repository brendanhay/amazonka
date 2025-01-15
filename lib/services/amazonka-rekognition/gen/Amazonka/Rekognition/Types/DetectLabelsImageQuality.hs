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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsImageQuality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsImageQuality where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The quality of an image provided for label detection, with regard to
-- brightness, sharpness, and contrast.
--
-- /See:/ 'newDetectLabelsImageQuality' smart constructor.
data DetectLabelsImageQuality = DetectLabelsImageQuality'
  { -- | The brightness of an image provided for label detection.
    brightness :: Prelude.Maybe Prelude.Double,
    -- | The contrast of an image provided for label detection.
    contrast :: Prelude.Maybe Prelude.Double,
    -- | The sharpness of an image provided for label detection.
    sharpness :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsImageQuality' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brightness', 'detectLabelsImageQuality_brightness' - The brightness of an image provided for label detection.
--
-- 'contrast', 'detectLabelsImageQuality_contrast' - The contrast of an image provided for label detection.
--
-- 'sharpness', 'detectLabelsImageQuality_sharpness' - The sharpness of an image provided for label detection.
newDetectLabelsImageQuality ::
  DetectLabelsImageQuality
newDetectLabelsImageQuality =
  DetectLabelsImageQuality'
    { brightness =
        Prelude.Nothing,
      contrast = Prelude.Nothing,
      sharpness = Prelude.Nothing
    }

-- | The brightness of an image provided for label detection.
detectLabelsImageQuality_brightness :: Lens.Lens' DetectLabelsImageQuality (Prelude.Maybe Prelude.Double)
detectLabelsImageQuality_brightness = Lens.lens (\DetectLabelsImageQuality' {brightness} -> brightness) (\s@DetectLabelsImageQuality' {} a -> s {brightness = a} :: DetectLabelsImageQuality)

-- | The contrast of an image provided for label detection.
detectLabelsImageQuality_contrast :: Lens.Lens' DetectLabelsImageQuality (Prelude.Maybe Prelude.Double)
detectLabelsImageQuality_contrast = Lens.lens (\DetectLabelsImageQuality' {contrast} -> contrast) (\s@DetectLabelsImageQuality' {} a -> s {contrast = a} :: DetectLabelsImageQuality)

-- | The sharpness of an image provided for label detection.
detectLabelsImageQuality_sharpness :: Lens.Lens' DetectLabelsImageQuality (Prelude.Maybe Prelude.Double)
detectLabelsImageQuality_sharpness = Lens.lens (\DetectLabelsImageQuality' {sharpness} -> sharpness) (\s@DetectLabelsImageQuality' {} a -> s {sharpness = a} :: DetectLabelsImageQuality)

instance Data.FromJSON DetectLabelsImageQuality where
  parseJSON =
    Data.withObject
      "DetectLabelsImageQuality"
      ( \x ->
          DetectLabelsImageQuality'
            Prelude.<$> (x Data..:? "Brightness")
            Prelude.<*> (x Data..:? "Contrast")
            Prelude.<*> (x Data..:? "Sharpness")
      )

instance Prelude.Hashable DetectLabelsImageQuality where
  hashWithSalt _salt DetectLabelsImageQuality' {..} =
    _salt
      `Prelude.hashWithSalt` brightness
      `Prelude.hashWithSalt` contrast
      `Prelude.hashWithSalt` sharpness

instance Prelude.NFData DetectLabelsImageQuality where
  rnf DetectLabelsImageQuality' {..} =
    Prelude.rnf brightness `Prelude.seq`
      Prelude.rnf contrast `Prelude.seq`
        Prelude.rnf sharpness

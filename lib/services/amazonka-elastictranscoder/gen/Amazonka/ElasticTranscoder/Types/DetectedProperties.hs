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
-- Module      : Amazonka.ElasticTranscoder.Types.DetectedProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.DetectedProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The detected properties of the input file. Elastic Transcoder identifies
-- these values from the input file.
--
-- /See:/ 'newDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { -- | The detected file size of the input file, in bytes.
    fileSize :: Prelude.Maybe Prelude.Integer,
    -- | The detected width of the input file, in pixels.
    width :: Prelude.Maybe Prelude.Int,
    -- | The detected duration of the input file, in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The detected height of the input file, in pixels.
    height :: Prelude.Maybe Prelude.Int,
    -- | The detected frame rate of the input file, in frames per second.
    frameRate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSize', 'detectedProperties_fileSize' - The detected file size of the input file, in bytes.
--
-- 'width', 'detectedProperties_width' - The detected width of the input file, in pixels.
--
-- 'durationMillis', 'detectedProperties_durationMillis' - The detected duration of the input file, in milliseconds.
--
-- 'height', 'detectedProperties_height' - The detected height of the input file, in pixels.
--
-- 'frameRate', 'detectedProperties_frameRate' - The detected frame rate of the input file, in frames per second.
newDetectedProperties ::
  DetectedProperties
newDetectedProperties =
  DetectedProperties'
    { fileSize = Prelude.Nothing,
      width = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      height = Prelude.Nothing,
      frameRate = Prelude.Nothing
    }

-- | The detected file size of the input file, in bytes.
detectedProperties_fileSize :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_fileSize = Lens.lens (\DetectedProperties' {fileSize} -> fileSize) (\s@DetectedProperties' {} a -> s {fileSize = a} :: DetectedProperties)

-- | The detected width of the input file, in pixels.
detectedProperties_width :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_width = Lens.lens (\DetectedProperties' {width} -> width) (\s@DetectedProperties' {} a -> s {width = a} :: DetectedProperties)

-- | The detected duration of the input file, in milliseconds.
detectedProperties_durationMillis :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_durationMillis = Lens.lens (\DetectedProperties' {durationMillis} -> durationMillis) (\s@DetectedProperties' {} a -> s {durationMillis = a} :: DetectedProperties)

-- | The detected height of the input file, in pixels.
detectedProperties_height :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_height = Lens.lens (\DetectedProperties' {height} -> height) (\s@DetectedProperties' {} a -> s {height = a} :: DetectedProperties)

-- | The detected frame rate of the input file, in frames per second.
detectedProperties_frameRate :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Text)
detectedProperties_frameRate = Lens.lens (\DetectedProperties' {frameRate} -> frameRate) (\s@DetectedProperties' {} a -> s {frameRate = a} :: DetectedProperties)

instance Core.FromJSON DetectedProperties where
  parseJSON =
    Core.withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            Prelude.<$> (x Core..:? "FileSize")
            Prelude.<*> (x Core..:? "Width")
            Prelude.<*> (x Core..:? "DurationMillis")
            Prelude.<*> (x Core..:? "Height")
            Prelude.<*> (x Core..:? "FrameRate")
      )

instance Prelude.Hashable DetectedProperties where
  hashWithSalt _salt DetectedProperties' {..} =
    _salt `Prelude.hashWithSalt` fileSize
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` frameRate

instance Prelude.NFData DetectedProperties where
  rnf DetectedProperties' {..} =
    Prelude.rnf fileSize
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf frameRate

instance Core.ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FileSize" Core..=) Prelude.<$> fileSize,
            ("Width" Core..=) Prelude.<$> width,
            ("DurationMillis" Core..=)
              Prelude.<$> durationMillis,
            ("Height" Core..=) Prelude.<$> height,
            ("FrameRate" Core..=) Prelude.<$> frameRate
          ]
      )

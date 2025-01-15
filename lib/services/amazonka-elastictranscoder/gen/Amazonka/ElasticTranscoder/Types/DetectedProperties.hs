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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.DetectedProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detected properties of the input file. Elastic Transcoder identifies
-- these values from the input file.
--
-- /See:/ 'newDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { -- | The detected duration of the input file, in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The detected file size of the input file, in bytes.
    fileSize :: Prelude.Maybe Prelude.Integer,
    -- | The detected frame rate of the input file, in frames per second.
    frameRate :: Prelude.Maybe Prelude.Text,
    -- | The detected height of the input file, in pixels.
    height :: Prelude.Maybe Prelude.Int,
    -- | The detected width of the input file, in pixels.
    width :: Prelude.Maybe Prelude.Int
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
-- 'durationMillis', 'detectedProperties_durationMillis' - The detected duration of the input file, in milliseconds.
--
-- 'fileSize', 'detectedProperties_fileSize' - The detected file size of the input file, in bytes.
--
-- 'frameRate', 'detectedProperties_frameRate' - The detected frame rate of the input file, in frames per second.
--
-- 'height', 'detectedProperties_height' - The detected height of the input file, in pixels.
--
-- 'width', 'detectedProperties_width' - The detected width of the input file, in pixels.
newDetectedProperties ::
  DetectedProperties
newDetectedProperties =
  DetectedProperties'
    { durationMillis =
        Prelude.Nothing,
      fileSize = Prelude.Nothing,
      frameRate = Prelude.Nothing,
      height = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | The detected duration of the input file, in milliseconds.
detectedProperties_durationMillis :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_durationMillis = Lens.lens (\DetectedProperties' {durationMillis} -> durationMillis) (\s@DetectedProperties' {} a -> s {durationMillis = a} :: DetectedProperties)

-- | The detected file size of the input file, in bytes.
detectedProperties_fileSize :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_fileSize = Lens.lens (\DetectedProperties' {fileSize} -> fileSize) (\s@DetectedProperties' {} a -> s {fileSize = a} :: DetectedProperties)

-- | The detected frame rate of the input file, in frames per second.
detectedProperties_frameRate :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Text)
detectedProperties_frameRate = Lens.lens (\DetectedProperties' {frameRate} -> frameRate) (\s@DetectedProperties' {} a -> s {frameRate = a} :: DetectedProperties)

-- | The detected height of the input file, in pixels.
detectedProperties_height :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_height = Lens.lens (\DetectedProperties' {height} -> height) (\s@DetectedProperties' {} a -> s {height = a} :: DetectedProperties)

-- | The detected width of the input file, in pixels.
detectedProperties_width :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_width = Lens.lens (\DetectedProperties' {width} -> width) (\s@DetectedProperties' {} a -> s {width = a} :: DetectedProperties)

instance Data.FromJSON DetectedProperties where
  parseJSON =
    Data.withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            Prelude.<$> (x Data..:? "DurationMillis")
            Prelude.<*> (x Data..:? "FileSize")
            Prelude.<*> (x Data..:? "FrameRate")
            Prelude.<*> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "Width")
      )

instance Prelude.Hashable DetectedProperties where
  hashWithSalt _salt DetectedProperties' {..} =
    _salt
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` fileSize
      `Prelude.hashWithSalt` frameRate
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` width

instance Prelude.NFData DetectedProperties where
  rnf DetectedProperties' {..} =
    Prelude.rnf durationMillis `Prelude.seq`
      Prelude.rnf fileSize `Prelude.seq`
        Prelude.rnf frameRate `Prelude.seq`
          Prelude.rnf height `Prelude.seq`
            Prelude.rnf width

instance Data.ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationMillis" Data..=)
              Prelude.<$> durationMillis,
            ("FileSize" Data..=) Prelude.<$> fileSize,
            ("FrameRate" Data..=) Prelude.<$> frameRate,
            ("Height" Data..=) Prelude.<$> height,
            ("Width" Data..=) Prelude.<$> width
          ]
      )

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
-- Module      : Network.AWS.ElasticTranscoder.Types.DetectedProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.DetectedProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detected properties of the input file. Elastic Transcoder identifies
-- these values from the input file.
--
-- /See:/ 'newDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { -- | The detected height of the input file, in pixels.
    height :: Prelude.Maybe Prelude.Int,
    -- | The detected width of the input file, in pixels.
    width :: Prelude.Maybe Prelude.Int,
    -- | The detected file size of the input file, in bytes.
    fileSize :: Prelude.Maybe Prelude.Integer,
    -- | The detected frame rate of the input file, in frames per second.
    frameRate :: Prelude.Maybe Prelude.Text,
    -- | The detected duration of the input file, in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectedProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'detectedProperties_height' - The detected height of the input file, in pixels.
--
-- 'width', 'detectedProperties_width' - The detected width of the input file, in pixels.
--
-- 'fileSize', 'detectedProperties_fileSize' - The detected file size of the input file, in bytes.
--
-- 'frameRate', 'detectedProperties_frameRate' - The detected frame rate of the input file, in frames per second.
--
-- 'durationMillis', 'detectedProperties_durationMillis' - The detected duration of the input file, in milliseconds.
newDetectedProperties ::
  DetectedProperties
newDetectedProperties =
  DetectedProperties'
    { height = Prelude.Nothing,
      width = Prelude.Nothing,
      fileSize = Prelude.Nothing,
      frameRate = Prelude.Nothing,
      durationMillis = Prelude.Nothing
    }

-- | The detected height of the input file, in pixels.
detectedProperties_height :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_height = Lens.lens (\DetectedProperties' {height} -> height) (\s@DetectedProperties' {} a -> s {height = a} :: DetectedProperties)

-- | The detected width of the input file, in pixels.
detectedProperties_width :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Int)
detectedProperties_width = Lens.lens (\DetectedProperties' {width} -> width) (\s@DetectedProperties' {} a -> s {width = a} :: DetectedProperties)

-- | The detected file size of the input file, in bytes.
detectedProperties_fileSize :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_fileSize = Lens.lens (\DetectedProperties' {fileSize} -> fileSize) (\s@DetectedProperties' {} a -> s {fileSize = a} :: DetectedProperties)

-- | The detected frame rate of the input file, in frames per second.
detectedProperties_frameRate :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Text)
detectedProperties_frameRate = Lens.lens (\DetectedProperties' {frameRate} -> frameRate) (\s@DetectedProperties' {} a -> s {frameRate = a} :: DetectedProperties)

-- | The detected duration of the input file, in milliseconds.
detectedProperties_durationMillis :: Lens.Lens' DetectedProperties (Prelude.Maybe Prelude.Integer)
detectedProperties_durationMillis = Lens.lens (\DetectedProperties' {durationMillis} -> durationMillis) (\s@DetectedProperties' {} a -> s {durationMillis = a} :: DetectedProperties)

instance Prelude.FromJSON DetectedProperties where
  parseJSON =
    Prelude.withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            Prelude.<$> (x Prelude..:? "Height")
            Prelude.<*> (x Prelude..:? "Width")
            Prelude.<*> (x Prelude..:? "FileSize")
            Prelude.<*> (x Prelude..:? "FrameRate")
            Prelude.<*> (x Prelude..:? "DurationMillis")
      )

instance Prelude.Hashable DetectedProperties

instance Prelude.NFData DetectedProperties

instance Prelude.ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Height" Prelude..=) Prelude.<$> height,
            ("Width" Prelude..=) Prelude.<$> width,
            ("FileSize" Prelude..=) Prelude.<$> fileSize,
            ("FrameRate" Prelude..=) Prelude.<$> frameRate,
            ("DurationMillis" Prelude..=)
              Prelude.<$> durationMillis
          ]
      )

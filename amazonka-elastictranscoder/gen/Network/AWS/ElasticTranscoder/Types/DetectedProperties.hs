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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The detected properties of the input file. Elastic Transcoder identifies
-- these values from the input file.
--
-- /See:/ 'newDetectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { -- | The detected height of the input file, in pixels.
    height :: Core.Maybe Core.Int,
    -- | The detected width of the input file, in pixels.
    width :: Core.Maybe Core.Int,
    -- | The detected file size of the input file, in bytes.
    fileSize :: Core.Maybe Core.Integer,
    -- | The detected frame rate of the input file, in frames per second.
    frameRate :: Core.Maybe Core.Text,
    -- | The detected duration of the input file, in milliseconds.
    durationMillis :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { height = Core.Nothing,
      width = Core.Nothing,
      fileSize = Core.Nothing,
      frameRate = Core.Nothing,
      durationMillis = Core.Nothing
    }

-- | The detected height of the input file, in pixels.
detectedProperties_height :: Lens.Lens' DetectedProperties (Core.Maybe Core.Int)
detectedProperties_height = Lens.lens (\DetectedProperties' {height} -> height) (\s@DetectedProperties' {} a -> s {height = a} :: DetectedProperties)

-- | The detected width of the input file, in pixels.
detectedProperties_width :: Lens.Lens' DetectedProperties (Core.Maybe Core.Int)
detectedProperties_width = Lens.lens (\DetectedProperties' {width} -> width) (\s@DetectedProperties' {} a -> s {width = a} :: DetectedProperties)

-- | The detected file size of the input file, in bytes.
detectedProperties_fileSize :: Lens.Lens' DetectedProperties (Core.Maybe Core.Integer)
detectedProperties_fileSize = Lens.lens (\DetectedProperties' {fileSize} -> fileSize) (\s@DetectedProperties' {} a -> s {fileSize = a} :: DetectedProperties)

-- | The detected frame rate of the input file, in frames per second.
detectedProperties_frameRate :: Lens.Lens' DetectedProperties (Core.Maybe Core.Text)
detectedProperties_frameRate = Lens.lens (\DetectedProperties' {frameRate} -> frameRate) (\s@DetectedProperties' {} a -> s {frameRate = a} :: DetectedProperties)

-- | The detected duration of the input file, in milliseconds.
detectedProperties_durationMillis :: Lens.Lens' DetectedProperties (Core.Maybe Core.Integer)
detectedProperties_durationMillis = Lens.lens (\DetectedProperties' {durationMillis} -> durationMillis) (\s@DetectedProperties' {} a -> s {durationMillis = a} :: DetectedProperties)

instance Core.FromJSON DetectedProperties where
  parseJSON =
    Core.withObject
      "DetectedProperties"
      ( \x ->
          DetectedProperties'
            Core.<$> (x Core..:? "Height")
            Core.<*> (x Core..:? "Width")
            Core.<*> (x Core..:? "FileSize")
            Core.<*> (x Core..:? "FrameRate")
            Core.<*> (x Core..:? "DurationMillis")
      )

instance Core.Hashable DetectedProperties

instance Core.NFData DetectedProperties

instance Core.ToJSON DetectedProperties where
  toJSON DetectedProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Height" Core..=) Core.<$> height,
            ("Width" Core..=) Core.<$> width,
            ("FileSize" Core..=) Core.<$> fileSize,
            ("FrameRate" Core..=) Core.<$> frameRate,
            ("DurationMillis" Core..=) Core.<$> durationMillis
          ]
      )

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
-- Module      : Network.AWS.MediaConvert.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FrameCaptureSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value FRAME_CAPTURE.
--
-- /See:/ 'newFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { -- | Frame capture will encode the first frame of the output stream, then one
    -- frame every framerateDenominator\/framerateNumerator seconds. For
    -- example, settings of framerateNumerator = 1 and framerateDenominator = 3
    -- (a rate of 1\/3 frame per second) will capture the first frame, then 1
    -- frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is
    -- the 0-based frame sequence number zero padded to 7 decimal places.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Maximum number of captures (encoded jpg output files).
    maxCaptures :: Prelude.Maybe Prelude.Natural,
    -- | Frame capture will encode the first frame of the output stream, then one
    -- frame every framerateDenominator\/framerateNumerator seconds. For
    -- example, settings of framerateNumerator = 1 and framerateDenominator = 3
    -- (a rate of 1\/3 frame per second) will capture the first frame, then 1
    -- frame every 3s. Files will be named as filename.n.jpg where n is the
    -- 0-based sequence number of each Capture.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | JPEG Quality - a higher value equals higher quality.
    quality :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framerateNumerator', 'frameCaptureSettings_framerateNumerator' - Frame capture will encode the first frame of the output stream, then one
-- frame every framerateDenominator\/framerateNumerator seconds. For
-- example, settings of framerateNumerator = 1 and framerateDenominator = 3
-- (a rate of 1\/3 frame per second) will capture the first frame, then 1
-- frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is
-- the 0-based frame sequence number zero padded to 7 decimal places.
--
-- 'maxCaptures', 'frameCaptureSettings_maxCaptures' - Maximum number of captures (encoded jpg output files).
--
-- 'framerateDenominator', 'frameCaptureSettings_framerateDenominator' - Frame capture will encode the first frame of the output stream, then one
-- frame every framerateDenominator\/framerateNumerator seconds. For
-- example, settings of framerateNumerator = 1 and framerateDenominator = 3
-- (a rate of 1\/3 frame per second) will capture the first frame, then 1
-- frame every 3s. Files will be named as filename.n.jpg where n is the
-- 0-based sequence number of each Capture.
--
-- 'quality', 'frameCaptureSettings_quality' - JPEG Quality - a higher value equals higher quality.
newFrameCaptureSettings ::
  FrameCaptureSettings
newFrameCaptureSettings =
  FrameCaptureSettings'
    { framerateNumerator =
        Prelude.Nothing,
      maxCaptures = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      quality = Prelude.Nothing
    }

-- | Frame capture will encode the first frame of the output stream, then one
-- frame every framerateDenominator\/framerateNumerator seconds. For
-- example, settings of framerateNumerator = 1 and framerateDenominator = 3
-- (a rate of 1\/3 frame per second) will capture the first frame, then 1
-- frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is
-- the 0-based frame sequence number zero padded to 7 decimal places.
frameCaptureSettings_framerateNumerator :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_framerateNumerator = Lens.lens (\FrameCaptureSettings' {framerateNumerator} -> framerateNumerator) (\s@FrameCaptureSettings' {} a -> s {framerateNumerator = a} :: FrameCaptureSettings)

-- | Maximum number of captures (encoded jpg output files).
frameCaptureSettings_maxCaptures :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_maxCaptures = Lens.lens (\FrameCaptureSettings' {maxCaptures} -> maxCaptures) (\s@FrameCaptureSettings' {} a -> s {maxCaptures = a} :: FrameCaptureSettings)

-- | Frame capture will encode the first frame of the output stream, then one
-- frame every framerateDenominator\/framerateNumerator seconds. For
-- example, settings of framerateNumerator = 1 and framerateDenominator = 3
-- (a rate of 1\/3 frame per second) will capture the first frame, then 1
-- frame every 3s. Files will be named as filename.n.jpg where n is the
-- 0-based sequence number of each Capture.
frameCaptureSettings_framerateDenominator :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_framerateDenominator = Lens.lens (\FrameCaptureSettings' {framerateDenominator} -> framerateDenominator) (\s@FrameCaptureSettings' {} a -> s {framerateDenominator = a} :: FrameCaptureSettings)

-- | JPEG Quality - a higher value equals higher quality.
frameCaptureSettings_quality :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_quality = Lens.lens (\FrameCaptureSettings' {quality} -> quality) (\s@FrameCaptureSettings' {} a -> s {quality = a} :: FrameCaptureSettings)

instance Core.FromJSON FrameCaptureSettings where
  parseJSON =
    Core.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Prelude.<$> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "maxCaptures")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "quality")
      )

instance Prelude.Hashable FrameCaptureSettings

instance Prelude.NFData FrameCaptureSettings

instance Core.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("maxCaptures" Core..=) Prelude.<$> maxCaptures,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("quality" Core..=) Prelude.<$> quality
          ]
      )

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
-- Module      : Amazonka.MediaConvert.Types.Vp9Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vp9Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.Vp9FramerateControl
import Amazonka.MediaConvert.Types.Vp9FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Vp9ParControl
import Amazonka.MediaConvert.Types.Vp9QualityTuningLevel
import Amazonka.MediaConvert.Types.Vp9RateControlMode
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP9.
--
-- /See:/ 'newVp9Settings' smart constructor.
data Vp9Settings = Vp9Settings'
  { -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five
    -- megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
    -- | Target bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | If you are using the console, use the Framerate setting to specify the
    -- frame rate for this output. If you want to keep the same frame rate as
    -- the input video, choose Follow source. If you want to do frame rate
    -- conversion, choose a frame rate from the dropdown list or choose Custom.
    -- The framerates shown in the dropdown list are decimal approximations of
    -- fractions. If you choose Custom, specify your frame rate as a fraction.
    -- If you are creating your transcoding job specification as a JSON file
    -- without the console, use FramerateControl to specify which value the
    -- service uses for the frame rate for this output. Choose
    -- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
    -- from the input. Choose SPECIFIED if you want the service to use the
    -- frame rate you specify in the settings FramerateNumerator and
    -- FramerateDenominator.
    framerateControl :: Prelude.Maybe Vp9FramerateControl,
    -- | Optional. Specify how the service determines the pixel aspect ratio for
    -- this output. The default behavior is to use the same pixel aspect ratio
    -- as your input video.
    parControl :: Prelude.Maybe Vp9ParControl,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, multi-pass encoding.
    qualityTuningLevel :: Prelude.Maybe Vp9QualityTuningLevel,
    -- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
    -- Optional. Specify the maximum bitrate in bits\/second. For example,
    -- enter five megabits per second as 5000000. The default behavior uses
    -- twice the target bitrate as the maximum bitrate.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parDenominator is 33.
    parDenominator :: Prelude.Maybe Prelude.Natural,
    -- | With the VP9 codec, you can use only the variable bitrate (VBR) rate
    -- control mode.
    rateControlMode :: Prelude.Maybe Vp9RateControlMode,
    -- | Choose the method that you want MediaConvert to use when increasing or
    -- decreasing the frame rate. We recommend using drop duplicate
    -- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
    -- 30 fps. For numerically complex conversions, you can use interpolate
    -- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
    -- might introduce undesirable video artifacts. For complex frame rate
    -- conversions, especially if your source video has already been converted
    -- from its original cadence, use FrameFormer (FRAMEFORMER) to do
    -- motion-compensated interpolation. FrameFormer chooses the best
    -- conversion method frame by frame. Note that using FrameFormer increases
    -- the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Prelude.Maybe Vp9FramerateConversionAlgorithm,
    -- | GOP Length (keyframe interval) in frames. Must be greater than zero.
    gopSize :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Vp9Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parNumerator', 'vp9Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'framerateDenominator', 'vp9Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'hrdBufferSize', 'vp9Settings_hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
--
-- 'bitrate', 'vp9Settings_bitrate' - Target bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
--
-- 'framerateControl', 'vp9Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
--
-- 'parControl', 'vp9Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio for
-- this output. The default behavior is to use the same pixel aspect ratio
-- as your input video.
--
-- 'qualityTuningLevel', 'vp9Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
--
-- 'maxBitrate', 'vp9Settings_maxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
-- Optional. Specify the maximum bitrate in bits\/second. For example,
-- enter five megabits per second as 5000000. The default behavior uses
-- twice the target bitrate as the maximum bitrate.
--
-- 'framerateNumerator', 'vp9Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'parDenominator', 'vp9Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'rateControlMode', 'vp9Settings_rateControlMode' - With the VP9 codec, you can use only the variable bitrate (VBR) rate
-- control mode.
--
-- 'framerateConversionAlgorithm', 'vp9Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
--
-- 'gopSize', 'vp9Settings_gopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
newVp9Settings ::
  Vp9Settings
newVp9Settings =
  Vp9Settings'
    { parNumerator = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      parControl = Prelude.Nothing,
      qualityTuningLevel = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      gopSize = Prelude.Nothing
    }

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
vp9Settings_parNumerator :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_parNumerator = Lens.lens (\Vp9Settings' {parNumerator} -> parNumerator) (\s@Vp9Settings' {} a -> s {parNumerator = a} :: Vp9Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vp9Settings_framerateDenominator :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_framerateDenominator = Lens.lens (\Vp9Settings' {framerateDenominator} -> framerateDenominator) (\s@Vp9Settings' {} a -> s {framerateDenominator = a} :: Vp9Settings)

-- | Size of buffer (HRD buffer model) in bits. For example, enter five
-- megabits as 5000000.
vp9Settings_hrdBufferSize :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_hrdBufferSize = Lens.lens (\Vp9Settings' {hrdBufferSize} -> hrdBufferSize) (\s@Vp9Settings' {} a -> s {hrdBufferSize = a} :: Vp9Settings)

-- | Target bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
vp9Settings_bitrate :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_bitrate = Lens.lens (\Vp9Settings' {bitrate} -> bitrate) (\s@Vp9Settings' {} a -> s {bitrate = a} :: Vp9Settings)

-- | If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
vp9Settings_framerateControl :: Lens.Lens' Vp9Settings (Prelude.Maybe Vp9FramerateControl)
vp9Settings_framerateControl = Lens.lens (\Vp9Settings' {framerateControl} -> framerateControl) (\s@Vp9Settings' {} a -> s {framerateControl = a} :: Vp9Settings)

-- | Optional. Specify how the service determines the pixel aspect ratio for
-- this output. The default behavior is to use the same pixel aspect ratio
-- as your input video.
vp9Settings_parControl :: Lens.Lens' Vp9Settings (Prelude.Maybe Vp9ParControl)
vp9Settings_parControl = Lens.lens (\Vp9Settings' {parControl} -> parControl) (\s@Vp9Settings' {} a -> s {parControl = a} :: Vp9Settings)

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
vp9Settings_qualityTuningLevel :: Lens.Lens' Vp9Settings (Prelude.Maybe Vp9QualityTuningLevel)
vp9Settings_qualityTuningLevel = Lens.lens (\Vp9Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@Vp9Settings' {} a -> s {qualityTuningLevel = a} :: Vp9Settings)

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
-- Optional. Specify the maximum bitrate in bits\/second. For example,
-- enter five megabits per second as 5000000. The default behavior uses
-- twice the target bitrate as the maximum bitrate.
vp9Settings_maxBitrate :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_maxBitrate = Lens.lens (\Vp9Settings' {maxBitrate} -> maxBitrate) (\s@Vp9Settings' {} a -> s {maxBitrate = a} :: Vp9Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vp9Settings_framerateNumerator :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_framerateNumerator = Lens.lens (\Vp9Settings' {framerateNumerator} -> framerateNumerator) (\s@Vp9Settings' {} a -> s {framerateNumerator = a} :: Vp9Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
vp9Settings_parDenominator :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Natural)
vp9Settings_parDenominator = Lens.lens (\Vp9Settings' {parDenominator} -> parDenominator) (\s@Vp9Settings' {} a -> s {parDenominator = a} :: Vp9Settings)

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate
-- control mode.
vp9Settings_rateControlMode :: Lens.Lens' Vp9Settings (Prelude.Maybe Vp9RateControlMode)
vp9Settings_rateControlMode = Lens.lens (\Vp9Settings' {rateControlMode} -> rateControlMode) (\s@Vp9Settings' {} a -> s {rateControlMode = a} :: Vp9Settings)

-- | Choose the method that you want MediaConvert to use when increasing or
-- decreasing the frame rate. We recommend using drop duplicate
-- (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to
-- 30 fps. For numerically complex conversions, you can use interpolate
-- (INTERPOLATE) to avoid stutter. This results in a smooth picture, but
-- might introduce undesirable video artifacts. For complex frame rate
-- conversions, especially if your source video has already been converted
-- from its original cadence, use FrameFormer (FRAMEFORMER) to do
-- motion-compensated interpolation. FrameFormer chooses the best
-- conversion method frame by frame. Note that using FrameFormer increases
-- the transcoding time and incurs a significant add-on cost.
vp9Settings_framerateConversionAlgorithm :: Lens.Lens' Vp9Settings (Prelude.Maybe Vp9FramerateConversionAlgorithm)
vp9Settings_framerateConversionAlgorithm = Lens.lens (\Vp9Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Vp9Settings' {} a -> s {framerateConversionAlgorithm = a} :: Vp9Settings)

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
vp9Settings_gopSize :: Lens.Lens' Vp9Settings (Prelude.Maybe Prelude.Double)
vp9Settings_gopSize = Lens.lens (\Vp9Settings' {gopSize} -> gopSize) (\s@Vp9Settings' {} a -> s {gopSize = a} :: Vp9Settings)

instance Core.FromJSON Vp9Settings where
  parseJSON =
    Core.withObject
      "Vp9Settings"
      ( \x ->
          Vp9Settings'
            Prelude.<$> (x Core..:? "parNumerator")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "hrdBufferSize")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "parControl")
            Prelude.<*> (x Core..:? "qualityTuningLevel")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "parDenominator")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "gopSize")
      )

instance Prelude.Hashable Vp9Settings where
  hashWithSalt _salt Vp9Settings' {..} =
    _salt `Prelude.hashWithSalt` parNumerator
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` hrdBufferSize
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` parControl
      `Prelude.hashWithSalt` qualityTuningLevel
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` parDenominator
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` gopSize

instance Prelude.NFData Vp9Settings where
  rnf Vp9Settings' {..} =
    Prelude.rnf parNumerator
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf hrdBufferSize
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf parControl
      `Prelude.seq` Prelude.rnf qualityTuningLevel
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf parDenominator
      `Prelude.seq` Prelude.rnf rateControlMode
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf gopSize

instance Core.ToJSON Vp9Settings where
  toJSON Vp9Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("parNumerator" Core..=) Prelude.<$> parNumerator,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("hrdBufferSize" Core..=) Prelude.<$> hrdBufferSize,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("parControl" Core..=) Prelude.<$> parControl,
            ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel,
            ("maxBitrate" Core..=) Prelude.<$> maxBitrate,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("parDenominator" Core..=)
              Prelude.<$> parDenominator,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("gopSize" Core..=) Prelude.<$> gopSize
          ]
      )

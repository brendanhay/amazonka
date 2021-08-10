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
-- Module      : Network.AWS.MediaConvert.Types.Vp8Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Vp8FramerateControl
import Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp8ParControl
import Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp8RateControlMode
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings)
-- to the value VP8.
--
-- /See:/ 'newVp8Settings' smart constructor.
data Vp8Settings = Vp8Settings'
  { -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
    -- you want to trade off encoding speed for output video quality. The
    -- default behavior is faster, lower quality, multi-pass encoding.
    qualityTuningLevel :: Prelude.Maybe Vp8QualityTuningLevel,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | With the VP8 codec, you can use only the variable bitrate (VBR) rate
    -- control mode.
    rateControlMode :: Prelude.Maybe Vp8RateControlMode,
    -- | GOP Length (keyframe interval) in frames. Must be greater than zero.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parNumerator is 40.
    parNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Specify how the service determines the pixel aspect ratio
    -- (PAR) for this output. The default behavior, Follow source
    -- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
    -- output. To specify a different PAR in the console, choose any value
    -- other than Follow source. To specify a different PAR by editing the JSON
    -- job specification, choose SPECIFIED. When you choose SPECIFIED for this
    -- setting, you must also specify values for the parNumerator and
    -- parDenominator settings.
    parControl :: Prelude.Maybe Vp8ParControl,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
    -- the console, this corresponds to any value other than Follow source.
    -- When you specify an output pixel aspect ratio (PAR) that is different
    -- from your input video PAR, provide your output PAR as a ratio. For
    -- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
    -- In this example, the value for parDenominator is 33.
    parDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
    -- Optional. Specify the maximum bitrate in bits\/second. For example,
    -- enter five megabits per second as 5000000. The default behavior uses
    -- twice the target bitrate as the maximum bitrate.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Size of buffer (HRD buffer model) in bits. For example, enter
    -- five megabits as 5000000.
    hrdBufferSize :: Prelude.Maybe Prelude.Natural,
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
    framerateControl :: Prelude.Maybe Vp8FramerateControl,
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
    framerateConversionAlgorithm :: Prelude.Maybe Vp8FramerateConversionAlgorithm,
    -- | Target bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000.
    bitrate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Vp8Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualityTuningLevel', 'vp8Settings_qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
--
-- 'framerateNumerator', 'vp8Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'rateControlMode', 'vp8Settings_rateControlMode' - With the VP8 codec, you can use only the variable bitrate (VBR) rate
-- control mode.
--
-- 'gopSize', 'vp8Settings_gopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- 'framerateDenominator', 'vp8Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'parNumerator', 'vp8Settings_parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
--
-- 'parControl', 'vp8Settings_parControl' - Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
--
-- 'parDenominator', 'vp8Settings_parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
--
-- 'maxBitrate', 'vp8Settings_maxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
-- Optional. Specify the maximum bitrate in bits\/second. For example,
-- enter five megabits per second as 5000000. The default behavior uses
-- twice the target bitrate as the maximum bitrate.
--
-- 'hrdBufferSize', 'vp8Settings_hrdBufferSize' - Optional. Size of buffer (HRD buffer model) in bits. For example, enter
-- five megabits as 5000000.
--
-- 'framerateControl', 'vp8Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
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
-- 'framerateConversionAlgorithm', 'vp8Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'bitrate', 'vp8Settings_bitrate' - Target bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
newVp8Settings ::
  Vp8Settings
newVp8Settings =
  Vp8Settings'
    { qualityTuningLevel = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      parNumerator = Prelude.Nothing,
      parControl = Prelude.Nothing,
      parDenominator = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      hrdBufferSize = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      bitrate = Prelude.Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, multi-pass encoding.
vp8Settings_qualityTuningLevel :: Lens.Lens' Vp8Settings (Prelude.Maybe Vp8QualityTuningLevel)
vp8Settings_qualityTuningLevel = Lens.lens (\Vp8Settings' {qualityTuningLevel} -> qualityTuningLevel) (\s@Vp8Settings' {} a -> s {qualityTuningLevel = a} :: Vp8Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vp8Settings_framerateNumerator :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_framerateNumerator = Lens.lens (\Vp8Settings' {framerateNumerator} -> framerateNumerator) (\s@Vp8Settings' {} a -> s {framerateNumerator = a} :: Vp8Settings)

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate
-- control mode.
vp8Settings_rateControlMode :: Lens.Lens' Vp8Settings (Prelude.Maybe Vp8RateControlMode)
vp8Settings_rateControlMode = Lens.lens (\Vp8Settings' {rateControlMode} -> rateControlMode) (\s@Vp8Settings' {} a -> s {rateControlMode = a} :: Vp8Settings)

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
vp8Settings_gopSize :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Double)
vp8Settings_gopSize = Lens.lens (\Vp8Settings' {gopSize} -> gopSize) (\s@Vp8Settings' {} a -> s {gopSize = a} :: Vp8Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
vp8Settings_framerateDenominator :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_framerateDenominator = Lens.lens (\Vp8Settings' {framerateDenominator} -> framerateDenominator) (\s@Vp8Settings' {} a -> s {framerateDenominator = a} :: Vp8Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parNumerator is 40.
vp8Settings_parNumerator :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_parNumerator = Lens.lens (\Vp8Settings' {parNumerator} -> parNumerator) (\s@Vp8Settings' {} a -> s {parNumerator = a} :: Vp8Settings)

-- | Optional. Specify how the service determines the pixel aspect ratio
-- (PAR) for this output. The default behavior, Follow source
-- (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your
-- output. To specify a different PAR in the console, choose any value
-- other than Follow source. To specify a different PAR by editing the JSON
-- job specification, choose SPECIFIED. When you choose SPECIFIED for this
-- setting, you must also specify values for the parNumerator and
-- parDenominator settings.
vp8Settings_parControl :: Lens.Lens' Vp8Settings (Prelude.Maybe Vp8ParControl)
vp8Settings_parControl = Lens.lens (\Vp8Settings' {parControl} -> parControl) (\s@Vp8Settings' {} a -> s {parControl = a} :: Vp8Settings)

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On
-- the console, this corresponds to any value other than Follow source.
-- When you specify an output pixel aspect ratio (PAR) that is different
-- from your input video PAR, provide your output PAR as a ratio. For
-- example, for D1\/DV NTSC widescreen, you would specify the ratio 40:33.
-- In this example, the value for parDenominator is 33.
vp8Settings_parDenominator :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_parDenominator = Lens.lens (\Vp8Settings' {parDenominator} -> parDenominator) (\s@Vp8Settings' {} a -> s {parDenominator = a} :: Vp8Settings)

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS.
-- Optional. Specify the maximum bitrate in bits\/second. For example,
-- enter five megabits per second as 5000000. The default behavior uses
-- twice the target bitrate as the maximum bitrate.
vp8Settings_maxBitrate :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_maxBitrate = Lens.lens (\Vp8Settings' {maxBitrate} -> maxBitrate) (\s@Vp8Settings' {} a -> s {maxBitrate = a} :: Vp8Settings)

-- | Optional. Size of buffer (HRD buffer model) in bits. For example, enter
-- five megabits as 5000000.
vp8Settings_hrdBufferSize :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_hrdBufferSize = Lens.lens (\Vp8Settings' {hrdBufferSize} -> hrdBufferSize) (\s@Vp8Settings' {} a -> s {hrdBufferSize = a} :: Vp8Settings)

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
vp8Settings_framerateControl :: Lens.Lens' Vp8Settings (Prelude.Maybe Vp8FramerateControl)
vp8Settings_framerateControl = Lens.lens (\Vp8Settings' {framerateControl} -> framerateControl) (\s@Vp8Settings' {} a -> s {framerateControl = a} :: Vp8Settings)

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
vp8Settings_framerateConversionAlgorithm :: Lens.Lens' Vp8Settings (Prelude.Maybe Vp8FramerateConversionAlgorithm)
vp8Settings_framerateConversionAlgorithm = Lens.lens (\Vp8Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Vp8Settings' {} a -> s {framerateConversionAlgorithm = a} :: Vp8Settings)

-- | Target bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000.
vp8Settings_bitrate :: Lens.Lens' Vp8Settings (Prelude.Maybe Prelude.Natural)
vp8Settings_bitrate = Lens.lens (\Vp8Settings' {bitrate} -> bitrate) (\s@Vp8Settings' {} a -> s {bitrate = a} :: Vp8Settings)

instance Core.FromJSON Vp8Settings where
  parseJSON =
    Core.withObject
      "Vp8Settings"
      ( \x ->
          Vp8Settings'
            Prelude.<$> (x Core..:? "qualityTuningLevel")
            Prelude.<*> (x Core..:? "framerateNumerator")
            Prelude.<*> (x Core..:? "rateControlMode")
            Prelude.<*> (x Core..:? "gopSize")
            Prelude.<*> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "parNumerator")
            Prelude.<*> (x Core..:? "parControl")
            Prelude.<*> (x Core..:? "parDenominator")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "hrdBufferSize")
            Prelude.<*> (x Core..:? "framerateControl")
            Prelude.<*> (x Core..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Core..:? "bitrate")
      )

instance Prelude.Hashable Vp8Settings

instance Prelude.NFData Vp8Settings

instance Core.ToJSON Vp8Settings where
  toJSON Vp8Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("qualityTuningLevel" Core..=)
              Prelude.<$> qualityTuningLevel,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator,
            ("rateControlMode" Core..=)
              Prelude.<$> rateControlMode,
            ("gopSize" Core..=) Prelude.<$> gopSize,
            ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("parNumerator" Core..=) Prelude.<$> parNumerator,
            ("parControl" Core..=) Prelude.<$> parControl,
            ("parDenominator" Core..=)
              Prelude.<$> parDenominator,
            ("maxBitrate" Core..=) Prelude.<$> maxBitrate,
            ("hrdBufferSize" Core..=) Prelude.<$> hrdBufferSize,
            ("framerateControl" Core..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Core..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("bitrate" Core..=) Prelude.<$> bitrate
          ]
      )

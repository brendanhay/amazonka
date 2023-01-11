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
-- Module      : Amazonka.MediaConvert.Types.Av1Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Av1Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Av1AdaptiveQuantization
import Amazonka.MediaConvert.Types.Av1BitDepth
import Amazonka.MediaConvert.Types.Av1FramerateControl
import Amazonka.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Amazonka.MediaConvert.Types.Av1QvbrSettings
import Amazonka.MediaConvert.Types.Av1RateControlMode
import Amazonka.MediaConvert.Types.Av1SpatialAdaptiveQuantization
import qualified Amazonka.Prelude as Prelude

-- | Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
--
-- /See:/ 'newAv1Settings' smart constructor.
data Av1Settings = Av1Settings'
  { -- | Specify the strength of any adaptive quantization filters that you
    -- enable. The value that you choose here applies to Spatial adaptive
    -- quantization (spatialAdaptiveQuantization).
    adaptiveQuantization :: Prelude.Maybe Av1AdaptiveQuantization,
    -- | Specify the Bit depth (Av1BitDepth). You can choose 8-bit (BIT_8) or
    -- 10-bit (BIT_10).
    bitDepth :: Prelude.Maybe Av1BitDepth,
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
    framerateControl :: Prelude.Maybe Av1FramerateControl,
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
    framerateConversionAlgorithm :: Prelude.Maybe Av1FramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Prelude.Maybe Prelude.Natural,
    -- | Specify the GOP length (keyframe interval) in frames. With AV1,
    -- MediaConvert doesn\'t support GOP length in seconds. This value must be
    -- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
    -- where x is an integer value.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify from the number of B-frames, in the range of 0-15. For AV1
    -- encoding, we recommend using 7 or 15. Choose a larger number for a lower
    -- bitrate and smaller file size; choose a smaller number for better video
    -- quality.
    numberBFramesBetweenReferenceFrames :: Prelude.Maybe Prelude.Natural,
    -- | Settings for quality-defined variable bitrate encoding with the H.265
    -- codec. Use these settings only when you set QVBR for Rate control mode
    -- (RateControlMode).
    qvbrSettings :: Prelude.Maybe Av1QvbrSettings,
    -- | \'With AV1 outputs, for rate control mode, MediaConvert supports only
    -- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
    rateControlMode :: Prelude.Maybe Av1RateControlMode,
    -- | Specify the number of slices per picture. This value must be 1, 2, 4, 8,
    -- 16, or 32. For progressive pictures, this value must be less than or
    -- equal to the number of macroblock rows. For interlaced pictures, this
    -- value must be less than or equal to half the number of macroblock rows.
    slices :: Prelude.Maybe Prelude.Natural,
    -- | Keep the default value, Enabled (ENABLED), to adjust quantization within
    -- each frame based on spatial variation of content complexity. When you
    -- enable this feature, the encoder uses fewer bits on areas that can
    -- sustain more distortion with no noticeable visual degradation and uses
    -- more bits on areas where any small distortion will be noticeable. For
    -- example, complex textured blocks are encoded with fewer bits and smooth
    -- textured blocks are encoded with more bits. Enabling this feature will
    -- almost always improve your video quality. Note, though, that this
    -- feature doesn\'t take into account where the viewer\'s attention is
    -- likely to be. If viewers are likely to be focusing their attention on a
    -- part of the screen with a lot of complex texture, you might choose to
    -- disable this feature. Related setting: When you enable spatial adaptive
    -- quantization, set the value for Adaptive quantization
    -- (adaptiveQuantization) depending on your content. For homogeneous
    -- content, such as cartoons and video games, set it to Low. For content
    -- with a wider variety of textures, set it to High or Higher.
    spatialAdaptiveQuantization :: Prelude.Maybe Av1SpatialAdaptiveQuantization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Av1Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adaptiveQuantization', 'av1Settings_adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
--
-- 'bitDepth', 'av1Settings_bitDepth' - Specify the Bit depth (Av1BitDepth). You can choose 8-bit (BIT_8) or
-- 10-bit (BIT_10).
--
-- 'framerateControl', 'av1Settings_framerateControl' - If you are using the console, use the Framerate setting to specify the
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
-- 'framerateConversionAlgorithm', 'av1Settings_framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or
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
-- 'framerateDenominator', 'av1Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'framerateNumerator', 'av1Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'gopSize', 'av1Settings_gopSize' - Specify the GOP length (keyframe interval) in frames. With AV1,
-- MediaConvert doesn\'t support GOP length in seconds. This value must be
-- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
-- where x is an integer value.
--
-- 'maxBitrate', 'av1Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'numberBFramesBetweenReferenceFrames', 'av1Settings_numberBFramesBetweenReferenceFrames' - Specify from the number of B-frames, in the range of 0-15. For AV1
-- encoding, we recommend using 7 or 15. Choose a larger number for a lower
-- bitrate and smaller file size; choose a smaller number for better video
-- quality.
--
-- 'qvbrSettings', 'av1Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- 'rateControlMode', 'av1Settings_rateControlMode' - \'With AV1 outputs, for rate control mode, MediaConvert supports only
-- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
--
-- 'slices', 'av1Settings_slices' - Specify the number of slices per picture. This value must be 1, 2, 4, 8,
-- 16, or 32. For progressive pictures, this value must be less than or
-- equal to the number of macroblock rows. For interlaced pictures, this
-- value must be less than or equal to half the number of macroblock rows.
--
-- 'spatialAdaptiveQuantization', 'av1Settings_spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
newAv1Settings ::
  Av1Settings
newAv1Settings =
  Av1Settings'
    { adaptiveQuantization =
        Prelude.Nothing,
      bitDepth = Prelude.Nothing,
      framerateControl = Prelude.Nothing,
      framerateConversionAlgorithm = Prelude.Nothing,
      framerateDenominator = Prelude.Nothing,
      framerateNumerator = Prelude.Nothing,
      gopSize = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      numberBFramesBetweenReferenceFrames =
        Prelude.Nothing,
      qvbrSettings = Prelude.Nothing,
      rateControlMode = Prelude.Nothing,
      slices = Prelude.Nothing,
      spatialAdaptiveQuantization = Prelude.Nothing
    }

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
av1Settings_adaptiveQuantization :: Lens.Lens' Av1Settings (Prelude.Maybe Av1AdaptiveQuantization)
av1Settings_adaptiveQuantization = Lens.lens (\Av1Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@Av1Settings' {} a -> s {adaptiveQuantization = a} :: Av1Settings)

-- | Specify the Bit depth (Av1BitDepth). You can choose 8-bit (BIT_8) or
-- 10-bit (BIT_10).
av1Settings_bitDepth :: Lens.Lens' Av1Settings (Prelude.Maybe Av1BitDepth)
av1Settings_bitDepth = Lens.lens (\Av1Settings' {bitDepth} -> bitDepth) (\s@Av1Settings' {} a -> s {bitDepth = a} :: Av1Settings)

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
av1Settings_framerateControl :: Lens.Lens' Av1Settings (Prelude.Maybe Av1FramerateControl)
av1Settings_framerateControl = Lens.lens (\Av1Settings' {framerateControl} -> framerateControl) (\s@Av1Settings' {} a -> s {framerateControl = a} :: Av1Settings)

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
av1Settings_framerateConversionAlgorithm :: Lens.Lens' Av1Settings (Prelude.Maybe Av1FramerateConversionAlgorithm)
av1Settings_framerateConversionAlgorithm = Lens.lens (\Av1Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Av1Settings' {} a -> s {framerateConversionAlgorithm = a} :: Av1Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
av1Settings_framerateDenominator :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Natural)
av1Settings_framerateDenominator = Lens.lens (\Av1Settings' {framerateDenominator} -> framerateDenominator) (\s@Av1Settings' {} a -> s {framerateDenominator = a} :: Av1Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
av1Settings_framerateNumerator :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Natural)
av1Settings_framerateNumerator = Lens.lens (\Av1Settings' {framerateNumerator} -> framerateNumerator) (\s@Av1Settings' {} a -> s {framerateNumerator = a} :: Av1Settings)

-- | Specify the GOP length (keyframe interval) in frames. With AV1,
-- MediaConvert doesn\'t support GOP length in seconds. This value must be
-- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
-- where x is an integer value.
av1Settings_gopSize :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Double)
av1Settings_gopSize = Lens.lens (\Av1Settings' {gopSize} -> gopSize) (\s@Av1Settings' {} a -> s {gopSize = a} :: Av1Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
av1Settings_maxBitrate :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Natural)
av1Settings_maxBitrate = Lens.lens (\Av1Settings' {maxBitrate} -> maxBitrate) (\s@Av1Settings' {} a -> s {maxBitrate = a} :: Av1Settings)

-- | Specify from the number of B-frames, in the range of 0-15. For AV1
-- encoding, we recommend using 7 or 15. Choose a larger number for a lower
-- bitrate and smaller file size; choose a smaller number for better video
-- quality.
av1Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Natural)
av1Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\Av1Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@Av1Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: Av1Settings)

-- | Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
av1Settings_qvbrSettings :: Lens.Lens' Av1Settings (Prelude.Maybe Av1QvbrSettings)
av1Settings_qvbrSettings = Lens.lens (\Av1Settings' {qvbrSettings} -> qvbrSettings) (\s@Av1Settings' {} a -> s {qvbrSettings = a} :: Av1Settings)

-- | \'With AV1 outputs, for rate control mode, MediaConvert supports only
-- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
av1Settings_rateControlMode :: Lens.Lens' Av1Settings (Prelude.Maybe Av1RateControlMode)
av1Settings_rateControlMode = Lens.lens (\Av1Settings' {rateControlMode} -> rateControlMode) (\s@Av1Settings' {} a -> s {rateControlMode = a} :: Av1Settings)

-- | Specify the number of slices per picture. This value must be 1, 2, 4, 8,
-- 16, or 32. For progressive pictures, this value must be less than or
-- equal to the number of macroblock rows. For interlaced pictures, this
-- value must be less than or equal to half the number of macroblock rows.
av1Settings_slices :: Lens.Lens' Av1Settings (Prelude.Maybe Prelude.Natural)
av1Settings_slices = Lens.lens (\Av1Settings' {slices} -> slices) (\s@Av1Settings' {} a -> s {slices = a} :: Av1Settings)

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on spatial variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas that can
-- sustain more distortion with no noticeable visual degradation and uses
-- more bits on areas where any small distortion will be noticeable. For
-- example, complex textured blocks are encoded with fewer bits and smooth
-- textured blocks are encoded with more bits. Enabling this feature will
-- almost always improve your video quality. Note, though, that this
-- feature doesn\'t take into account where the viewer\'s attention is
-- likely to be. If viewers are likely to be focusing their attention on a
-- part of the screen with a lot of complex texture, you might choose to
-- disable this feature. Related setting: When you enable spatial adaptive
-- quantization, set the value for Adaptive quantization
-- (adaptiveQuantization) depending on your content. For homogeneous
-- content, such as cartoons and video games, set it to Low. For content
-- with a wider variety of textures, set it to High or Higher.
av1Settings_spatialAdaptiveQuantization :: Lens.Lens' Av1Settings (Prelude.Maybe Av1SpatialAdaptiveQuantization)
av1Settings_spatialAdaptiveQuantization = Lens.lens (\Av1Settings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@Av1Settings' {} a -> s {spatialAdaptiveQuantization = a} :: Av1Settings)

instance Data.FromJSON Av1Settings where
  parseJSON =
    Data.withObject
      "Av1Settings"
      ( \x ->
          Av1Settings'
            Prelude.<$> (x Data..:? "adaptiveQuantization")
            Prelude.<*> (x Data..:? "bitDepth")
            Prelude.<*> (x Data..:? "framerateControl")
            Prelude.<*> (x Data..:? "framerateConversionAlgorithm")
            Prelude.<*> (x Data..:? "framerateDenominator")
            Prelude.<*> (x Data..:? "framerateNumerator")
            Prelude.<*> (x Data..:? "gopSize")
            Prelude.<*> (x Data..:? "maxBitrate")
            Prelude.<*> (x Data..:? "numberBFramesBetweenReferenceFrames")
            Prelude.<*> (x Data..:? "qvbrSettings")
            Prelude.<*> (x Data..:? "rateControlMode")
            Prelude.<*> (x Data..:? "slices")
            Prelude.<*> (x Data..:? "spatialAdaptiveQuantization")
      )

instance Prelude.Hashable Av1Settings where
  hashWithSalt _salt Av1Settings' {..} =
    _salt `Prelude.hashWithSalt` adaptiveQuantization
      `Prelude.hashWithSalt` bitDepth
      `Prelude.hashWithSalt` framerateControl
      `Prelude.hashWithSalt` framerateConversionAlgorithm
      `Prelude.hashWithSalt` framerateDenominator
      `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` gopSize
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` numberBFramesBetweenReferenceFrames
      `Prelude.hashWithSalt` qvbrSettings
      `Prelude.hashWithSalt` rateControlMode
      `Prelude.hashWithSalt` slices
      `Prelude.hashWithSalt` spatialAdaptiveQuantization

instance Prelude.NFData Av1Settings where
  rnf Av1Settings' {..} =
    Prelude.rnf adaptiveQuantization
      `Prelude.seq` Prelude.rnf bitDepth
      `Prelude.seq` Prelude.rnf framerateControl
      `Prelude.seq` Prelude.rnf framerateConversionAlgorithm
      `Prelude.seq` Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator
      `Prelude.seq` Prelude.rnf gopSize
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf numberBFramesBetweenReferenceFrames
      `Prelude.seq` Prelude.rnf qvbrSettings
      `Prelude.seq` Prelude.rnf rateControlMode
      `Prelude.seq` Prelude.rnf slices
      `Prelude.seq` Prelude.rnf spatialAdaptiveQuantization

instance Data.ToJSON Av1Settings where
  toJSON Av1Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adaptiveQuantization" Data..=)
              Prelude.<$> adaptiveQuantization,
            ("bitDepth" Data..=) Prelude.<$> bitDepth,
            ("framerateControl" Data..=)
              Prelude.<$> framerateControl,
            ("framerateConversionAlgorithm" Data..=)
              Prelude.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Data..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Data..=)
              Prelude.<$> framerateNumerator,
            ("gopSize" Data..=) Prelude.<$> gopSize,
            ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("numberBFramesBetweenReferenceFrames" Data..=)
              Prelude.<$> numberBFramesBetweenReferenceFrames,
            ("qvbrSettings" Data..=) Prelude.<$> qvbrSettings,
            ("rateControlMode" Data..=)
              Prelude.<$> rateControlMode,
            ("slices" Data..=) Prelude.<$> slices,
            ("spatialAdaptiveQuantization" Data..=)
              Prelude.<$> spatialAdaptiveQuantization
          ]
      )

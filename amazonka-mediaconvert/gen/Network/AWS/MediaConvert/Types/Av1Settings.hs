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
-- Module      : Network.AWS.MediaConvert.Types.Av1Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Av1FramerateControl
import Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
import Network.AWS.MediaConvert.Types.Av1RateControlMode
import Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization

-- | Required when you set Codec, under VideoDescription>CodecSettings to the
-- value AV1.
--
-- /See:/ 'newAv1Settings' smart constructor.
data Av1Settings = Av1Settings'
  { -- | Keep the default value, Enabled (ENABLED), to adjust quantization within
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
    spatialAdaptiveQuantization :: Core.Maybe Av1SpatialAdaptiveQuantization,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateNumerator to specify the numerator of this
    -- fraction. In this example, use 24000 for the value of
    -- FramerateNumerator. When you use the console for transcode jobs that use
    -- frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateNumerator :: Core.Maybe Core.Natural,
    -- | \'With AV1 outputs, for rate control mode, MediaConvert supports only
    -- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
    rateControlMode :: Core.Maybe Av1RateControlMode,
    -- | Specify the number of slices per picture. This value must be 1, 2, 4, 8,
    -- 16, or 32. For progressive pictures, this value must be less than or
    -- equal to the number of macroblock rows. For interlaced pictures, this
    -- value must be less than or equal to half the number of macroblock rows.
    slices :: Core.Maybe Core.Natural,
    -- | Specify the GOP length (keyframe interval) in frames. With AV1,
    -- MediaConvert doesn\'t support GOP length in seconds. This value must be
    -- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
    -- where x is an integer value.
    gopSize :: Core.Maybe Core.Double,
    -- | When you use the API for transcode jobs that use frame rate conversion,
    -- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
    -- 23.976 fps. Use FramerateDenominator to specify the denominator of this
    -- fraction. In this example, use 1001 for the value of
    -- FramerateDenominator. When you use the console for transcode jobs that
    -- use frame rate conversion, provide the value as a decimal number for
    -- Framerate. In this example, specify 23.976.
    framerateDenominator :: Core.Maybe Core.Natural,
    -- | Maximum bitrate in bits\/second. For example, enter five megabits per
    -- second as 5000000. Required when Rate control mode is QVBR.
    maxBitrate :: Core.Maybe Core.Natural,
    -- | Specify the strength of any adaptive quantization filters that you
    -- enable. The value that you choose here applies to Spatial adaptive
    -- quantization (spatialAdaptiveQuantization).
    adaptiveQuantization :: Core.Maybe Av1AdaptiveQuantization,
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
    framerateControl :: Core.Maybe Av1FramerateControl,
    -- | Specify the number of B-frames. With AV1, MediaConvert supports only 7
    -- or 15.
    numberBFramesBetweenReferenceFrames :: Core.Maybe Core.Natural,
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
    framerateConversionAlgorithm :: Core.Maybe Av1FramerateConversionAlgorithm,
    -- | Settings for quality-defined variable bitrate encoding with the AV1
    -- codec. Required when you set Rate control mode to QVBR. Not valid when
    -- you set Rate control mode to a value other than QVBR, or when you don\'t
    -- define Rate control mode.
    qvbrSettings :: Core.Maybe Av1QvbrSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Av1Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'framerateNumerator', 'av1Settings_framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'rateControlMode', 'av1Settings_rateControlMode' - \'With AV1 outputs, for rate control mode, MediaConvert supports only
-- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
--
-- 'slices', 'av1Settings_slices' - Specify the number of slices per picture. This value must be 1, 2, 4, 8,
-- 16, or 32. For progressive pictures, this value must be less than or
-- equal to the number of macroblock rows. For interlaced pictures, this
-- value must be less than or equal to half the number of macroblock rows.
--
-- 'gopSize', 'av1Settings_gopSize' - Specify the GOP length (keyframe interval) in frames. With AV1,
-- MediaConvert doesn\'t support GOP length in seconds. This value must be
-- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
-- where x is an integer value.
--
-- 'framerateDenominator', 'av1Settings_framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
--
-- 'maxBitrate', 'av1Settings_maxBitrate' - Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
--
-- 'adaptiveQuantization', 'av1Settings_adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
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
-- 'numberBFramesBetweenReferenceFrames', 'av1Settings_numberBFramesBetweenReferenceFrames' - Specify the number of B-frames. With AV1, MediaConvert supports only 7
-- or 15.
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
-- 'qvbrSettings', 'av1Settings_qvbrSettings' - Settings for quality-defined variable bitrate encoding with the AV1
-- codec. Required when you set Rate control mode to QVBR. Not valid when
-- you set Rate control mode to a value other than QVBR, or when you don\'t
-- define Rate control mode.
newAv1Settings ::
  Av1Settings
newAv1Settings =
  Av1Settings'
    { spatialAdaptiveQuantization =
        Core.Nothing,
      framerateNumerator = Core.Nothing,
      rateControlMode = Core.Nothing,
      slices = Core.Nothing,
      gopSize = Core.Nothing,
      framerateDenominator = Core.Nothing,
      maxBitrate = Core.Nothing,
      adaptiveQuantization = Core.Nothing,
      framerateControl = Core.Nothing,
      numberBFramesBetweenReferenceFrames = Core.Nothing,
      framerateConversionAlgorithm = Core.Nothing,
      qvbrSettings = Core.Nothing
    }

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
av1Settings_spatialAdaptiveQuantization :: Lens.Lens' Av1Settings (Core.Maybe Av1SpatialAdaptiveQuantization)
av1Settings_spatialAdaptiveQuantization = Lens.lens (\Av1Settings' {spatialAdaptiveQuantization} -> spatialAdaptiveQuantization) (\s@Av1Settings' {} a -> s {spatialAdaptiveQuantization = a} :: Av1Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateNumerator to specify the numerator of this
-- fraction. In this example, use 24000 for the value of
-- FramerateNumerator. When you use the console for transcode jobs that use
-- frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
av1Settings_framerateNumerator :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
av1Settings_framerateNumerator = Lens.lens (\Av1Settings' {framerateNumerator} -> framerateNumerator) (\s@Av1Settings' {} a -> s {framerateNumerator = a} :: Av1Settings)

-- | \'With AV1 outputs, for rate control mode, MediaConvert supports only
-- quality-defined variable bitrate (QVBR). You can\'\'t use CBR or VBR.\'
av1Settings_rateControlMode :: Lens.Lens' Av1Settings (Core.Maybe Av1RateControlMode)
av1Settings_rateControlMode = Lens.lens (\Av1Settings' {rateControlMode} -> rateControlMode) (\s@Av1Settings' {} a -> s {rateControlMode = a} :: Av1Settings)

-- | Specify the number of slices per picture. This value must be 1, 2, 4, 8,
-- 16, or 32. For progressive pictures, this value must be less than or
-- equal to the number of macroblock rows. For interlaced pictures, this
-- value must be less than or equal to half the number of macroblock rows.
av1Settings_slices :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
av1Settings_slices = Lens.lens (\Av1Settings' {slices} -> slices) (\s@Av1Settings' {} a -> s {slices = a} :: Av1Settings)

-- | Specify the GOP length (keyframe interval) in frames. With AV1,
-- MediaConvert doesn\'t support GOP length in seconds. This value must be
-- greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x),
-- where x is an integer value.
av1Settings_gopSize :: Lens.Lens' Av1Settings (Core.Maybe Core.Double)
av1Settings_gopSize = Lens.lens (\Av1Settings' {gopSize} -> gopSize) (\s@Av1Settings' {} a -> s {gopSize = a} :: Av1Settings)

-- | When you use the API for transcode jobs that use frame rate conversion,
-- specify the frame rate as a fraction. For example, 24000 \/ 1001 =
-- 23.976 fps. Use FramerateDenominator to specify the denominator of this
-- fraction. In this example, use 1001 for the value of
-- FramerateDenominator. When you use the console for transcode jobs that
-- use frame rate conversion, provide the value as a decimal number for
-- Framerate. In this example, specify 23.976.
av1Settings_framerateDenominator :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
av1Settings_framerateDenominator = Lens.lens (\Av1Settings' {framerateDenominator} -> framerateDenominator) (\s@Av1Settings' {} a -> s {framerateDenominator = a} :: Av1Settings)

-- | Maximum bitrate in bits\/second. For example, enter five megabits per
-- second as 5000000. Required when Rate control mode is QVBR.
av1Settings_maxBitrate :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
av1Settings_maxBitrate = Lens.lens (\Av1Settings' {maxBitrate} -> maxBitrate) (\s@Av1Settings' {} a -> s {maxBitrate = a} :: Av1Settings)

-- | Specify the strength of any adaptive quantization filters that you
-- enable. The value that you choose here applies to Spatial adaptive
-- quantization (spatialAdaptiveQuantization).
av1Settings_adaptiveQuantization :: Lens.Lens' Av1Settings (Core.Maybe Av1AdaptiveQuantization)
av1Settings_adaptiveQuantization = Lens.lens (\Av1Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@Av1Settings' {} a -> s {adaptiveQuantization = a} :: Av1Settings)

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
av1Settings_framerateControl :: Lens.Lens' Av1Settings (Core.Maybe Av1FramerateControl)
av1Settings_framerateControl = Lens.lens (\Av1Settings' {framerateControl} -> framerateControl) (\s@Av1Settings' {} a -> s {framerateControl = a} :: Av1Settings)

-- | Specify the number of B-frames. With AV1, MediaConvert supports only 7
-- or 15.
av1Settings_numberBFramesBetweenReferenceFrames :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
av1Settings_numberBFramesBetweenReferenceFrames = Lens.lens (\Av1Settings' {numberBFramesBetweenReferenceFrames} -> numberBFramesBetweenReferenceFrames) (\s@Av1Settings' {} a -> s {numberBFramesBetweenReferenceFrames = a} :: Av1Settings)

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
av1Settings_framerateConversionAlgorithm :: Lens.Lens' Av1Settings (Core.Maybe Av1FramerateConversionAlgorithm)
av1Settings_framerateConversionAlgorithm = Lens.lens (\Av1Settings' {framerateConversionAlgorithm} -> framerateConversionAlgorithm) (\s@Av1Settings' {} a -> s {framerateConversionAlgorithm = a} :: Av1Settings)

-- | Settings for quality-defined variable bitrate encoding with the AV1
-- codec. Required when you set Rate control mode to QVBR. Not valid when
-- you set Rate control mode to a value other than QVBR, or when you don\'t
-- define Rate control mode.
av1Settings_qvbrSettings :: Lens.Lens' Av1Settings (Core.Maybe Av1QvbrSettings)
av1Settings_qvbrSettings = Lens.lens (\Av1Settings' {qvbrSettings} -> qvbrSettings) (\s@Av1Settings' {} a -> s {qvbrSettings = a} :: Av1Settings)

instance Core.FromJSON Av1Settings where
  parseJSON =
    Core.withObject
      "Av1Settings"
      ( \x ->
          Av1Settings'
            Core.<$> (x Core..:? "spatialAdaptiveQuantization")
            Core.<*> (x Core..:? "framerateNumerator")
            Core.<*> (x Core..:? "rateControlMode")
            Core.<*> (x Core..:? "slices")
            Core.<*> (x Core..:? "gopSize")
            Core.<*> (x Core..:? "framerateDenominator")
            Core.<*> (x Core..:? "maxBitrate")
            Core.<*> (x Core..:? "adaptiveQuantization")
            Core.<*> (x Core..:? "framerateControl")
            Core.<*> (x Core..:? "numberBFramesBetweenReferenceFrames")
            Core.<*> (x Core..:? "framerateConversionAlgorithm")
            Core.<*> (x Core..:? "qvbrSettings")
      )

instance Core.Hashable Av1Settings

instance Core.NFData Av1Settings

instance Core.ToJSON Av1Settings where
  toJSON Av1Settings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("spatialAdaptiveQuantization" Core..=)
              Core.<$> spatialAdaptiveQuantization,
            ("framerateNumerator" Core..=)
              Core.<$> framerateNumerator,
            ("rateControlMode" Core..=) Core.<$> rateControlMode,
            ("slices" Core..=) Core.<$> slices,
            ("gopSize" Core..=) Core.<$> gopSize,
            ("framerateDenominator" Core..=)
              Core.<$> framerateDenominator,
            ("maxBitrate" Core..=) Core.<$> maxBitrate,
            ("adaptiveQuantization" Core..=)
              Core.<$> adaptiveQuantization,
            ("framerateControl" Core..=)
              Core.<$> framerateControl,
            ("numberBFramesBetweenReferenceFrames" Core..=)
              Core.<$> numberBFramesBetweenReferenceFrames,
            ("framerateConversionAlgorithm" Core..=)
              Core.<$> framerateConversionAlgorithm,
            ("qvbrSettings" Core..=) Core.<$> qvbrSettings
          ]
      )

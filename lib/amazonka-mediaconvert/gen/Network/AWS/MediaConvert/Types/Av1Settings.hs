{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1Settings
  ( Av1Settings (..),

    -- * Smart constructor
    mkAv1Settings,

    -- * Lenses
    asGopSize,
    asNumberBFramesBetweenReferenceFrames,
    asSlices,
    asRateControlMode,
    asQvbrSettings,
    asFramerateDenominator,
    asFramerateConversionAlgorithm,
    asFramerateControl,
    asAdaptiveQuantization,
    asFramerateNumerator,
    asMaxBitrate,
    asSpatialAdaptiveQuantization,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
import Network.AWS.MediaConvert.Types.Av1FramerateControl
import Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Av1QvbrSettings
import Network.AWS.MediaConvert.Types.Av1RateControlMode
import Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization
import qualified Network.AWS.Prelude as Lude

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- /See:/ 'mkAv1Settings' smart constructor.
data Av1Settings = Av1Settings'
  { gopSize :: Lude.Maybe Lude.Double,
    numberBFramesBetweenReferenceFrames :: Lude.Maybe Lude.Natural,
    slices :: Lude.Maybe Lude.Natural,
    rateControlMode :: Lude.Maybe Av1RateControlMode,
    qvbrSettings :: Lude.Maybe Av1QvbrSettings,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    framerateConversionAlgorithm ::
      Lude.Maybe Av1FramerateConversionAlgorithm,
    framerateControl :: Lude.Maybe Av1FramerateControl,
    adaptiveQuantization :: Lude.Maybe Av1AdaptiveQuantization,
    framerateNumerator :: Lude.Maybe Lude.Natural,
    maxBitrate :: Lude.Maybe Lude.Natural,
    spatialAdaptiveQuantization ::
      Lude.Maybe Av1SpatialAdaptiveQuantization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Av1Settings' with the minimum fields required to make a request.
--
-- * 'adaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'gopSize' - Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
-- * 'maxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
-- * 'numberBFramesBetweenReferenceFrames' - Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
-- * 'qvbrSettings' - Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
-- * 'rateControlMode' - 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
-- * 'slices' - Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
-- * 'spatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
mkAv1Settings ::
  Av1Settings
mkAv1Settings =
  Av1Settings'
    { gopSize = Lude.Nothing,
      numberBFramesBetweenReferenceFrames = Lude.Nothing,
      slices = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      qvbrSettings = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      framerateControl = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      spatialAdaptiveQuantization = Lude.Nothing
    }

-- | Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asGopSize :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Double)
asGopSize = Lens.lens (gopSize :: Av1Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: Av1Settings)
{-# DEPRECATED asGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asNumberBFramesBetweenReferenceFrames :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Natural)
asNumberBFramesBetweenReferenceFrames = Lens.lens (numberBFramesBetweenReferenceFrames :: Av1Settings -> Lude.Maybe Lude.Natural) (\s a -> s {numberBFramesBetweenReferenceFrames = a} :: Av1Settings)
{-# DEPRECATED asNumberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead." #-}

-- | Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSlices :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Natural)
asSlices = Lens.lens (slices :: Av1Settings -> Lude.Maybe Lude.Natural) (\s a -> s {slices = a} :: Av1Settings)
{-# DEPRECATED asSlices "Use generic-lens or generic-optics with 'slices' instead." #-}

-- | 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRateControlMode :: Lens.Lens' Av1Settings (Lude.Maybe Av1RateControlMode)
asRateControlMode = Lens.lens (rateControlMode :: Av1Settings -> Lude.Maybe Av1RateControlMode) (\s a -> s {rateControlMode = a} :: Av1Settings)
{-# DEPRECATED asRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asQvbrSettings :: Lens.Lens' Av1Settings (Lude.Maybe Av1QvbrSettings)
asQvbrSettings = Lens.lens (qvbrSettings :: Av1Settings -> Lude.Maybe Av1QvbrSettings) (\s a -> s {qvbrSettings = a} :: Av1Settings)
{-# DEPRECATED asQvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFramerateDenominator :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Natural)
asFramerateDenominator = Lens.lens (framerateDenominator :: Av1Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: Av1Settings)
{-# DEPRECATED asFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFramerateConversionAlgorithm :: Lens.Lens' Av1Settings (Lude.Maybe Av1FramerateConversionAlgorithm)
asFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: Av1Settings -> Lude.Maybe Av1FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: Av1Settings)
{-# DEPRECATED asFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFramerateControl :: Lens.Lens' Av1Settings (Lude.Maybe Av1FramerateControl)
asFramerateControl = Lens.lens (framerateControl :: Av1Settings -> Lude.Maybe Av1FramerateControl) (\s a -> s {framerateControl = a} :: Av1Settings)
{-# DEPRECATED asFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAdaptiveQuantization :: Lens.Lens' Av1Settings (Lude.Maybe Av1AdaptiveQuantization)
asAdaptiveQuantization = Lens.lens (adaptiveQuantization :: Av1Settings -> Lude.Maybe Av1AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: Av1Settings)
{-# DEPRECATED asAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asFramerateNumerator :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Natural)
asFramerateNumerator = Lens.lens (framerateNumerator :: Av1Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: Av1Settings)
{-# DEPRECATED asFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxBitrate :: Lens.Lens' Av1Settings (Lude.Maybe Lude.Natural)
asMaxBitrate = Lens.lens (maxBitrate :: Av1Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: Av1Settings)
{-# DEPRECATED asMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSpatialAdaptiveQuantization :: Lens.Lens' Av1Settings (Lude.Maybe Av1SpatialAdaptiveQuantization)
asSpatialAdaptiveQuantization = Lens.lens (spatialAdaptiveQuantization :: Av1Settings -> Lude.Maybe Av1SpatialAdaptiveQuantization) (\s a -> s {spatialAdaptiveQuantization = a} :: Av1Settings)
{-# DEPRECATED asSpatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead." #-}

instance Lude.FromJSON Av1Settings where
  parseJSON =
    Lude.withObject
      "Av1Settings"
      ( \x ->
          Av1Settings'
            Lude.<$> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "numberBFramesBetweenReferenceFrames")
            Lude.<*> (x Lude..:? "slices")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "qvbrSettings")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "spatialAdaptiveQuantization")
      )

instance Lude.ToJSON Av1Settings where
  toJSON Av1Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("gopSize" Lude..=) Lude.<$> gopSize,
            ("numberBFramesBetweenReferenceFrames" Lude..=)
              Lude.<$> numberBFramesBetweenReferenceFrames,
            ("slices" Lude..=) Lude.<$> slices,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("qvbrSettings" Lude..=) Lude.<$> qvbrSettings,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("spatialAdaptiveQuantization" Lude..=)
              Lude.<$> spatialAdaptiveQuantization
          ]
      )

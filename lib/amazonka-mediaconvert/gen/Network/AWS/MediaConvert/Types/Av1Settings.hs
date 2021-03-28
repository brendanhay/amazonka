{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1Settings
  ( Av1Settings (..)
  -- * Smart constructor
  , mkAv1Settings
  -- * Lenses
  , aAdaptiveQuantization
  , aFramerateControl
  , aFramerateConversionAlgorithm
  , aFramerateDenominator
  , aFramerateNumerator
  , aGopSize
  , aMaxBitrate
  , aNumberBFramesBetweenReferenceFrames
  , aQvbrSettings
  , aRateControlMode
  , aSlices
  , aSpatialAdaptiveQuantization
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization as Types
import qualified Network.AWS.MediaConvert.Types.Av1FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.Av1QvbrSettings as Types
import qualified Network.AWS.MediaConvert.Types.Av1RateControlMode as Types
import qualified Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set Codec, under VideoDescription>CodecSettings to the value AV1.
--
-- /See:/ 'mkAv1Settings' smart constructor.
data Av1Settings = Av1Settings'
  { adaptiveQuantization :: Core.Maybe Types.Av1AdaptiveQuantization
    -- ^ Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
  , framerateControl :: Core.Maybe Types.Av1FramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.Av1FramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , gopSize :: Core.Maybe Core.Double
    -- ^ Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
  , maxBitrate :: Core.Maybe Core.Natural
    -- ^ Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
  , numberBFramesBetweenReferenceFrames :: Core.Maybe Core.Natural
    -- ^ Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
  , qvbrSettings :: Core.Maybe Types.Av1QvbrSettings
    -- ^ Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
  , rateControlMode :: Core.Maybe Types.Av1RateControlMode
    -- ^ 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
  , slices :: Core.Maybe Core.Natural
    -- ^ Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
  , spatialAdaptiveQuantization :: Core.Maybe Types.Av1SpatialAdaptiveQuantization
    -- ^ Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Av1Settings' value with any optional fields omitted.
mkAv1Settings
    :: Av1Settings
mkAv1Settings
  = Av1Settings'{adaptiveQuantization = Core.Nothing,
                 framerateControl = Core.Nothing,
                 framerateConversionAlgorithm = Core.Nothing,
                 framerateDenominator = Core.Nothing,
                 framerateNumerator = Core.Nothing, gopSize = Core.Nothing,
                 maxBitrate = Core.Nothing,
                 numberBFramesBetweenReferenceFrames = Core.Nothing,
                 qvbrSettings = Core.Nothing, rateControlMode = Core.Nothing,
                 slices = Core.Nothing, spatialAdaptiveQuantization = Core.Nothing}

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAdaptiveQuantization :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1AdaptiveQuantization)
aAdaptiveQuantization = Lens.field @"adaptiveQuantization"
{-# INLINEABLE aAdaptiveQuantization #-}
{-# DEPRECATED adaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFramerateControl :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1FramerateControl)
aFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE aFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFramerateConversionAlgorithm :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1FramerateConversionAlgorithm)
aFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE aFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFramerateDenominator :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
aFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE aFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFramerateNumerator :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
aFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE aFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Specify the GOP length (keyframe interval) in frames. With AV1, MediaConvert doesn't support GOP length in seconds. This value must be greater than zero and preferably equal to 1 + ((numberBFrames + 1) * x), where x is an integer value.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aGopSize :: Lens.Lens' Av1Settings (Core.Maybe Core.Double)
aGopSize = Lens.field @"gopSize"
{-# INLINEABLE aGopSize #-}
{-# DEPRECATED gopSize "Use generic-lens or generic-optics with 'gopSize' instead"  #-}

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMaxBitrate :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
aMaxBitrate = Lens.field @"maxBitrate"
{-# INLINEABLE aMaxBitrate #-}
{-# DEPRECATED maxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead"  #-}

-- | Specify the number of B-frames. With AV1, MediaConvert supports only 7 or 15.
--
-- /Note:/ Consider using 'numberBFramesBetweenReferenceFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNumberBFramesBetweenReferenceFrames :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
aNumberBFramesBetweenReferenceFrames = Lens.field @"numberBFramesBetweenReferenceFrames"
{-# INLINEABLE aNumberBFramesBetweenReferenceFrames #-}
{-# DEPRECATED numberBFramesBetweenReferenceFrames "Use generic-lens or generic-optics with 'numberBFramesBetweenReferenceFrames' instead"  #-}

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /Note:/ Consider using 'qvbrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aQvbrSettings :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1QvbrSettings)
aQvbrSettings = Lens.field @"qvbrSettings"
{-# INLINEABLE aQvbrSettings #-}
{-# DEPRECATED qvbrSettings "Use generic-lens or generic-optics with 'qvbrSettings' instead"  #-}

-- | 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRateControlMode :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1RateControlMode)
aRateControlMode = Lens.field @"rateControlMode"
{-# INLINEABLE aRateControlMode #-}
{-# DEPRECATED rateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead"  #-}

-- | Specify the number of slices per picture. This value must be 1, 2, 4, 8, 16, or 32. For progressive pictures, this value must be less than or equal to the number of macroblock rows. For interlaced pictures, this value must be less than or equal to half the number of macroblock rows.
--
-- /Note:/ Consider using 'slices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSlices :: Lens.Lens' Av1Settings (Core.Maybe Core.Natural)
aSlices = Lens.field @"slices"
{-# INLINEABLE aSlices #-}
{-# DEPRECATED slices "Use generic-lens or generic-optics with 'slices' instead"  #-}

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
--
-- /Note:/ Consider using 'spatialAdaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSpatialAdaptiveQuantization :: Lens.Lens' Av1Settings (Core.Maybe Types.Av1SpatialAdaptiveQuantization)
aSpatialAdaptiveQuantization = Lens.field @"spatialAdaptiveQuantization"
{-# INLINEABLE aSpatialAdaptiveQuantization #-}
{-# DEPRECATED spatialAdaptiveQuantization "Use generic-lens or generic-optics with 'spatialAdaptiveQuantization' instead"  #-}

instance Core.FromJSON Av1Settings where
        toJSON Av1Settings{..}
          = Core.object
              (Core.catMaybes
                 [("adaptiveQuantization" Core..=) Core.<$> adaptiveQuantization,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateConversionAlgorithm" Core..=) Core.<$>
                    framerateConversionAlgorithm,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("gopSize" Core..=) Core.<$> gopSize,
                  ("maxBitrate" Core..=) Core.<$> maxBitrate,
                  ("numberBFramesBetweenReferenceFrames" Core..=) Core.<$>
                    numberBFramesBetweenReferenceFrames,
                  ("qvbrSettings" Core..=) Core.<$> qvbrSettings,
                  ("rateControlMode" Core..=) Core.<$> rateControlMode,
                  ("slices" Core..=) Core.<$> slices,
                  ("spatialAdaptiveQuantization" Core..=) Core.<$>
                    spatialAdaptiveQuantization])

instance Core.FromJSON Av1Settings where
        parseJSON
          = Core.withObject "Av1Settings" Core.$
              \ x ->
                Av1Settings' Core.<$>
                  (x Core..:? "adaptiveQuantization") Core.<*>
                    x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateConversionAlgorithm"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "gopSize"
                    Core.<*> x Core..:? "maxBitrate"
                    Core.<*> x Core..:? "numberBFramesBetweenReferenceFrames"
                    Core.<*> x Core..:? "qvbrSettings"
                    Core.<*> x Core..:? "rateControlMode"
                    Core.<*> x Core..:? "slices"
                    Core.<*> x Core..:? "spatialAdaptiveQuantization"

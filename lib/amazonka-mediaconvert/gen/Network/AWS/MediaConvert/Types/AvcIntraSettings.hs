{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AvcIntraSettings
  ( AvcIntraSettings (..)
  -- * Smart constructor
  , mkAvcIntraSettings
  -- * Lenses
  , aisAvcIntraClass
  , aisFramerateControl
  , aisFramerateConversionAlgorithm
  , aisFramerateDenominator
  , aisFramerateNumerator
  , aisInterlaceMode
  , aisSlowPal
  , aisTelecine
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AvcIntraClass as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraFramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraSlowPal as Types
import qualified Network.AWS.MediaConvert.Types.AvcIntraTelecine as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
--
-- /See:/ 'mkAvcIntraSettings' smart constructor.
data AvcIntraSettings = AvcIntraSettings'
  { avcIntraClass :: Core.Maybe Types.AvcIntraClass
    -- ^ Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
  , framerateControl :: Core.Maybe Types.AvcIntraFramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.AvcIntraFramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , interlaceMode :: Core.Maybe Types.AvcIntraInterlaceMode
    -- ^ Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
  , slowPal :: Core.Maybe Types.AvcIntraSlowPal
    -- ^ Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
  , telecine :: Core.Maybe Types.AvcIntraTelecine
    -- ^ When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvcIntraSettings' value with any optional fields omitted.
mkAvcIntraSettings
    :: AvcIntraSettings
mkAvcIntraSettings
  = AvcIntraSettings'{avcIntraClass = Core.Nothing,
                      framerateControl = Core.Nothing,
                      framerateConversionAlgorithm = Core.Nothing,
                      framerateDenominator = Core.Nothing,
                      framerateNumerator = Core.Nothing, interlaceMode = Core.Nothing,
                      slowPal = Core.Nothing, telecine = Core.Nothing}

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
--
-- /Note:/ Consider using 'avcIntraClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisAvcIntraClass :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraClass)
aisAvcIntraClass = Lens.field @"avcIntraClass"
{-# INLINEABLE aisAvcIntraClass #-}
{-# DEPRECATED avcIntraClass "Use generic-lens or generic-optics with 'avcIntraClass' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateControl :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraFramerateControl)
aisFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE aisFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateConversionAlgorithm :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraFramerateConversionAlgorithm)
aisFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE aisFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateDenominator :: Lens.Lens' AvcIntraSettings (Core.Maybe Core.Natural)
aisFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE aisFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateNumerator :: Lens.Lens' AvcIntraSettings (Core.Maybe Core.Natural)
aisFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE aisFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisInterlaceMode :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraInterlaceMode)
aisInterlaceMode = Lens.field @"interlaceMode"
{-# INLINEABLE aisInterlaceMode #-}
{-# DEPRECATED interlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead"  #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisSlowPal :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraSlowPal)
aisSlowPal = Lens.field @"slowPal"
{-# INLINEABLE aisSlowPal #-}
{-# DEPRECATED slowPal "Use generic-lens or generic-optics with 'slowPal' instead"  #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisTelecine :: Lens.Lens' AvcIntraSettings (Core.Maybe Types.AvcIntraTelecine)
aisTelecine = Lens.field @"telecine"
{-# INLINEABLE aisTelecine #-}
{-# DEPRECATED telecine "Use generic-lens or generic-optics with 'telecine' instead"  #-}

instance Core.FromJSON AvcIntraSettings where
        toJSON AvcIntraSettings{..}
          = Core.object
              (Core.catMaybes
                 [("avcIntraClass" Core..=) Core.<$> avcIntraClass,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateConversionAlgorithm" Core..=) Core.<$>
                    framerateConversionAlgorithm,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("interlaceMode" Core..=) Core.<$> interlaceMode,
                  ("slowPal" Core..=) Core.<$> slowPal,
                  ("telecine" Core..=) Core.<$> telecine])

instance Core.FromJSON AvcIntraSettings where
        parseJSON
          = Core.withObject "AvcIntraSettings" Core.$
              \ x ->
                AvcIntraSettings' Core.<$>
                  (x Core..:? "avcIntraClass") Core.<*> x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateConversionAlgorithm"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "interlaceMode"
                    Core.<*> x Core..:? "slowPal"
                    Core.<*> x Core..:? "telecine"

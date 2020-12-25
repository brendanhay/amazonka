{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3Settings
  ( Vc3Settings (..),

    -- * Smart constructor
    mkVc3Settings,

    -- * Lenses
    vsFramerateControl,
    vsFramerateConversionAlgorithm,
    vsFramerateDenominator,
    vsFramerateNumerator,
    vsInterlaceMode,
    vsSlowPal,
    vsTelecine,
    vsVc3Class,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Vc3Class as Types
import qualified Network.AWS.MediaConvert.Types.Vc3FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.Vc3InterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.Vc3SlowPal as Types
import qualified Network.AWS.MediaConvert.Types.Vc3Telecine as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
--
-- /See:/ 'mkVc3Settings' smart constructor.
data Vc3Settings = Vc3Settings'
  { -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Core.Maybe Types.Vc3FramerateControl,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Core.Maybe Types.Vc3FramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Core.Maybe Core.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Core.Maybe Core.Natural,
    -- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
    interlaceMode :: Core.Maybe Types.Vc3InterlaceMode,
    -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Core.Maybe Types.Vc3SlowPal,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
    telecine :: Core.Maybe Types.Vc3Telecine,
    -- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
    vc3Class :: Core.Maybe Types.Vc3Class
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Vc3Settings' value with any optional fields omitted.
mkVc3Settings ::
  Vc3Settings
mkVc3Settings =
  Vc3Settings'
    { framerateControl = Core.Nothing,
      framerateConversionAlgorithm = Core.Nothing,
      framerateDenominator = Core.Nothing,
      framerateNumerator = Core.Nothing,
      interlaceMode = Core.Nothing,
      slowPal = Core.Nothing,
      telecine = Core.Nothing,
      vc3Class = Core.Nothing
    }

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateControl :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3FramerateControl)
vsFramerateControl = Lens.field @"framerateControl"
{-# DEPRECATED vsFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateConversionAlgorithm :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3FramerateConversionAlgorithm)
vsFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# DEPRECATED vsFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateDenominator :: Lens.Lens' Vc3Settings (Core.Maybe Core.Natural)
vsFramerateDenominator = Lens.field @"framerateDenominator"
{-# DEPRECATED vsFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateNumerator :: Lens.Lens' Vc3Settings (Core.Maybe Core.Natural)
vsFramerateNumerator = Lens.field @"framerateNumerator"
{-# DEPRECATED vsFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsInterlaceMode :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3InterlaceMode)
vsInterlaceMode = Lens.field @"interlaceMode"
{-# DEPRECATED vsInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSlowPal :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3SlowPal)
vsSlowPal = Lens.field @"slowPal"
{-# DEPRECATED vsSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsTelecine :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3Telecine)
vsTelecine = Lens.field @"telecine"
{-# DEPRECATED vsTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
--
-- /Note:/ Consider using 'vc3Class' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVc3Class :: Lens.Lens' Vc3Settings (Core.Maybe Types.Vc3Class)
vsVc3Class = Lens.field @"vc3Class"
{-# DEPRECATED vsVc3Class "Use generic-lens or generic-optics with 'vc3Class' instead." #-}

instance Core.FromJSON Vc3Settings where
  toJSON Vc3Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("framerateControl" Core..=) Core.<$> framerateControl,
            ("framerateConversionAlgorithm" Core..=)
              Core.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
            ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
            ("interlaceMode" Core..=) Core.<$> interlaceMode,
            ("slowPal" Core..=) Core.<$> slowPal,
            ("telecine" Core..=) Core.<$> telecine,
            ("vc3Class" Core..=) Core.<$> vc3Class
          ]
      )

instance Core.FromJSON Vc3Settings where
  parseJSON =
    Core.withObject "Vc3Settings" Core.$
      \x ->
        Vc3Settings'
          Core.<$> (x Core..:? "framerateControl")
          Core.<*> (x Core..:? "framerateConversionAlgorithm")
          Core.<*> (x Core..:? "framerateDenominator")
          Core.<*> (x Core..:? "framerateNumerator")
          Core.<*> (x Core..:? "interlaceMode")
          Core.<*> (x Core..:? "slowPal")
          Core.<*> (x Core..:? "telecine")
          Core.<*> (x Core..:? "vc3Class")

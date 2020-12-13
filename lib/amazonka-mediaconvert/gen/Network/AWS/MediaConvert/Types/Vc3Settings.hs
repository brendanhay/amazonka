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
    vssSlowPal,
    vssTelecine,
    vssInterlaceMode,
    vssFramerateDenominator,
    vssVc3Class,
    vssFramerateConversionAlgorithm,
    vssFramerateControl,
    vssFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Vc3Class
import Network.AWS.MediaConvert.Types.Vc3FramerateControl
import Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vc3InterlaceMode
import Network.AWS.MediaConvert.Types.Vc3SlowPal
import Network.AWS.MediaConvert.Types.Vc3Telecine
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VC3
--
-- /See:/ 'mkVc3Settings' smart constructor.
data Vc3Settings = Vc3Settings'
  { -- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
    slowPal :: Lude.Maybe Vc3SlowPal,
    -- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
    telecine :: Lude.Maybe Vc3Telecine,
    -- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
    interlaceMode :: Lude.Maybe Vc3InterlaceMode,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Lude.Maybe Lude.Natural,
    -- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
    vc3Class :: Lude.Maybe Vc3Class,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Lude.Maybe Vc3FramerateConversionAlgorithm,
    -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Lude.Maybe Vc3FramerateControl,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Vc3Settings' with the minimum fields required to make a request.
--
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
-- * 'interlaceMode' - Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'vc3Class' - Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
mkVc3Settings ::
  Vc3Settings
mkVc3Settings =
  Vc3Settings'
    { slowPal = Lude.Nothing,
      telecine = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      vc3Class = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      framerateControl = Lude.Nothing,
      framerateNumerator = Lude.Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssSlowPal :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3SlowPal)
vssSlowPal = Lens.lens (slowPal :: Vc3Settings -> Lude.Maybe Vc3SlowPal) (\s a -> s {slowPal = a} :: Vc3Settings)
{-# DEPRECATED vssSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssTelecine :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3Telecine)
vssTelecine = Lens.lens (telecine :: Vc3Settings -> Lude.Maybe Vc3Telecine) (\s a -> s {telecine = a} :: Vc3Settings)
{-# DEPRECATED vssTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Optional. Choose the scan line type for this output. If you don't specify a value, MediaConvert will create a progressive output.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssInterlaceMode :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3InterlaceMode)
vssInterlaceMode = Lens.lens (interlaceMode :: Vc3Settings -> Lude.Maybe Vc3InterlaceMode) (\s a -> s {interlaceMode = a} :: Vc3Settings)
{-# DEPRECATED vssInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateDenominator :: Lens.Lens' Vc3Settings (Lude.Maybe Lude.Natural)
vssFramerateDenominator = Lens.lens (framerateDenominator :: Vc3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: Vc3Settings)
{-# DEPRECATED vssFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Specify the VC3 class to choose the quality characteristics for this output. VC3 class, together with the settings Framerate (framerateNumerator and framerateDenominator) and Resolution (height and width), determine your output bitrate. For example, say that your video resolution is 1920x1080 and your framerate is 29.97. Then Class 145 (CLASS_145) gives you an output with a bitrate of approximately 145 Mbps and Class 220 (CLASS_220) gives you and output with a bitrate of approximately 220 Mbps. VC3 class also specifies the color bit depth of your output.
--
-- /Note:/ Consider using 'vc3Class' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssVc3Class :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3Class)
vssVc3Class = Lens.lens (vc3Class :: Vc3Settings -> Lude.Maybe Vc3Class) (\s a -> s {vc3Class = a} :: Vc3Settings)
{-# DEPRECATED vssVc3Class "Use generic-lens or generic-optics with 'vc3Class' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateConversionAlgorithm :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3FramerateConversionAlgorithm)
vssFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: Vc3Settings -> Lude.Maybe Vc3FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: Vc3Settings)
{-# DEPRECATED vssFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateControl :: Lens.Lens' Vc3Settings (Lude.Maybe Vc3FramerateControl)
vssFramerateControl = Lens.lens (framerateControl :: Vc3Settings -> Lude.Maybe Vc3FramerateControl) (\s a -> s {framerateControl = a} :: Vc3Settings)
{-# DEPRECATED vssFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateNumerator :: Lens.Lens' Vc3Settings (Lude.Maybe Lude.Natural)
vssFramerateNumerator = Lens.lens (framerateNumerator :: Vc3Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: Vc3Settings)
{-# DEPRECATED vssFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Lude.FromJSON Vc3Settings where
  parseJSON =
    Lude.withObject
      "Vc3Settings"
      ( \x ->
          Vc3Settings'
            Lude.<$> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "vc3Class")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "framerateNumerator")
      )

instance Lude.ToJSON Vc3Settings where
  toJSON Vc3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("slowPal" Lude..=) Lude.<$> slowPal,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("vc3Class" Lude..=) Lude.<$> vc3Class,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator
          ]
      )

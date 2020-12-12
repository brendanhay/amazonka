{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraSettings
  ( AvcIntraSettings (..),

    -- * Smart constructor
    mkAvcIntraSettings,

    -- * Lenses
    aisSlowPal,
    aisTelecine,
    aisInterlaceMode,
    aisAvcIntraClass,
    aisFramerateDenominator,
    aisFramerateConversionAlgorithm,
    aisFramerateControl,
    aisFramerateNumerator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AvcIntraClass
import Network.AWS.MediaConvert.Types.AvcIntraFramerateControl
import Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.AvcIntraInterlaceMode
import Network.AWS.MediaConvert.Types.AvcIntraSlowPal
import Network.AWS.MediaConvert.Types.AvcIntraTelecine
import qualified Network.AWS.Prelude as Lude

-- | Required when you set your output video codec to AVC-Intra. For more information about the AVC-I settings, see the relevant specification. For detailed information about SD and HD in AVC-I, see https://ieeexplore.ieee.org/document/7290936.
--
-- /See:/ 'mkAvcIntraSettings' smart constructor.
data AvcIntraSettings = AvcIntraSettings'
  { slowPal ::
      Lude.Maybe AvcIntraSlowPal,
    telecine :: Lude.Maybe AvcIntraTelecine,
    interlaceMode :: Lude.Maybe AvcIntraInterlaceMode,
    avcIntraClass :: Lude.Maybe AvcIntraClass,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    framerateConversionAlgorithm ::
      Lude.Maybe AvcIntraFramerateConversionAlgorithm,
    framerateControl :: Lude.Maybe AvcIntraFramerateControl,
    framerateNumerator :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvcIntraSettings' with the minimum fields required to make a request.
--
-- * 'avcIntraClass' - Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'interlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
mkAvcIntraSettings ::
  AvcIntraSettings
mkAvcIntraSettings =
  AvcIntraSettings'
    { slowPal = Lude.Nothing,
      telecine = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      avcIntraClass = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      framerateControl = Lude.Nothing,
      framerateNumerator = Lude.Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisSlowPal :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraSlowPal)
aisSlowPal = Lens.lens (slowPal :: AvcIntraSettings -> Lude.Maybe AvcIntraSlowPal) (\s a -> s {slowPal = a} :: AvcIntraSettings)
{-# DEPRECATED aisSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisTelecine :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraTelecine)
aisTelecine = Lens.lens (telecine :: AvcIntraSettings -> Lude.Maybe AvcIntraTelecine) (\s a -> s {telecine = a} :: AvcIntraSettings)
{-# DEPRECATED aisTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisInterlaceMode :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraInterlaceMode)
aisInterlaceMode = Lens.lens (interlaceMode :: AvcIntraSettings -> Lude.Maybe AvcIntraInterlaceMode) (\s a -> s {interlaceMode = a} :: AvcIntraSettings)
{-# DEPRECATED aisInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
--
-- /Note:/ Consider using 'avcIntraClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisAvcIntraClass :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraClass)
aisAvcIntraClass = Lens.lens (avcIntraClass :: AvcIntraSettings -> Lude.Maybe AvcIntraClass) (\s a -> s {avcIntraClass = a} :: AvcIntraSettings)
{-# DEPRECATED aisAvcIntraClass "Use generic-lens or generic-optics with 'avcIntraClass' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateDenominator :: Lens.Lens' AvcIntraSettings (Lude.Maybe Lude.Natural)
aisFramerateDenominator = Lens.lens (framerateDenominator :: AvcIntraSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: AvcIntraSettings)
{-# DEPRECATED aisFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateConversionAlgorithm :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraFramerateConversionAlgorithm)
aisFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: AvcIntraSettings -> Lude.Maybe AvcIntraFramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: AvcIntraSettings)
{-# DEPRECATED aisFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateControl :: Lens.Lens' AvcIntraSettings (Lude.Maybe AvcIntraFramerateControl)
aisFramerateControl = Lens.lens (framerateControl :: AvcIntraSettings -> Lude.Maybe AvcIntraFramerateControl) (\s a -> s {framerateControl = a} :: AvcIntraSettings)
{-# DEPRECATED aisFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aisFramerateNumerator :: Lens.Lens' AvcIntraSettings (Lude.Maybe Lude.Natural)
aisFramerateNumerator = Lens.lens (framerateNumerator :: AvcIntraSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: AvcIntraSettings)
{-# DEPRECATED aisFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

instance Lude.FromJSON AvcIntraSettings where
  parseJSON =
    Lude.withObject
      "AvcIntraSettings"
      ( \x ->
          AvcIntraSettings'
            Lude.<$> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "avcIntraClass")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "framerateNumerator")
      )

instance Lude.ToJSON AvcIntraSettings where
  toJSON AvcIntraSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("slowPal" Lude..=) Lude.<$> slowPal,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("avcIntraClass" Lude..=) Lude.<$> avcIntraClass,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator
          ]
      )

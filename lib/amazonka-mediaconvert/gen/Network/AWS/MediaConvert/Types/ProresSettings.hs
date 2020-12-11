-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresSettings
  ( ProresSettings (..),

    -- * Smart constructor
    mkProresSettings,

    -- * Lenses
    psSlowPal,
    psParNumerator,
    psTelecine,
    psInterlaceMode,
    psParControl,
    psCodecProfile,
    psFramerateDenominator,
    psFramerateConversionAlgorithm,
    psFramerateControl,
    psFramerateNumerator,
    psParDenominator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.ProresCodecProfile
import Network.AWS.MediaConvert.Types.ProresFramerateControl
import Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.ProresInterlaceMode
import Network.AWS.MediaConvert.Types.ProresParControl
import Network.AWS.MediaConvert.Types.ProresSlowPal
import Network.AWS.MediaConvert.Types.ProresTelecine
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /See:/ 'mkProresSettings' smart constructor.
data ProresSettings = ProresSettings'
  { slowPal ::
      Lude.Maybe ProresSlowPal,
    parNumerator :: Lude.Maybe Lude.Natural,
    telecine :: Lude.Maybe ProresTelecine,
    interlaceMode :: Lude.Maybe ProresInterlaceMode,
    parControl :: Lude.Maybe ProresParControl,
    codecProfile :: Lude.Maybe ProresCodecProfile,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    framerateConversionAlgorithm ::
      Lude.Maybe ProresFramerateConversionAlgorithm,
    framerateControl :: Lude.Maybe ProresFramerateControl,
    framerateNumerator :: Lude.Maybe Lude.Natural,
    parDenominator :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProresSettings' with the minimum fields required to make a request.
--
-- * 'codecProfile' - Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'interlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'slowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
-- * 'telecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
mkProresSettings ::
  ProresSettings
mkProresSettings =
  ProresSettings'
    { slowPal = Lude.Nothing,
      parNumerator = Lude.Nothing,
      telecine = Lude.Nothing,
      interlaceMode = Lude.Nothing,
      parControl = Lude.Nothing,
      codecProfile = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      framerateControl = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      parDenominator = Lude.Nothing
    }

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSlowPal :: Lens.Lens' ProresSettings (Lude.Maybe ProresSlowPal)
psSlowPal = Lens.lens (slowPal :: ProresSettings -> Lude.Maybe ProresSlowPal) (\s a -> s {slowPal = a} :: ProresSettings)
{-# DEPRECATED psSlowPal "Use generic-lens or generic-optics with 'slowPal' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParNumerator :: Lens.Lens' ProresSettings (Lude.Maybe Lude.Natural)
psParNumerator = Lens.lens (parNumerator :: ProresSettings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: ProresSettings)
{-# DEPRECATED psParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTelecine :: Lens.Lens' ProresSettings (Lude.Maybe ProresTelecine)
psTelecine = Lens.lens (telecine :: ProresSettings -> Lude.Maybe ProresTelecine) (\s a -> s {telecine = a} :: ProresSettings)
{-# DEPRECATED psTelecine "Use generic-lens or generic-optics with 'telecine' instead." #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psInterlaceMode :: Lens.Lens' ProresSettings (Lude.Maybe ProresInterlaceMode)
psInterlaceMode = Lens.lens (interlaceMode :: ProresSettings -> Lude.Maybe ProresInterlaceMode) (\s a -> s {interlaceMode = a} :: ProresSettings)
{-# DEPRECATED psInterlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParControl :: Lens.Lens' ProresSettings (Lude.Maybe ProresParControl)
psParControl = Lens.lens (parControl :: ProresSettings -> Lude.Maybe ProresParControl) (\s a -> s {parControl = a} :: ProresSettings)
{-# DEPRECATED psParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCodecProfile :: Lens.Lens' ProresSettings (Lude.Maybe ProresCodecProfile)
psCodecProfile = Lens.lens (codecProfile :: ProresSettings -> Lude.Maybe ProresCodecProfile) (\s a -> s {codecProfile = a} :: ProresSettings)
{-# DEPRECATED psCodecProfile "Use generic-lens or generic-optics with 'codecProfile' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateDenominator :: Lens.Lens' ProresSettings (Lude.Maybe Lude.Natural)
psFramerateDenominator = Lens.lens (framerateDenominator :: ProresSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: ProresSettings)
{-# DEPRECATED psFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateConversionAlgorithm :: Lens.Lens' ProresSettings (Lude.Maybe ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: ProresSettings -> Lude.Maybe ProresFramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: ProresSettings)
{-# DEPRECATED psFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateControl :: Lens.Lens' ProresSettings (Lude.Maybe ProresFramerateControl)
psFramerateControl = Lens.lens (framerateControl :: ProresSettings -> Lude.Maybe ProresFramerateControl) (\s a -> s {framerateControl = a} :: ProresSettings)
{-# DEPRECATED psFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateNumerator :: Lens.Lens' ProresSettings (Lude.Maybe Lude.Natural)
psFramerateNumerator = Lens.lens (framerateNumerator :: ProresSettings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: ProresSettings)
{-# DEPRECATED psFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParDenominator :: Lens.Lens' ProresSettings (Lude.Maybe Lude.Natural)
psParDenominator = Lens.lens (parDenominator :: ProresSettings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: ProresSettings)
{-# DEPRECATED psParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

instance Lude.FromJSON ProresSettings where
  parseJSON =
    Lude.withObject
      "ProresSettings"
      ( \x ->
          ProresSettings'
            Lude.<$> (x Lude..:? "slowPal")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "telecine")
            Lude.<*> (x Lude..:? "interlaceMode")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "codecProfile")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "parDenominator")
      )

instance Lude.ToJSON ProresSettings where
  toJSON ProresSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("slowPal" Lude..=) Lude.<$> slowPal,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("telecine" Lude..=) Lude.<$> telecine,
            ("interlaceMode" Lude..=) Lude.<$> interlaceMode,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("codecProfile" Lude..=) Lude.<$> codecProfile,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("parDenominator" Lude..=) Lude.<$> parDenominator
          ]
      )

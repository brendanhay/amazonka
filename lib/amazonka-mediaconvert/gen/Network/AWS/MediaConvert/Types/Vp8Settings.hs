-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8Settings
  ( Vp8Settings (..),

    -- * Smart constructor
    mkVp8Settings,

    -- * Lenses
    vQualityTuningLevel,
    vParNumerator,
    vGopSize,
    vHrdBufferSize,
    vRateControlMode,
    vParControl,
    vBitrate,
    vFramerateDenominator,
    vFramerateConversionAlgorithm,
    vFramerateControl,
    vFramerateNumerator,
    vMaxBitrate,
    vParDenominator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Vp8FramerateControl
import Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp8ParControl
import Network.AWS.MediaConvert.Types.Vp8QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp8RateControlMode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP8.
--
-- /See:/ 'mkVp8Settings' smart constructor.
data Vp8Settings = Vp8Settings'
  { qualityTuningLevel ::
      Lude.Maybe Vp8QualityTuningLevel,
    parNumerator :: Lude.Maybe Lude.Natural,
    gopSize :: Lude.Maybe Lude.Double,
    hrdBufferSize :: Lude.Maybe Lude.Natural,
    rateControlMode :: Lude.Maybe Vp8RateControlMode,
    parControl :: Lude.Maybe Vp8ParControl,
    bitrate :: Lude.Maybe Lude.Natural,
    framerateDenominator :: Lude.Maybe Lude.Natural,
    framerateConversionAlgorithm ::
      Lude.Maybe Vp8FramerateConversionAlgorithm,
    framerateControl :: Lude.Maybe Vp8FramerateControl,
    framerateNumerator :: Lude.Maybe Lude.Natural,
    maxBitrate :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'Vp8Settings' with the minimum fields required to make a request.
--
-- * 'bitrate' - Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'gopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
-- * 'hrdBufferSize' - Optional. Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
-- * 'maxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
-- * 'rateControlMode' - With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
mkVp8Settings ::
  Vp8Settings
mkVp8Settings =
  Vp8Settings'
    { qualityTuningLevel = Lude.Nothing,
      parNumerator = Lude.Nothing,
      gopSize = Lude.Nothing,
      hrdBufferSize = Lude.Nothing,
      rateControlMode = Lude.Nothing,
      parControl = Lude.Nothing,
      bitrate = Lude.Nothing,
      framerateDenominator = Lude.Nothing,
      framerateConversionAlgorithm = Lude.Nothing,
      framerateControl = Lude.Nothing,
      framerateNumerator = Lude.Nothing,
      maxBitrate = Lude.Nothing,
      parDenominator = Lude.Nothing
    }

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vQualityTuningLevel :: Lens.Lens' Vp8Settings (Lude.Maybe Vp8QualityTuningLevel)
vQualityTuningLevel = Lens.lens (qualityTuningLevel :: Vp8Settings -> Lude.Maybe Vp8QualityTuningLevel) (\s a -> s {qualityTuningLevel = a} :: Vp8Settings)
{-# DEPRECATED vQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vParNumerator :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vParNumerator = Lens.lens (parNumerator :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: Vp8Settings)
{-# DEPRECATED vParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vGopSize :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Double)
vGopSize = Lens.lens (gopSize :: Vp8Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: Vp8Settings)
{-# DEPRECATED vGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Optional. Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vHrdBufferSize :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vHrdBufferSize = Lens.lens (hrdBufferSize :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferSize = a} :: Vp8Settings)
{-# DEPRECATED vHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | With the VP8 codec, you can use only the variable bitrate (VBR) rate control mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vRateControlMode :: Lens.Lens' Vp8Settings (Lude.Maybe Vp8RateControlMode)
vRateControlMode = Lens.lens (rateControlMode :: Vp8Settings -> Lude.Maybe Vp8RateControlMode) (\s a -> s {rateControlMode = a} :: Vp8Settings)
{-# DEPRECATED vRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vParControl :: Lens.Lens' Vp8Settings (Lude.Maybe Vp8ParControl)
vParControl = Lens.lens (parControl :: Vp8Settings -> Lude.Maybe Vp8ParControl) (\s a -> s {parControl = a} :: Vp8Settings)
{-# DEPRECATED vParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vBitrate :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vBitrate = Lens.lens (bitrate :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Vp8Settings)
{-# DEPRECATED vBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFramerateDenominator :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vFramerateDenominator = Lens.lens (framerateDenominator :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: Vp8Settings)
{-# DEPRECATED vFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFramerateConversionAlgorithm :: Lens.Lens' Vp8Settings (Lude.Maybe Vp8FramerateConversionAlgorithm)
vFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: Vp8Settings -> Lude.Maybe Vp8FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: Vp8Settings)
{-# DEPRECATED vFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFramerateControl :: Lens.Lens' Vp8Settings (Lude.Maybe Vp8FramerateControl)
vFramerateControl = Lens.lens (framerateControl :: Vp8Settings -> Lude.Maybe Vp8FramerateControl) (\s a -> s {framerateControl = a} :: Vp8Settings)
{-# DEPRECATED vFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFramerateNumerator :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vFramerateNumerator = Lens.lens (framerateNumerator :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: Vp8Settings)
{-# DEPRECATED vFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vMaxBitrate :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vMaxBitrate = Lens.lens (maxBitrate :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: Vp8Settings)
{-# DEPRECATED vMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vParDenominator :: Lens.Lens' Vp8Settings (Lude.Maybe Lude.Natural)
vParDenominator = Lens.lens (parDenominator :: Vp8Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: Vp8Settings)
{-# DEPRECATED vParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

instance Lude.FromJSON Vp8Settings where
  parseJSON =
    Lude.withObject
      "Vp8Settings"
      ( \x ->
          Vp8Settings'
            Lude.<$> (x Lude..:? "qualityTuningLevel")
            Lude.<*> (x Lude..:? "parNumerator")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "hrdBufferSize")
            Lude.<*> (x Lude..:? "rateControlMode")
            Lude.<*> (x Lude..:? "parControl")
            Lude.<*> (x Lude..:? "bitrate")
            Lude.<*> (x Lude..:? "framerateDenominator")
            Lude.<*> (x Lude..:? "framerateConversionAlgorithm")
            Lude.<*> (x Lude..:? "framerateControl")
            Lude.<*> (x Lude..:? "framerateNumerator")
            Lude.<*> (x Lude..:? "maxBitrate")
            Lude.<*> (x Lude..:? "parDenominator")
      )

instance Lude.ToJSON Vp8Settings where
  toJSON Vp8Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("qualityTuningLevel" Lude..=) Lude.<$> qualityTuningLevel,
            ("parNumerator" Lude..=) Lude.<$> parNumerator,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("hrdBufferSize" Lude..=) Lude.<$> hrdBufferSize,
            ("rateControlMode" Lude..=) Lude.<$> rateControlMode,
            ("parControl" Lude..=) Lude.<$> parControl,
            ("bitrate" Lude..=) Lude.<$> bitrate,
            ("framerateDenominator" Lude..=) Lude.<$> framerateDenominator,
            ("framerateConversionAlgorithm" Lude..=)
              Lude.<$> framerateConversionAlgorithm,
            ("framerateControl" Lude..=) Lude.<$> framerateControl,
            ("framerateNumerator" Lude..=) Lude.<$> framerateNumerator,
            ("maxBitrate" Lude..=) Lude.<$> maxBitrate,
            ("parDenominator" Lude..=) Lude.<$> parDenominator
          ]
      )

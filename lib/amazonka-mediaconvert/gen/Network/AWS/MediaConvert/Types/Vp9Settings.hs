{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9Settings
  ( Vp9Settings (..),

    -- * Smart constructor
    mkVp9Settings,

    -- * Lenses
    vsQualityTuningLevel,
    vsParNumerator,
    vsGopSize,
    vsHrdBufferSize,
    vsRateControlMode,
    vsParControl,
    vsBitrate,
    vsFramerateDenominator,
    vsFramerateConversionAlgorithm,
    vsFramerateControl,
    vsFramerateNumerator,
    vsMaxBitrate,
    vsParDenominator,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Vp9FramerateControl
import Network.AWS.MediaConvert.Types.Vp9FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.Vp9ParControl
import Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
import Network.AWS.MediaConvert.Types.Vp9RateControlMode
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
--
-- /See:/ 'mkVp9Settings' smart constructor.
data Vp9Settings = Vp9Settings'
  { -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
    qualityTuningLevel :: Lude.Maybe Vp9QualityTuningLevel,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
    parNumerator :: Lude.Maybe Lude.Natural,
    -- | GOP Length (keyframe interval) in frames. Must be greater than zero.
    gopSize :: Lude.Maybe Lude.Double,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
    hrdBufferSize :: Lude.Maybe Lude.Natural,
    -- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
    rateControlMode :: Lude.Maybe Vp9RateControlMode,
    -- | Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
    parControl :: Lude.Maybe Vp9ParControl,
    -- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
    bitrate :: Lude.Maybe Lude.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Lude.Maybe Lude.Natural,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Lude.Maybe Vp9FramerateConversionAlgorithm,
    -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Lude.Maybe Vp9FramerateControl,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Lude.Maybe Lude.Natural,
    -- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
    maxBitrate :: Lude.Maybe Lude.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
    parDenominator :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Vp9Settings' with the minimum fields required to make a request.
--
-- * 'qualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
-- * 'parNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
-- * 'gopSize' - GOP Length (keyframe interval) in frames. Must be greater than zero.
-- * 'hrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
-- * 'rateControlMode' - With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
-- * 'parControl' - Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
-- * 'bitrate' - Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
-- * 'framerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'framerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
-- * 'framerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
-- * 'framerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
-- * 'maxBitrate' - Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
-- * 'parDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
mkVp9Settings ::
  Vp9Settings
mkVp9Settings =
  Vp9Settings'
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
vsQualityTuningLevel :: Lens.Lens' Vp9Settings (Lude.Maybe Vp9QualityTuningLevel)
vsQualityTuningLevel = Lens.lens (qualityTuningLevel :: Vp9Settings -> Lude.Maybe Vp9QualityTuningLevel) (\s a -> s {qualityTuningLevel = a} :: Vp9Settings)
{-# DEPRECATED vsQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsParNumerator :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsParNumerator = Lens.lens (parNumerator :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parNumerator = a} :: Vp9Settings)
{-# DEPRECATED vsParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsGopSize :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Double)
vsGopSize = Lens.lens (gopSize :: Vp9Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: Vp9Settings)
{-# DEPRECATED vsGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsHrdBufferSize :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsHrdBufferSize = Lens.lens (hrdBufferSize :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {hrdBufferSize = a} :: Vp9Settings)
{-# DEPRECATED vsHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsRateControlMode :: Lens.Lens' Vp9Settings (Lude.Maybe Vp9RateControlMode)
vsRateControlMode = Lens.lens (rateControlMode :: Vp9Settings -> Lude.Maybe Vp9RateControlMode) (\s a -> s {rateControlMode = a} :: Vp9Settings)
{-# DEPRECATED vsRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsParControl :: Lens.Lens' Vp9Settings (Lude.Maybe Vp9ParControl)
vsParControl = Lens.lens (parControl :: Vp9Settings -> Lude.Maybe Vp9ParControl) (\s a -> s {parControl = a} :: Vp9Settings)
{-# DEPRECATED vsParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsBitrate :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsBitrate = Lens.lens (bitrate :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {bitrate = a} :: Vp9Settings)
{-# DEPRECATED vsBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateDenominator :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsFramerateDenominator = Lens.lens (framerateDenominator :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateDenominator = a} :: Vp9Settings)
{-# DEPRECATED vsFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateConversionAlgorithm :: Lens.Lens' Vp9Settings (Lude.Maybe Vp9FramerateConversionAlgorithm)
vsFramerateConversionAlgorithm = Lens.lens (framerateConversionAlgorithm :: Vp9Settings -> Lude.Maybe Vp9FramerateConversionAlgorithm) (\s a -> s {framerateConversionAlgorithm = a} :: Vp9Settings)
{-# DEPRECATED vsFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateControl :: Lens.Lens' Vp9Settings (Lude.Maybe Vp9FramerateControl)
vsFramerateControl = Lens.lens (framerateControl :: Vp9Settings -> Lude.Maybe Vp9FramerateControl) (\s a -> s {framerateControl = a} :: Vp9Settings)
{-# DEPRECATED vsFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsFramerateNumerator :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsFramerateNumerator = Lens.lens (framerateNumerator :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {framerateNumerator = a} :: Vp9Settings)
{-# DEPRECATED vsFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsMaxBitrate :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsMaxBitrate = Lens.lens (maxBitrate :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {maxBitrate = a} :: Vp9Settings)
{-# DEPRECATED vsMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsParDenominator :: Lens.Lens' Vp9Settings (Lude.Maybe Lude.Natural)
vsParDenominator = Lens.lens (parDenominator :: Vp9Settings -> Lude.Maybe Lude.Natural) (\s a -> s {parDenominator = a} :: Vp9Settings)
{-# DEPRECATED vsParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

instance Lude.FromJSON Vp9Settings where
  parseJSON =
    Lude.withObject
      "Vp9Settings"
      ( \x ->
          Vp9Settings'
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

instance Lude.ToJSON Vp9Settings where
  toJSON Vp9Settings' {..} =
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

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
    vssBitrate,
    vssFramerateControl,
    vssFramerateConversionAlgorithm,
    vssFramerateDenominator,
    vssFramerateNumerator,
    vssGopSize,
    vssHrdBufferSize,
    vssMaxBitrate,
    vssParControl,
    vssParDenominator,
    vssParNumerator,
    vssQualityTuningLevel,
    vssRateControlMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Vp9FramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.Vp9FramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.Vp9ParControl as Types
import qualified Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel as Types
import qualified Network.AWS.MediaConvert.Types.Vp9RateControlMode as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value VP9.
--
-- /See:/ 'mkVp9Settings' smart constructor.
data Vp9Settings = Vp9Settings'
  { -- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
    bitrate :: Core.Maybe Core.Natural,
    -- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
    framerateControl :: Core.Maybe Types.Vp9FramerateControl,
    -- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
    framerateConversionAlgorithm :: Core.Maybe Types.Vp9FramerateConversionAlgorithm,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateDenominator :: Core.Maybe Core.Natural,
    -- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
    framerateNumerator :: Core.Maybe Core.Natural,
    -- | GOP Length (keyframe interval) in frames. Must be greater than zero.
    gopSize :: Core.Maybe Core.Double,
    -- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
    hrdBufferSize :: Core.Maybe Core.Natural,
    -- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
    maxBitrate :: Core.Maybe Core.Natural,
    -- | Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
    parControl :: Core.Maybe Types.Vp9ParControl,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
    parDenominator :: Core.Maybe Core.Natural,
    -- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
    parNumerator :: Core.Maybe Core.Natural,
    -- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
    qualityTuningLevel :: Core.Maybe Types.Vp9QualityTuningLevel,
    -- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
    rateControlMode :: Core.Maybe Types.Vp9RateControlMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Vp9Settings' value with any optional fields omitted.
mkVp9Settings ::
  Vp9Settings
mkVp9Settings =
  Vp9Settings'
    { bitrate = Core.Nothing,
      framerateControl = Core.Nothing,
      framerateConversionAlgorithm = Core.Nothing,
      framerateDenominator = Core.Nothing,
      framerateNumerator = Core.Nothing,
      gopSize = Core.Nothing,
      hrdBufferSize = Core.Nothing,
      maxBitrate = Core.Nothing,
      parControl = Core.Nothing,
      parDenominator = Core.Nothing,
      parNumerator = Core.Nothing,
      qualityTuningLevel = Core.Nothing,
      rateControlMode = Core.Nothing
    }

-- | Target bitrate in bits/second. For example, enter five megabits per second as 5000000.
--
-- /Note:/ Consider using 'bitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssBitrate :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssBitrate = Lens.field @"bitrate"
{-# DEPRECATED vssBitrate "Use generic-lens or generic-optics with 'bitrate' instead." #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateControl :: Lens.Lens' Vp9Settings (Core.Maybe Types.Vp9FramerateControl)
vssFramerateControl = Lens.field @"framerateControl"
{-# DEPRECATED vssFramerateControl "Use generic-lens or generic-optics with 'framerateControl' instead." #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateConversionAlgorithm :: Lens.Lens' Vp9Settings (Core.Maybe Types.Vp9FramerateConversionAlgorithm)
vssFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# DEPRECATED vssFramerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateDenominator :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssFramerateDenominator = Lens.field @"framerateDenominator"
{-# DEPRECATED vssFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssFramerateNumerator :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssFramerateNumerator = Lens.field @"framerateNumerator"
{-# DEPRECATED vssFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | GOP Length (keyframe interval) in frames. Must be greater than zero.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssGopSize :: Lens.Lens' Vp9Settings (Core.Maybe Core.Double)
vssGopSize = Lens.field @"gopSize"
{-# DEPRECATED vssGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- /Note:/ Consider using 'hrdBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssHrdBufferSize :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssHrdBufferSize = Lens.field @"hrdBufferSize"
{-# DEPRECATED vssHrdBufferSize "Use generic-lens or generic-optics with 'hrdBufferSize' instead." #-}

-- | Ignore this setting unless you set qualityTuningLevel to MULTI_PASS. Optional. Specify the maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. The default behavior uses twice the target bitrate as the maximum bitrate.
--
-- /Note:/ Consider using 'maxBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssMaxBitrate :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssMaxBitrate = Lens.field @"maxBitrate"
{-# DEPRECATED vssMaxBitrate "Use generic-lens or generic-optics with 'maxBitrate' instead." #-}

-- | Optional. Specify how the service determines the pixel aspect ratio for this output. The default behavior is to use the same pixel aspect ratio as your input video.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssParControl :: Lens.Lens' Vp9Settings (Core.Maybe Types.Vp9ParControl)
vssParControl = Lens.field @"parControl"
{-# DEPRECATED vssParControl "Use generic-lens or generic-optics with 'parControl' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssParDenominator :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssParDenominator = Lens.field @"parDenominator"
{-# DEPRECATED vssParDenominator "Use generic-lens or generic-optics with 'parDenominator' instead." #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssParNumerator :: Lens.Lens' Vp9Settings (Core.Maybe Core.Natural)
vssParNumerator = Lens.field @"parNumerator"
{-# DEPRECATED vssParNumerator "Use generic-lens or generic-optics with 'parNumerator' instead." #-}

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
--
-- /Note:/ Consider using 'qualityTuningLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssQualityTuningLevel :: Lens.Lens' Vp9Settings (Core.Maybe Types.Vp9QualityTuningLevel)
vssQualityTuningLevel = Lens.field @"qualityTuningLevel"
{-# DEPRECATED vssQualityTuningLevel "Use generic-lens or generic-optics with 'qualityTuningLevel' instead." #-}

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
--
-- /Note:/ Consider using 'rateControlMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vssRateControlMode :: Lens.Lens' Vp9Settings (Core.Maybe Types.Vp9RateControlMode)
vssRateControlMode = Lens.field @"rateControlMode"
{-# DEPRECATED vssRateControlMode "Use generic-lens or generic-optics with 'rateControlMode' instead." #-}

instance Core.FromJSON Vp9Settings where
  toJSON Vp9Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("bitrate" Core..=) Core.<$> bitrate,
            ("framerateControl" Core..=) Core.<$> framerateControl,
            ("framerateConversionAlgorithm" Core..=)
              Core.<$> framerateConversionAlgorithm,
            ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
            ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
            ("gopSize" Core..=) Core.<$> gopSize,
            ("hrdBufferSize" Core..=) Core.<$> hrdBufferSize,
            ("maxBitrate" Core..=) Core.<$> maxBitrate,
            ("parControl" Core..=) Core.<$> parControl,
            ("parDenominator" Core..=) Core.<$> parDenominator,
            ("parNumerator" Core..=) Core.<$> parNumerator,
            ("qualityTuningLevel" Core..=) Core.<$> qualityTuningLevel,
            ("rateControlMode" Core..=) Core.<$> rateControlMode
          ]
      )

instance Core.FromJSON Vp9Settings where
  parseJSON =
    Core.withObject "Vp9Settings" Core.$
      \x ->
        Vp9Settings'
          Core.<$> (x Core..:? "bitrate")
          Core.<*> (x Core..:? "framerateControl")
          Core.<*> (x Core..:? "framerateConversionAlgorithm")
          Core.<*> (x Core..:? "framerateDenominator")
          Core.<*> (x Core..:? "framerateNumerator")
          Core.<*> (x Core..:? "gopSize")
          Core.<*> (x Core..:? "hrdBufferSize")
          Core.<*> (x Core..:? "maxBitrate")
          Core.<*> (x Core..:? "parControl")
          Core.<*> (x Core..:? "parDenominator")
          Core.<*> (x Core..:? "parNumerator")
          Core.<*> (x Core..:? "qualityTuningLevel")
          Core.<*> (x Core..:? "rateControlMode")

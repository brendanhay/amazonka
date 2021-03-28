{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresSettings
  ( ProresSettings (..)
  -- * Smart constructor
  , mkProresSettings
  -- * Lenses
  , psCodecProfile
  , psFramerateControl
  , psFramerateConversionAlgorithm
  , psFramerateDenominator
  , psFramerateNumerator
  , psInterlaceMode
  , psParControl
  , psParDenominator
  , psParNumerator
  , psSlowPal
  , psTelecine
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.ProresCodecProfile as Types
import qualified Network.AWS.MediaConvert.Types.ProresFramerateControl as Types
import qualified Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.ProresInterlaceMode as Types
import qualified Network.AWS.MediaConvert.Types.ProresParControl as Types
import qualified Network.AWS.MediaConvert.Types.ProresSlowPal as Types
import qualified Network.AWS.MediaConvert.Types.ProresTelecine as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /See:/ 'mkProresSettings' smart constructor.
data ProresSettings = ProresSettings'
  { codecProfile :: Core.Maybe Types.ProresCodecProfile
    -- ^ Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
  , framerateControl :: Core.Maybe Types.ProresFramerateControl
    -- ^ If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
  , framerateConversionAlgorithm :: Core.Maybe Types.ProresFramerateConversionAlgorithm
    -- ^ Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
  , framerateDenominator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , framerateNumerator :: Core.Maybe Core.Natural
    -- ^ When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
  , interlaceMode :: Core.Maybe Types.ProresInterlaceMode
    -- ^ Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
  , parControl :: Core.Maybe Types.ProresParControl
    -- ^ Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
  , parDenominator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
  , parNumerator :: Core.Maybe Core.Natural
    -- ^ Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
  , slowPal :: Core.Maybe Types.ProresSlowPal
    -- ^ Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
  , telecine :: Core.Maybe Types.ProresTelecine
    -- ^ When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProresSettings' value with any optional fields omitted.
mkProresSettings
    :: ProresSettings
mkProresSettings
  = ProresSettings'{codecProfile = Core.Nothing,
                    framerateControl = Core.Nothing,
                    framerateConversionAlgorithm = Core.Nothing,
                    framerateDenominator = Core.Nothing,
                    framerateNumerator = Core.Nothing, interlaceMode = Core.Nothing,
                    parControl = Core.Nothing, parDenominator = Core.Nothing,
                    parNumerator = Core.Nothing, slowPal = Core.Nothing,
                    telecine = Core.Nothing}

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
--
-- /Note:/ Consider using 'codecProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCodecProfile :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresCodecProfile)
psCodecProfile = Lens.field @"codecProfile"
{-# INLINEABLE psCodecProfile #-}
{-# DEPRECATED codecProfile "Use generic-lens or generic-optics with 'codecProfile' instead"  #-}

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- /Note:/ Consider using 'framerateControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateControl :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresFramerateControl)
psFramerateControl = Lens.field @"framerateControl"
{-# INLINEABLE psFramerateControl #-}
{-# DEPRECATED framerateControl "Use generic-lens or generic-optics with 'framerateControl' instead"  #-}

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- /Note:/ Consider using 'framerateConversionAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateConversionAlgorithm :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = Lens.field @"framerateConversionAlgorithm"
{-# INLINEABLE psFramerateConversionAlgorithm #-}
{-# DEPRECATED framerateConversionAlgorithm "Use generic-lens or generic-optics with 'framerateConversionAlgorithm' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateDenominator :: Lens.Lens' ProresSettings (Core.Maybe Core.Natural)
psFramerateDenominator = Lens.field @"framerateDenominator"
{-# INLINEABLE psFramerateDenominator #-}
{-# DEPRECATED framerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead"  #-}

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psFramerateNumerator :: Lens.Lens' ProresSettings (Core.Maybe Core.Natural)
psFramerateNumerator = Lens.field @"framerateNumerator"
{-# INLINEABLE psFramerateNumerator #-}
{-# DEPRECATED framerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead"  #-}

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- /Note:/ Consider using 'interlaceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psInterlaceMode :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresInterlaceMode)
psInterlaceMode = Lens.field @"interlaceMode"
{-# INLINEABLE psInterlaceMode #-}
{-# DEPRECATED interlaceMode "Use generic-lens or generic-optics with 'interlaceMode' instead"  #-}

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- /Note:/ Consider using 'parControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParControl :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresParControl)
psParControl = Lens.field @"parControl"
{-# INLINEABLE psParControl #-}
{-# DEPRECATED parControl "Use generic-lens or generic-optics with 'parControl' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- /Note:/ Consider using 'parDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParDenominator :: Lens.Lens' ProresSettings (Core.Maybe Core.Natural)
psParDenominator = Lens.field @"parDenominator"
{-# INLINEABLE psParDenominator #-}
{-# DEPRECATED parDenominator "Use generic-lens or generic-optics with 'parDenominator' instead"  #-}

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- /Note:/ Consider using 'parNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psParNumerator :: Lens.Lens' ProresSettings (Core.Maybe Core.Natural)
psParNumerator = Lens.field @"parNumerator"
{-# INLINEABLE psParNumerator #-}
{-# DEPRECATED parNumerator "Use generic-lens or generic-optics with 'parNumerator' instead"  #-}

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- /Note:/ Consider using 'slowPal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSlowPal :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresSlowPal)
psSlowPal = Lens.field @"slowPal"
{-# INLINEABLE psSlowPal #-}
{-# DEPRECATED slowPal "Use generic-lens or generic-optics with 'slowPal' instead"  #-}

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard telecine (HARD) to create a smoother picture. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- /Note:/ Consider using 'telecine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psTelecine :: Lens.Lens' ProresSettings (Core.Maybe Types.ProresTelecine)
psTelecine = Lens.field @"telecine"
{-# INLINEABLE psTelecine #-}
{-# DEPRECATED telecine "Use generic-lens or generic-optics with 'telecine' instead"  #-}

instance Core.FromJSON ProresSettings where
        toJSON ProresSettings{..}
          = Core.object
              (Core.catMaybes
                 [("codecProfile" Core..=) Core.<$> codecProfile,
                  ("framerateControl" Core..=) Core.<$> framerateControl,
                  ("framerateConversionAlgorithm" Core..=) Core.<$>
                    framerateConversionAlgorithm,
                  ("framerateDenominator" Core..=) Core.<$> framerateDenominator,
                  ("framerateNumerator" Core..=) Core.<$> framerateNumerator,
                  ("interlaceMode" Core..=) Core.<$> interlaceMode,
                  ("parControl" Core..=) Core.<$> parControl,
                  ("parDenominator" Core..=) Core.<$> parDenominator,
                  ("parNumerator" Core..=) Core.<$> parNumerator,
                  ("slowPal" Core..=) Core.<$> slowPal,
                  ("telecine" Core..=) Core.<$> telecine])

instance Core.FromJSON ProresSettings where
        parseJSON
          = Core.withObject "ProresSettings" Core.$
              \ x ->
                ProresSettings' Core.<$>
                  (x Core..:? "codecProfile") Core.<*> x Core..:? "framerateControl"
                    Core.<*> x Core..:? "framerateConversionAlgorithm"
                    Core.<*> x Core..:? "framerateDenominator"
                    Core.<*> x Core..:? "framerateNumerator"
                    Core.<*> x Core..:? "interlaceMode"
                    Core.<*> x Core..:? "parControl"
                    Core.<*> x Core..:? "parDenominator"
                    Core.<*> x Core..:? "parNumerator"
                    Core.<*> x Core..:? "slowPal"
                    Core.<*> x Core..:? "telecine"

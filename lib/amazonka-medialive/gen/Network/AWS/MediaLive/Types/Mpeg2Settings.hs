{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2Settings
  ( Mpeg2Settings (..),

    -- * Smart constructor
    mkMpeg2Settings,

    -- * Lenses
    msScanType,
    msTimecodeInsertion,
    msAfdSignaling,
    msGopSize,
    msGopSizeUnits,
    msSubgopLength,
    msDisplayAspectRatio,
    msGopNumBFrames,
    msFixedAfd,
    msFilterSettings,
    msFramerateDenominator,
    msColorMetadata,
    msAdaptiveQuantization,
    msFramerateNumerator,
    msGopClosedCadence,
    msColorSpace,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.Mpeg2AdaptiveQuantization
import Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
import Network.AWS.MediaLive.Types.Mpeg2ColorSpace
import Network.AWS.MediaLive.Types.Mpeg2DisplayRatio
import Network.AWS.MediaLive.Types.Mpeg2FilterSettings
import Network.AWS.MediaLive.Types.Mpeg2GopSizeUnits
import Network.AWS.MediaLive.Types.Mpeg2ScanType
import Network.AWS.MediaLive.Types.Mpeg2SubGopLength
import Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Settings
--
-- /See:/ 'mkMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { -- | Set the scan type of the output to PROGRESSIVE or INTERLACED (top field first).
    scanType :: Lude.Maybe Mpeg2ScanType,
    -- | Determines how MediaLive inserts timecodes in the output video. For detailed information about setting up the input and the output for a timecode, see the section on \"MediaLive Features - Timecode configuration\" in the MediaLive User Guide.
    --
    -- DISABLED: do not include timecodes.
    -- GOP_TIMECODE: Include timecode metadata in the GOP header.
    timecodeInsertion :: Lude.Maybe Mpeg2TimecodeInsertionBehavior,
    -- | Indicates the AFD values that MediaLive will write into the video encode. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose AUTO.
    --
    -- AUTO: MediaLive will try to preserve the input AFD value (in cases where multiple AFD values are valid).
    -- FIXED: MediaLive will use the value you specify in fixedAFD.
    afdSignaling :: Lude.Maybe AfdSignaling,
    -- | Relates to the GOP structure. The GOP size (keyframe interval) in the units specified in gopSizeUnits. If you do not know what GOP is, use the default.
    --
    -- If gopSizeUnits is frames, then the gopSize must be an integer and must be greater than or equal to 1.
    -- If gopSizeUnits is seconds, the gopSize must be greater than 0, but does not need to be an integer.
    gopSize :: Lude.Maybe Lude.Double,
    -- | Relates to the GOP structure. Specifies whether the gopSize is specified in frames or seconds. If you do not plan to change the default gopSize, leave the default. If you specify SECONDS, MediaLive will internally convert the gop size to a frame count.
    gopSizeUnits :: Lude.Maybe Mpeg2GopSizeUnits,
    -- | Relates to the GOP structure. If you do not know what GOP is, use the default.
    --
    -- FIXED: Set the number of B-frames in each sub-GOP to the value in gopNumBFrames.
    -- DYNAMIC: Let MediaLive optimize the number of B-frames in each sub-GOP, to improve visual quality.
    subgopLength :: Lude.Maybe Mpeg2SubGopLength,
    -- | Sets the pixel aspect ratio for the encode.
    displayAspectRatio :: Lude.Maybe Mpeg2DisplayRatio,
    -- | Relates to the GOP structure. The number of B-frames between reference frames. If you do not know what a B-frame is, use the default.
    gopNumBFrames :: Lude.Maybe Lude.Natural,
    -- | Complete this field only when afdSignaling is set to FIXED. Enter the AFD value (4 bits) to write on all frames of the video encode.
    fixedAfd :: Lude.Maybe FixedAfd,
    -- | Optionally specify a noise reduction filter, which can improve quality of compressed content. If you do not choose a filter, no filter will be applied.
    --
    -- TEMPORAL: This filter is useful for both source content that is noisy (when it has excessive digital artifacts) and source content that is clean.
    -- When the content is noisy, the filter cleans up the source content before the encoding phase, with these two effects: First, it improves the output video quality because the content has been cleaned up. Secondly, it decreases the bandwidth because MediaLive does not waste bits on encoding noise.
    -- When the content is reasonably clean, the filter tends to decrease the bitrate.
    filterSettings :: Lude.Maybe Mpeg2FilterSettings,
    -- | description": "The framerate denominator. For example, 1001. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
    framerateDenominator :: Lude.Natural,
    -- | Specifies whether to include the color space metadata. The metadata describes the color space that applies to the video (the colorSpace field). We recommend that you insert the metadata.
    colorMetadata :: Lude.Maybe Mpeg2ColorMetadata,
    -- | Choose Off to disable adaptive quantization. Or choose another value to enable the quantizer and set its strength. The strengths are: Auto, Off, Low, Medium, High. When you enable this field, MediaLive allows intra-frame quantizers to vary, which might improve visual quality.
    adaptiveQuantization :: Lude.Maybe Mpeg2AdaptiveQuantization,
    -- | The framerate numerator. For example, 24000. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
    framerateNumerator :: Lude.Natural,
    -- | MPEG2: default is open GOP.
    gopClosedCadence :: Lude.Maybe Lude.Natural,
    -- | Choose the type of color space conversion to apply to the output. For detailed information on setting up both the input and the output to obtain the desired color space in the output, see the section on \"MediaLive Features - Video - color space\" in the MediaLive User Guide.
    --
    -- PASSTHROUGH: Keep the color space of the input content - do not convert it.
    -- AUTO:Convert all content that is SD to rec 601, and convert all content that is HD to rec 709.
    colorSpace :: Lude.Maybe Mpeg2ColorSpace
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mpeg2Settings' with the minimum fields required to make a request.
--
-- * 'scanType' - Set the scan type of the output to PROGRESSIVE or INTERLACED (top field first).
-- * 'timecodeInsertion' - Determines how MediaLive inserts timecodes in the output video. For detailed information about setting up the input and the output for a timecode, see the section on \"MediaLive Features - Timecode configuration\" in the MediaLive User Guide.
--
-- DISABLED: do not include timecodes.
-- GOP_TIMECODE: Include timecode metadata in the GOP header.
-- * 'afdSignaling' - Indicates the AFD values that MediaLive will write into the video encode. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose AUTO.
--
-- AUTO: MediaLive will try to preserve the input AFD value (in cases where multiple AFD values are valid).
-- FIXED: MediaLive will use the value you specify in fixedAFD.
-- * 'gopSize' - Relates to the GOP structure. The GOP size (keyframe interval) in the units specified in gopSizeUnits. If you do not know what GOP is, use the default.
--
-- If gopSizeUnits is frames, then the gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, the gopSize must be greater than 0, but does not need to be an integer.
-- * 'gopSizeUnits' - Relates to the GOP structure. Specifies whether the gopSize is specified in frames or seconds. If you do not plan to change the default gopSize, leave the default. If you specify SECONDS, MediaLive will internally convert the gop size to a frame count.
-- * 'subgopLength' - Relates to the GOP structure. If you do not know what GOP is, use the default.
--
-- FIXED: Set the number of B-frames in each sub-GOP to the value in gopNumBFrames.
-- DYNAMIC: Let MediaLive optimize the number of B-frames in each sub-GOP, to improve visual quality.
-- * 'displayAspectRatio' - Sets the pixel aspect ratio for the encode.
-- * 'gopNumBFrames' - Relates to the GOP structure. The number of B-frames between reference frames. If you do not know what a B-frame is, use the default.
-- * 'fixedAfd' - Complete this field only when afdSignaling is set to FIXED. Enter the AFD value (4 bits) to write on all frames of the video encode.
-- * 'filterSettings' - Optionally specify a noise reduction filter, which can improve quality of compressed content. If you do not choose a filter, no filter will be applied.
--
-- TEMPORAL: This filter is useful for both source content that is noisy (when it has excessive digital artifacts) and source content that is clean.
-- When the content is noisy, the filter cleans up the source content before the encoding phase, with these two effects: First, it improves the output video quality because the content has been cleaned up. Secondly, it decreases the bandwidth because MediaLive does not waste bits on encoding noise.
-- When the content is reasonably clean, the filter tends to decrease the bitrate.
-- * 'framerateDenominator' - description": "The framerate denominator. For example, 1001. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
-- * 'colorMetadata' - Specifies whether to include the color space metadata. The metadata describes the color space that applies to the video (the colorSpace field). We recommend that you insert the metadata.
-- * 'adaptiveQuantization' - Choose Off to disable adaptive quantization. Or choose another value to enable the quantizer and set its strength. The strengths are: Auto, Off, Low, Medium, High. When you enable this field, MediaLive allows intra-frame quantizers to vary, which might improve visual quality.
-- * 'framerateNumerator' - The framerate numerator. For example, 24000. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
-- * 'gopClosedCadence' - MPEG2: default is open GOP.
-- * 'colorSpace' - Choose the type of color space conversion to apply to the output. For detailed information on setting up both the input and the output to obtain the desired color space in the output, see the section on \"MediaLive Features - Video - color space\" in the MediaLive User Guide.
--
-- PASSTHROUGH: Keep the color space of the input content - do not convert it.
-- AUTO:Convert all content that is SD to rec 601, and convert all content that is HD to rec 709.
mkMpeg2Settings ::
  -- | 'framerateDenominator'
  Lude.Natural ->
  -- | 'framerateNumerator'
  Lude.Natural ->
  Mpeg2Settings
mkMpeg2Settings pFramerateDenominator_ pFramerateNumerator_ =
  Mpeg2Settings'
    { scanType = Lude.Nothing,
      timecodeInsertion = Lude.Nothing,
      afdSignaling = Lude.Nothing,
      gopSize = Lude.Nothing,
      gopSizeUnits = Lude.Nothing,
      subgopLength = Lude.Nothing,
      displayAspectRatio = Lude.Nothing,
      gopNumBFrames = Lude.Nothing,
      fixedAfd = Lude.Nothing,
      filterSettings = Lude.Nothing,
      framerateDenominator = pFramerateDenominator_,
      colorMetadata = Lude.Nothing,
      adaptiveQuantization = Lude.Nothing,
      framerateNumerator = pFramerateNumerator_,
      gopClosedCadence = Lude.Nothing,
      colorSpace = Lude.Nothing
    }

-- | Set the scan type of the output to PROGRESSIVE or INTERLACED (top field first).
--
-- /Note:/ Consider using 'scanType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msScanType :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2ScanType)
msScanType = Lens.lens (scanType :: Mpeg2Settings -> Lude.Maybe Mpeg2ScanType) (\s a -> s {scanType = a} :: Mpeg2Settings)
{-# DEPRECATED msScanType "Use generic-lens or generic-optics with 'scanType' instead." #-}

-- | Determines how MediaLive inserts timecodes in the output video. For detailed information about setting up the input and the output for a timecode, see the section on \"MediaLive Features - Timecode configuration\" in the MediaLive User Guide.
--
-- DISABLED: do not include timecodes.
-- GOP_TIMECODE: Include timecode metadata in the GOP header.
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTimecodeInsertion :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2TimecodeInsertionBehavior)
msTimecodeInsertion = Lens.lens (timecodeInsertion :: Mpeg2Settings -> Lude.Maybe Mpeg2TimecodeInsertionBehavior) (\s a -> s {timecodeInsertion = a} :: Mpeg2Settings)
{-# DEPRECATED msTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

-- | Indicates the AFD values that MediaLive will write into the video encode. If you do not know what AFD signaling is, or if your downstream system has not given you guidance, choose AUTO.
--
-- AUTO: MediaLive will try to preserve the input AFD value (in cases where multiple AFD values are valid).
-- FIXED: MediaLive will use the value you specify in fixedAFD.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAfdSignaling :: Lens.Lens' Mpeg2Settings (Lude.Maybe AfdSignaling)
msAfdSignaling = Lens.lens (afdSignaling :: Mpeg2Settings -> Lude.Maybe AfdSignaling) (\s a -> s {afdSignaling = a} :: Mpeg2Settings)
{-# DEPRECATED msAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | Relates to the GOP structure. The GOP size (keyframe interval) in the units specified in gopSizeUnits. If you do not know what GOP is, use the default.
--
-- If gopSizeUnits is frames, then the gopSize must be an integer and must be greater than or equal to 1.
-- If gopSizeUnits is seconds, the gopSize must be greater than 0, but does not need to be an integer.
--
-- /Note:/ Consider using 'gopSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSize :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Double)
msGopSize = Lens.lens (gopSize :: Mpeg2Settings -> Lude.Maybe Lude.Double) (\s a -> s {gopSize = a} :: Mpeg2Settings)
{-# DEPRECATED msGopSize "Use generic-lens or generic-optics with 'gopSize' instead." #-}

-- | Relates to the GOP structure. Specifies whether the gopSize is specified in frames or seconds. If you do not plan to change the default gopSize, leave the default. If you specify SECONDS, MediaLive will internally convert the gop size to a frame count.
--
-- /Note:/ Consider using 'gopSizeUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopSizeUnits :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = Lens.lens (gopSizeUnits :: Mpeg2Settings -> Lude.Maybe Mpeg2GopSizeUnits) (\s a -> s {gopSizeUnits = a} :: Mpeg2Settings)
{-# DEPRECATED msGopSizeUnits "Use generic-lens or generic-optics with 'gopSizeUnits' instead." #-}

-- | Relates to the GOP structure. If you do not know what GOP is, use the default.
--
-- FIXED: Set the number of B-frames in each sub-GOP to the value in gopNumBFrames.
-- DYNAMIC: Let MediaLive optimize the number of B-frames in each sub-GOP, to improve visual quality.
--
-- /Note:/ Consider using 'subgopLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msSubgopLength :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2SubGopLength)
msSubgopLength = Lens.lens (subgopLength :: Mpeg2Settings -> Lude.Maybe Mpeg2SubGopLength) (\s a -> s {subgopLength = a} :: Mpeg2Settings)
{-# DEPRECATED msSubgopLength "Use generic-lens or generic-optics with 'subgopLength' instead." #-}

-- | Sets the pixel aspect ratio for the encode.
--
-- /Note:/ Consider using 'displayAspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msDisplayAspectRatio :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2DisplayRatio)
msDisplayAspectRatio = Lens.lens (displayAspectRatio :: Mpeg2Settings -> Lude.Maybe Mpeg2DisplayRatio) (\s a -> s {displayAspectRatio = a} :: Mpeg2Settings)
{-# DEPRECATED msDisplayAspectRatio "Use generic-lens or generic-optics with 'displayAspectRatio' instead." #-}

-- | Relates to the GOP structure. The number of B-frames between reference frames. If you do not know what a B-frame is, use the default.
--
-- /Note:/ Consider using 'gopNumBFrames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopNumBFrames :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msGopNumBFrames = Lens.lens (gopNumBFrames :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopNumBFrames = a} :: Mpeg2Settings)
{-# DEPRECATED msGopNumBFrames "Use generic-lens or generic-optics with 'gopNumBFrames' instead." #-}

-- | Complete this field only when afdSignaling is set to FIXED. Enter the AFD value (4 bits) to write on all frames of the video encode.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFixedAfd :: Lens.Lens' Mpeg2Settings (Lude.Maybe FixedAfd)
msFixedAfd = Lens.lens (fixedAfd :: Mpeg2Settings -> Lude.Maybe FixedAfd) (\s a -> s {fixedAfd = a} :: Mpeg2Settings)
{-# DEPRECATED msFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | Optionally specify a noise reduction filter, which can improve quality of compressed content. If you do not choose a filter, no filter will be applied.
--
-- TEMPORAL: This filter is useful for both source content that is noisy (when it has excessive digital artifacts) and source content that is clean.
-- When the content is noisy, the filter cleans up the source content before the encoding phase, with these two effects: First, it improves the output video quality because the content has been cleaned up. Secondly, it decreases the bandwidth because MediaLive does not waste bits on encoding noise.
-- When the content is reasonably clean, the filter tends to decrease the bitrate.
--
-- /Note:/ Consider using 'filterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFilterSettings :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2FilterSettings)
msFilterSettings = Lens.lens (filterSettings :: Mpeg2Settings -> Lude.Maybe Mpeg2FilterSettings) (\s a -> s {filterSettings = a} :: Mpeg2Settings)
{-# DEPRECATED msFilterSettings "Use generic-lens or generic-optics with 'filterSettings' instead." #-}

-- | description": "The framerate denominator. For example, 1001. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
--
-- /Note:/ Consider using 'framerateDenominator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateDenominator :: Lens.Lens' Mpeg2Settings Lude.Natural
msFramerateDenominator = Lens.lens (framerateDenominator :: Mpeg2Settings -> Lude.Natural) (\s a -> s {framerateDenominator = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateDenominator "Use generic-lens or generic-optics with 'framerateDenominator' instead." #-}

-- | Specifies whether to include the color space metadata. The metadata describes the color space that applies to the video (the colorSpace field). We recommend that you insert the metadata.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msColorMetadata :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2ColorMetadata)
msColorMetadata = Lens.lens (colorMetadata :: Mpeg2Settings -> Lude.Maybe Mpeg2ColorMetadata) (\s a -> s {colorMetadata = a} :: Mpeg2Settings)
{-# DEPRECATED msColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Choose Off to disable adaptive quantization. Or choose another value to enable the quantizer and set its strength. The strengths are: Auto, Off, Low, Medium, High. When you enable this field, MediaLive allows intra-frame quantizers to vary, which might improve visual quality.
--
-- /Note:/ Consider using 'adaptiveQuantization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAdaptiveQuantization :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = Lens.lens (adaptiveQuantization :: Mpeg2Settings -> Lude.Maybe Mpeg2AdaptiveQuantization) (\s a -> s {adaptiveQuantization = a} :: Mpeg2Settings)
{-# DEPRECATED msAdaptiveQuantization "Use generic-lens or generic-optics with 'adaptiveQuantization' instead." #-}

-- | The framerate numerator. For example, 24000. The framerate is the numerator divided by the denominator. For example, 24000 / 1001 = 23.976 FPS.
--
-- /Note:/ Consider using 'framerateNumerator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msFramerateNumerator :: Lens.Lens' Mpeg2Settings Lude.Natural
msFramerateNumerator = Lens.lens (framerateNumerator :: Mpeg2Settings -> Lude.Natural) (\s a -> s {framerateNumerator = a} :: Mpeg2Settings)
{-# DEPRECATED msFramerateNumerator "Use generic-lens or generic-optics with 'framerateNumerator' instead." #-}

-- | MPEG2: default is open GOP.
--
-- /Note:/ Consider using 'gopClosedCadence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msGopClosedCadence :: Lens.Lens' Mpeg2Settings (Lude.Maybe Lude.Natural)
msGopClosedCadence = Lens.lens (gopClosedCadence :: Mpeg2Settings -> Lude.Maybe Lude.Natural) (\s a -> s {gopClosedCadence = a} :: Mpeg2Settings)
{-# DEPRECATED msGopClosedCadence "Use generic-lens or generic-optics with 'gopClosedCadence' instead." #-}

-- | Choose the type of color space conversion to apply to the output. For detailed information on setting up both the input and the output to obtain the desired color space in the output, see the section on \"MediaLive Features - Video - color space\" in the MediaLive User Guide.
--
-- PASSTHROUGH: Keep the color space of the input content - do not convert it.
-- AUTO:Convert all content that is SD to rec 601, and convert all content that is HD to rec 709.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msColorSpace :: Lens.Lens' Mpeg2Settings (Lude.Maybe Mpeg2ColorSpace)
msColorSpace = Lens.lens (colorSpace :: Mpeg2Settings -> Lude.Maybe Mpeg2ColorSpace) (\s a -> s {colorSpace = a} :: Mpeg2Settings)
{-# DEPRECATED msColorSpace "Use generic-lens or generic-optics with 'colorSpace' instead." #-}

instance Lude.FromJSON Mpeg2Settings where
  parseJSON =
    Lude.withObject
      "Mpeg2Settings"
      ( \x ->
          Mpeg2Settings'
            Lude.<$> (x Lude..:? "scanType")
            Lude.<*> (x Lude..:? "timecodeInsertion")
            Lude.<*> (x Lude..:? "afdSignaling")
            Lude.<*> (x Lude..:? "gopSize")
            Lude.<*> (x Lude..:? "gopSizeUnits")
            Lude.<*> (x Lude..:? "subgopLength")
            Lude.<*> (x Lude..:? "displayAspectRatio")
            Lude.<*> (x Lude..:? "gopNumBFrames")
            Lude.<*> (x Lude..:? "fixedAfd")
            Lude.<*> (x Lude..:? "filterSettings")
            Lude.<*> (x Lude..: "framerateDenominator")
            Lude.<*> (x Lude..:? "colorMetadata")
            Lude.<*> (x Lude..:? "adaptiveQuantization")
            Lude.<*> (x Lude..: "framerateNumerator")
            Lude.<*> (x Lude..:? "gopClosedCadence")
            Lude.<*> (x Lude..:? "colorSpace")
      )

instance Lude.ToJSON Mpeg2Settings where
  toJSON Mpeg2Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scanType" Lude..=) Lude.<$> scanType,
            ("timecodeInsertion" Lude..=) Lude.<$> timecodeInsertion,
            ("afdSignaling" Lude..=) Lude.<$> afdSignaling,
            ("gopSize" Lude..=) Lude.<$> gopSize,
            ("gopSizeUnits" Lude..=) Lude.<$> gopSizeUnits,
            ("subgopLength" Lude..=) Lude.<$> subgopLength,
            ("displayAspectRatio" Lude..=) Lude.<$> displayAspectRatio,
            ("gopNumBFrames" Lude..=) Lude.<$> gopNumBFrames,
            ("fixedAfd" Lude..=) Lude.<$> fixedAfd,
            ("filterSettings" Lude..=) Lude.<$> filterSettings,
            Lude.Just ("framerateDenominator" Lude..= framerateDenominator),
            ("colorMetadata" Lude..=) Lude.<$> colorMetadata,
            ("adaptiveQuantization" Lude..=) Lude.<$> adaptiveQuantization,
            Lude.Just ("framerateNumerator" Lude..= framerateNumerator),
            ("gopClosedCadence" Lude..=) Lude.<$> gopClosedCadence,
            ("colorSpace" Lude..=) Lude.<$> colorSpace
          ]
      )

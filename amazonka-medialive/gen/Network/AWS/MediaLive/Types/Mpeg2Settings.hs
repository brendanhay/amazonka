{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2Settings where

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Prelude as Prelude

-- | Mpeg2 Settings
--
-- /See:/ 'newMpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { -- | Set the scan type of the output to PROGRESSIVE or INTERLACED (top field
    -- first).
    scanType :: Prelude.Maybe Mpeg2ScanType,
    -- | Sets the pixel aspect ratio for the encode.
    displayAspectRatio :: Prelude.Maybe Mpeg2DisplayRatio,
    -- | Specifies whether to include the color space metadata. The metadata
    -- describes the color space that applies to the video (the colorSpace
    -- field). We recommend that you insert the metadata.
    colorMetadata :: Prelude.Maybe Mpeg2ColorMetadata,
    -- | Relates to the GOP structure. If you do not know what GOP is, use the
    -- default. FIXED: Set the number of B-frames in each sub-GOP to the value
    -- in gopNumBFrames. DYNAMIC: Let MediaLive optimize the number of B-frames
    -- in each sub-GOP, to improve visual quality.
    subgopLength :: Prelude.Maybe Mpeg2SubGopLength,
    -- | Relates to the GOP structure. Specifies whether the gopSize is specified
    -- in frames or seconds. If you do not plan to change the default gopSize,
    -- leave the default. If you specify SECONDS, MediaLive will internally
    -- convert the gop size to a frame count.
    gopSizeUnits :: Prelude.Maybe Mpeg2GopSizeUnits,
    -- | Relates to the GOP structure. The GOP size (keyframe interval) in the
    -- units specified in gopSizeUnits. If you do not know what GOP is, use the
    -- default. If gopSizeUnits is frames, then the gopSize must be an integer
    -- and must be greater than or equal to 1. If gopSizeUnits is seconds, the
    -- gopSize must be greater than 0, but does not need to be an integer.
    gopSize :: Prelude.Maybe Prelude.Double,
    -- | Complete this field only when afdSignaling is set to FIXED. Enter the
    -- AFD value (4 bits) to write on all frames of the video encode.
    fixedAfd :: Prelude.Maybe FixedAfd,
    -- | Optionally specify a noise reduction filter, which can improve quality
    -- of compressed content. If you do not choose a filter, no filter will be
    -- applied. TEMPORAL: This filter is useful for both source content that is
    -- noisy (when it has excessive digital artifacts) and source content that
    -- is clean. When the content is noisy, the filter cleans up the source
    -- content before the encoding phase, with these two effects: First, it
    -- improves the output video quality because the content has been cleaned
    -- up. Secondly, it decreases the bandwidth because MediaLive does not
    -- waste bits on encoding noise. When the content is reasonably clean, the
    -- filter tends to decrease the bitrate.
    filterSettings :: Prelude.Maybe Mpeg2FilterSettings,
    -- | Relates to the GOP structure. The number of B-frames between reference
    -- frames. If you do not know what a B-frame is, use the default.
    gopNumBFrames :: Prelude.Maybe Prelude.Natural,
    -- | Determines how MediaLive inserts timecodes in the output video. For
    -- detailed information about setting up the input and the output for a
    -- timecode, see the section on \\\"MediaLive Features - Timecode
    -- configuration\\\" in the MediaLive User Guide. DISABLED: do not include
    -- timecodes. GOP_TIMECODE: Include timecode metadata in the GOP header.
    timecodeInsertion :: Prelude.Maybe Mpeg2TimecodeInsertionBehavior,
    -- | MPEG2: default is open GOP.
    gopClosedCadence :: Prelude.Maybe Prelude.Natural,
    -- | Choose the type of color space conversion to apply to the output. For
    -- detailed information on setting up both the input and the output to
    -- obtain the desired color space in the output, see the section on
    -- \\\"MediaLive Features - Video - color space\\\" in the MediaLive User
    -- Guide. PASSTHROUGH: Keep the color space of the input content - do not
    -- convert it. AUTO:Convert all content that is SD to rec 601, and convert
    -- all content that is HD to rec 709.
    colorSpace :: Prelude.Maybe Mpeg2ColorSpace,
    -- | Choose Off to disable adaptive quantization. Or choose another value to
    -- enable the quantizer and set its strength. The strengths are: Auto, Off,
    -- Low, Medium, High. When you enable this field, MediaLive allows
    -- intra-frame quantizers to vary, which might improve visual quality.
    adaptiveQuantization :: Prelude.Maybe Mpeg2AdaptiveQuantization,
    -- | Indicates the AFD values that MediaLive will write into the video
    -- encode. If you do not know what AFD signaling is, or if your downstream
    -- system has not given you guidance, choose AUTO. AUTO: MediaLive will try
    -- to preserve the input AFD value (in cases where multiple AFD values are
    -- valid). FIXED: MediaLive will use the value you specify in fixedAFD.
    afdSignaling :: Prelude.Maybe AfdSignaling,
    -- | The framerate numerator. For example, 24000. The framerate is the
    -- numerator divided by the denominator. For example, 24000 \/ 1001 =
    -- 23.976 FPS.
    framerateNumerator :: Prelude.Natural,
    -- | description\": \"The framerate denominator. For example, 1001. The
    -- framerate is the numerator divided by the denominator. For example,
    -- 24000 \/ 1001 = 23.976 FPS.
    framerateDenominator :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mpeg2Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanType', 'mpeg2Settings_scanType' - Set the scan type of the output to PROGRESSIVE or INTERLACED (top field
-- first).
--
-- 'displayAspectRatio', 'mpeg2Settings_displayAspectRatio' - Sets the pixel aspect ratio for the encode.
--
-- 'colorMetadata', 'mpeg2Settings_colorMetadata' - Specifies whether to include the color space metadata. The metadata
-- describes the color space that applies to the video (the colorSpace
-- field). We recommend that you insert the metadata.
--
-- 'subgopLength', 'mpeg2Settings_subgopLength' - Relates to the GOP structure. If you do not know what GOP is, use the
-- default. FIXED: Set the number of B-frames in each sub-GOP to the value
-- in gopNumBFrames. DYNAMIC: Let MediaLive optimize the number of B-frames
-- in each sub-GOP, to improve visual quality.
--
-- 'gopSizeUnits', 'mpeg2Settings_gopSizeUnits' - Relates to the GOP structure. Specifies whether the gopSize is specified
-- in frames or seconds. If you do not plan to change the default gopSize,
-- leave the default. If you specify SECONDS, MediaLive will internally
-- convert the gop size to a frame count.
--
-- 'gopSize', 'mpeg2Settings_gopSize' - Relates to the GOP structure. The GOP size (keyframe interval) in the
-- units specified in gopSizeUnits. If you do not know what GOP is, use the
-- default. If gopSizeUnits is frames, then the gopSize must be an integer
-- and must be greater than or equal to 1. If gopSizeUnits is seconds, the
-- gopSize must be greater than 0, but does not need to be an integer.
--
-- 'fixedAfd', 'mpeg2Settings_fixedAfd' - Complete this field only when afdSignaling is set to FIXED. Enter the
-- AFD value (4 bits) to write on all frames of the video encode.
--
-- 'filterSettings', 'mpeg2Settings_filterSettings' - Optionally specify a noise reduction filter, which can improve quality
-- of compressed content. If you do not choose a filter, no filter will be
-- applied. TEMPORAL: This filter is useful for both source content that is
-- noisy (when it has excessive digital artifacts) and source content that
-- is clean. When the content is noisy, the filter cleans up the source
-- content before the encoding phase, with these two effects: First, it
-- improves the output video quality because the content has been cleaned
-- up. Secondly, it decreases the bandwidth because MediaLive does not
-- waste bits on encoding noise. When the content is reasonably clean, the
-- filter tends to decrease the bitrate.
--
-- 'gopNumBFrames', 'mpeg2Settings_gopNumBFrames' - Relates to the GOP structure. The number of B-frames between reference
-- frames. If you do not know what a B-frame is, use the default.
--
-- 'timecodeInsertion', 'mpeg2Settings_timecodeInsertion' - Determines how MediaLive inserts timecodes in the output video. For
-- detailed information about setting up the input and the output for a
-- timecode, see the section on \\\"MediaLive Features - Timecode
-- configuration\\\" in the MediaLive User Guide. DISABLED: do not include
-- timecodes. GOP_TIMECODE: Include timecode metadata in the GOP header.
--
-- 'gopClosedCadence', 'mpeg2Settings_gopClosedCadence' - MPEG2: default is open GOP.
--
-- 'colorSpace', 'mpeg2Settings_colorSpace' - Choose the type of color space conversion to apply to the output. For
-- detailed information on setting up both the input and the output to
-- obtain the desired color space in the output, see the section on
-- \\\"MediaLive Features - Video - color space\\\" in the MediaLive User
-- Guide. PASSTHROUGH: Keep the color space of the input content - do not
-- convert it. AUTO:Convert all content that is SD to rec 601, and convert
-- all content that is HD to rec 709.
--
-- 'adaptiveQuantization', 'mpeg2Settings_adaptiveQuantization' - Choose Off to disable adaptive quantization. Or choose another value to
-- enable the quantizer and set its strength. The strengths are: Auto, Off,
-- Low, Medium, High. When you enable this field, MediaLive allows
-- intra-frame quantizers to vary, which might improve visual quality.
--
-- 'afdSignaling', 'mpeg2Settings_afdSignaling' - Indicates the AFD values that MediaLive will write into the video
-- encode. If you do not know what AFD signaling is, or if your downstream
-- system has not given you guidance, choose AUTO. AUTO: MediaLive will try
-- to preserve the input AFD value (in cases where multiple AFD values are
-- valid). FIXED: MediaLive will use the value you specify in fixedAFD.
--
-- 'framerateNumerator', 'mpeg2Settings_framerateNumerator' - The framerate numerator. For example, 24000. The framerate is the
-- numerator divided by the denominator. For example, 24000 \/ 1001 =
-- 23.976 FPS.
--
-- 'framerateDenominator', 'mpeg2Settings_framerateDenominator' - description\": \"The framerate denominator. For example, 1001. The
-- framerate is the numerator divided by the denominator. For example,
-- 24000 \/ 1001 = 23.976 FPS.
newMpeg2Settings ::
  -- | 'framerateNumerator'
  Prelude.Natural ->
  -- | 'framerateDenominator'
  Prelude.Natural ->
  Mpeg2Settings
newMpeg2Settings
  pFramerateNumerator_
  pFramerateDenominator_ =
    Mpeg2Settings'
      { scanType = Prelude.Nothing,
        displayAspectRatio = Prelude.Nothing,
        colorMetadata = Prelude.Nothing,
        subgopLength = Prelude.Nothing,
        gopSizeUnits = Prelude.Nothing,
        gopSize = Prelude.Nothing,
        fixedAfd = Prelude.Nothing,
        filterSettings = Prelude.Nothing,
        gopNumBFrames = Prelude.Nothing,
        timecodeInsertion = Prelude.Nothing,
        gopClosedCadence = Prelude.Nothing,
        colorSpace = Prelude.Nothing,
        adaptiveQuantization = Prelude.Nothing,
        afdSignaling = Prelude.Nothing,
        framerateNumerator = pFramerateNumerator_,
        framerateDenominator = pFramerateDenominator_
      }

-- | Set the scan type of the output to PROGRESSIVE or INTERLACED (top field
-- first).
mpeg2Settings_scanType :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2ScanType)
mpeg2Settings_scanType = Lens.lens (\Mpeg2Settings' {scanType} -> scanType) (\s@Mpeg2Settings' {} a -> s {scanType = a} :: Mpeg2Settings)

-- | Sets the pixel aspect ratio for the encode.
mpeg2Settings_displayAspectRatio :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2DisplayRatio)
mpeg2Settings_displayAspectRatio = Lens.lens (\Mpeg2Settings' {displayAspectRatio} -> displayAspectRatio) (\s@Mpeg2Settings' {} a -> s {displayAspectRatio = a} :: Mpeg2Settings)

-- | Specifies whether to include the color space metadata. The metadata
-- describes the color space that applies to the video (the colorSpace
-- field). We recommend that you insert the metadata.
mpeg2Settings_colorMetadata :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2ColorMetadata)
mpeg2Settings_colorMetadata = Lens.lens (\Mpeg2Settings' {colorMetadata} -> colorMetadata) (\s@Mpeg2Settings' {} a -> s {colorMetadata = a} :: Mpeg2Settings)

-- | Relates to the GOP structure. If you do not know what GOP is, use the
-- default. FIXED: Set the number of B-frames in each sub-GOP to the value
-- in gopNumBFrames. DYNAMIC: Let MediaLive optimize the number of B-frames
-- in each sub-GOP, to improve visual quality.
mpeg2Settings_subgopLength :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2SubGopLength)
mpeg2Settings_subgopLength = Lens.lens (\Mpeg2Settings' {subgopLength} -> subgopLength) (\s@Mpeg2Settings' {} a -> s {subgopLength = a} :: Mpeg2Settings)

-- | Relates to the GOP structure. Specifies whether the gopSize is specified
-- in frames or seconds. If you do not plan to change the default gopSize,
-- leave the default. If you specify SECONDS, MediaLive will internally
-- convert the gop size to a frame count.
mpeg2Settings_gopSizeUnits :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2GopSizeUnits)
mpeg2Settings_gopSizeUnits = Lens.lens (\Mpeg2Settings' {gopSizeUnits} -> gopSizeUnits) (\s@Mpeg2Settings' {} a -> s {gopSizeUnits = a} :: Mpeg2Settings)

-- | Relates to the GOP structure. The GOP size (keyframe interval) in the
-- units specified in gopSizeUnits. If you do not know what GOP is, use the
-- default. If gopSizeUnits is frames, then the gopSize must be an integer
-- and must be greater than or equal to 1. If gopSizeUnits is seconds, the
-- gopSize must be greater than 0, but does not need to be an integer.
mpeg2Settings_gopSize :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Double)
mpeg2Settings_gopSize = Lens.lens (\Mpeg2Settings' {gopSize} -> gopSize) (\s@Mpeg2Settings' {} a -> s {gopSize = a} :: Mpeg2Settings)

-- | Complete this field only when afdSignaling is set to FIXED. Enter the
-- AFD value (4 bits) to write on all frames of the video encode.
mpeg2Settings_fixedAfd :: Lens.Lens' Mpeg2Settings (Prelude.Maybe FixedAfd)
mpeg2Settings_fixedAfd = Lens.lens (\Mpeg2Settings' {fixedAfd} -> fixedAfd) (\s@Mpeg2Settings' {} a -> s {fixedAfd = a} :: Mpeg2Settings)

-- | Optionally specify a noise reduction filter, which can improve quality
-- of compressed content. If you do not choose a filter, no filter will be
-- applied. TEMPORAL: This filter is useful for both source content that is
-- noisy (when it has excessive digital artifacts) and source content that
-- is clean. When the content is noisy, the filter cleans up the source
-- content before the encoding phase, with these two effects: First, it
-- improves the output video quality because the content has been cleaned
-- up. Secondly, it decreases the bandwidth because MediaLive does not
-- waste bits on encoding noise. When the content is reasonably clean, the
-- filter tends to decrease the bitrate.
mpeg2Settings_filterSettings :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2FilterSettings)
mpeg2Settings_filterSettings = Lens.lens (\Mpeg2Settings' {filterSettings} -> filterSettings) (\s@Mpeg2Settings' {} a -> s {filterSettings = a} :: Mpeg2Settings)

-- | Relates to the GOP structure. The number of B-frames between reference
-- frames. If you do not know what a B-frame is, use the default.
mpeg2Settings_gopNumBFrames :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_gopNumBFrames = Lens.lens (\Mpeg2Settings' {gopNumBFrames} -> gopNumBFrames) (\s@Mpeg2Settings' {} a -> s {gopNumBFrames = a} :: Mpeg2Settings)

-- | Determines how MediaLive inserts timecodes in the output video. For
-- detailed information about setting up the input and the output for a
-- timecode, see the section on \\\"MediaLive Features - Timecode
-- configuration\\\" in the MediaLive User Guide. DISABLED: do not include
-- timecodes. GOP_TIMECODE: Include timecode metadata in the GOP header.
mpeg2Settings_timecodeInsertion :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2TimecodeInsertionBehavior)
mpeg2Settings_timecodeInsertion = Lens.lens (\Mpeg2Settings' {timecodeInsertion} -> timecodeInsertion) (\s@Mpeg2Settings' {} a -> s {timecodeInsertion = a} :: Mpeg2Settings)

-- | MPEG2: default is open GOP.
mpeg2Settings_gopClosedCadence :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Prelude.Natural)
mpeg2Settings_gopClosedCadence = Lens.lens (\Mpeg2Settings' {gopClosedCadence} -> gopClosedCadence) (\s@Mpeg2Settings' {} a -> s {gopClosedCadence = a} :: Mpeg2Settings)

-- | Choose the type of color space conversion to apply to the output. For
-- detailed information on setting up both the input and the output to
-- obtain the desired color space in the output, see the section on
-- \\\"MediaLive Features - Video - color space\\\" in the MediaLive User
-- Guide. PASSTHROUGH: Keep the color space of the input content - do not
-- convert it. AUTO:Convert all content that is SD to rec 601, and convert
-- all content that is HD to rec 709.
mpeg2Settings_colorSpace :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2ColorSpace)
mpeg2Settings_colorSpace = Lens.lens (\Mpeg2Settings' {colorSpace} -> colorSpace) (\s@Mpeg2Settings' {} a -> s {colorSpace = a} :: Mpeg2Settings)

-- | Choose Off to disable adaptive quantization. Or choose another value to
-- enable the quantizer and set its strength. The strengths are: Auto, Off,
-- Low, Medium, High. When you enable this field, MediaLive allows
-- intra-frame quantizers to vary, which might improve visual quality.
mpeg2Settings_adaptiveQuantization :: Lens.Lens' Mpeg2Settings (Prelude.Maybe Mpeg2AdaptiveQuantization)
mpeg2Settings_adaptiveQuantization = Lens.lens (\Mpeg2Settings' {adaptiveQuantization} -> adaptiveQuantization) (\s@Mpeg2Settings' {} a -> s {adaptiveQuantization = a} :: Mpeg2Settings)

-- | Indicates the AFD values that MediaLive will write into the video
-- encode. If you do not know what AFD signaling is, or if your downstream
-- system has not given you guidance, choose AUTO. AUTO: MediaLive will try
-- to preserve the input AFD value (in cases where multiple AFD values are
-- valid). FIXED: MediaLive will use the value you specify in fixedAFD.
mpeg2Settings_afdSignaling :: Lens.Lens' Mpeg2Settings (Prelude.Maybe AfdSignaling)
mpeg2Settings_afdSignaling = Lens.lens (\Mpeg2Settings' {afdSignaling} -> afdSignaling) (\s@Mpeg2Settings' {} a -> s {afdSignaling = a} :: Mpeg2Settings)

-- | The framerate numerator. For example, 24000. The framerate is the
-- numerator divided by the denominator. For example, 24000 \/ 1001 =
-- 23.976 FPS.
mpeg2Settings_framerateNumerator :: Lens.Lens' Mpeg2Settings Prelude.Natural
mpeg2Settings_framerateNumerator = Lens.lens (\Mpeg2Settings' {framerateNumerator} -> framerateNumerator) (\s@Mpeg2Settings' {} a -> s {framerateNumerator = a} :: Mpeg2Settings)

-- | description\": \"The framerate denominator. For example, 1001. The
-- framerate is the numerator divided by the denominator. For example,
-- 24000 \/ 1001 = 23.976 FPS.
mpeg2Settings_framerateDenominator :: Lens.Lens' Mpeg2Settings Prelude.Natural
mpeg2Settings_framerateDenominator = Lens.lens (\Mpeg2Settings' {framerateDenominator} -> framerateDenominator) (\s@Mpeg2Settings' {} a -> s {framerateDenominator = a} :: Mpeg2Settings)

instance Core.FromJSON Mpeg2Settings where
  parseJSON =
    Core.withObject
      "Mpeg2Settings"
      ( \x ->
          Mpeg2Settings'
            Prelude.<$> (x Core..:? "scanType")
            Prelude.<*> (x Core..:? "displayAspectRatio")
            Prelude.<*> (x Core..:? "colorMetadata")
            Prelude.<*> (x Core..:? "subgopLength")
            Prelude.<*> (x Core..:? "gopSizeUnits")
            Prelude.<*> (x Core..:? "gopSize")
            Prelude.<*> (x Core..:? "fixedAfd")
            Prelude.<*> (x Core..:? "filterSettings")
            Prelude.<*> (x Core..:? "gopNumBFrames")
            Prelude.<*> (x Core..:? "timecodeInsertion")
            Prelude.<*> (x Core..:? "gopClosedCadence")
            Prelude.<*> (x Core..:? "colorSpace")
            Prelude.<*> (x Core..:? "adaptiveQuantization")
            Prelude.<*> (x Core..:? "afdSignaling")
            Prelude.<*> (x Core..: "framerateNumerator")
            Prelude.<*> (x Core..: "framerateDenominator")
      )

instance Prelude.Hashable Mpeg2Settings

instance Prelude.NFData Mpeg2Settings

instance Core.ToJSON Mpeg2Settings where
  toJSON Mpeg2Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scanType" Core..=) Prelude.<$> scanType,
            ("displayAspectRatio" Core..=)
              Prelude.<$> displayAspectRatio,
            ("colorMetadata" Core..=) Prelude.<$> colorMetadata,
            ("subgopLength" Core..=) Prelude.<$> subgopLength,
            ("gopSizeUnits" Core..=) Prelude.<$> gopSizeUnits,
            ("gopSize" Core..=) Prelude.<$> gopSize,
            ("fixedAfd" Core..=) Prelude.<$> fixedAfd,
            ("filterSettings" Core..=)
              Prelude.<$> filterSettings,
            ("gopNumBFrames" Core..=) Prelude.<$> gopNumBFrames,
            ("timecodeInsertion" Core..=)
              Prelude.<$> timecodeInsertion,
            ("gopClosedCadence" Core..=)
              Prelude.<$> gopClosedCadence,
            ("colorSpace" Core..=) Prelude.<$> colorSpace,
            ("adaptiveQuantization" Core..=)
              Prelude.<$> adaptiveQuantization,
            ("afdSignaling" Core..=) Prelude.<$> afdSignaling,
            Prelude.Just
              ("framerateNumerator" Core..= framerateNumerator),
            Prelude.Just
              ( "framerateDenominator"
                  Core..= framerateDenominator
              )
          ]
      )

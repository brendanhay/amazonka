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
-- Module      : Network.AWS.MediaConvert.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.MediaConvert.Types.HlsAdMarkers
import Network.AWS.MediaConvert.Types.HlsAdditionalManifest
import Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping
import Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
import Network.AWS.MediaConvert.Types.HlsClientCache
import Network.AWS.MediaConvert.Types.HlsCodecSpecification
import Network.AWS.MediaConvert.Types.HlsDirectoryStructure
import Network.AWS.MediaConvert.Types.HlsEncryptionSettings
import Network.AWS.MediaConvert.Types.HlsImageBasedTrickPlay
import Network.AWS.MediaConvert.Types.HlsImageBasedTrickPlaySettings
import Network.AWS.MediaConvert.Types.HlsManifestCompression
import Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
import Network.AWS.MediaConvert.Types.HlsOutputSelection
import Network.AWS.MediaConvert.Types.HlsProgramDateTime
import Network.AWS.MediaConvert.Types.HlsSegmentControl
import Network.AWS.MediaConvert.Types.HlsSegmentLengthControl
import Network.AWS.MediaConvert.Types.HlsStreamInfResolution
import Network.AWS.MediaConvert.Types.HlsTargetDurationCompatibilityMode
import Network.AWS.MediaConvert.Types.HlsTimedMetadataId3Frame
import qualified Network.AWS.Prelude as Prelude

-- | Settings related to your HLS output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to HLS_GROUP_SETTINGS.
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Specify the length, in whole seconds, of each segment. When you don\'t
    -- specify a value, MediaConvert defaults to 10. Related settings: Use
    -- Segment length control (SegmentLengthControl) to specify whether the
    -- encoder enforces this value strictly. Use Segment control
    -- (HlsSegmentControl) to specify whether MediaConvert creates separate
    -- segment files or one content file that has metadata to mark the segment
    -- boundaries.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Prelude.Maybe HlsTimedMetadataId3Frame,
    -- | Indicates whether the .m3u8 manifest file should be generated for this
    -- HLS output group.
    outputSelection :: Prelude.Maybe HlsOutputSelection,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Prelude.Maybe Prelude.Int,
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
    -- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Prelude.Maybe HlsSegmentControl,
    -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Prelude.Maybe HlsDirectoryStructure,
    -- | Choose one or more ad marker types to decorate your Apple HLS manifest.
    -- This setting does not determine whether SCTE-35 markers appear in the
    -- outputs themselves.
    adMarkers :: Prelude.Maybe [HlsAdMarkers],
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Prelude.Maybe HlsManifestCompression,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Prelude.Maybe HlsCodecSpecification,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Prelude.Maybe HlsStreamInfResolution,
    -- | Specify how you want MediaConvert to determine the segment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Segment length (SegmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    segmentLengthControl :: Prelude.Maybe HlsSegmentLengthControl,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated as follows: either the program date and
    -- time are initialized using the input timecode source, or the time is
    -- initialized using the input timecode source and the date is initialized
    -- using the timestamp_offset.
    programDateTime :: Prelude.Maybe HlsProgramDateTime,
    -- | By default, the service creates one top-level .m3u8 HLS manifest for
    -- each HLS output group in your job. This default manifest references
    -- every output in the output group. To create additional top-level
    -- manifests that reference a subset of the outputs in the output group,
    -- specify a list of them here.
    additionalManifests :: Prelude.Maybe [HlsAdditionalManifest],
    -- | Specify whether MediaConvert generates images for trick play. Keep the
    -- default value, None (NONE), to not generate any images. Choose Thumbnail
    -- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
    -- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
    -- full-resolution images of single frames. MediaConvert creates a child
    -- manifest for each set of images that you generate and adds corresponding
    -- entries to the parent manifest. A common application for these images is
    -- Roku trick mode. The thumbnails and full-frame images that MediaConvert
    -- creates with this feature are compatible with this Roku specification:
    -- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
    imageBasedTrickPlay :: Prelude.Maybe HlsImageBasedTrickPlay,
    -- | Number of segments to write to a subdirectory before starting a new one.
    -- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
    -- effect.
    segmentsPerSubdirectory :: Prelude.Maybe Prelude.Natural,
    -- | DRM settings.
    encryption :: Prelude.Maybe HlsEncryptionSettings,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Keep this setting at the default value of 0, unless you are
    -- troubleshooting a problem with how devices play back the end of your
    -- video asset. If you know that player devices are hanging on the final
    -- segment of your video because the length of your final segment is too
    -- short, use this setting to specify a minimum final segment length, in
    -- seconds. Choose a value that is greater than or equal to 1 and less than
    -- your segment length. When you specify a value for this setting, the
    -- encoder will combine any final segment that is shorter than the length
    -- that you specify with the previous segment. For example, your segment
    -- length is 3 seconds and your final segment is .5 seconds without a
    -- minimum final segment length; when you set the minimum final segment
    -- length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Prelude.Maybe Prelude.Double,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | Language to be used on Caption outputs
    captionLanguageMappings :: Prelude.Maybe [HlsCaptionLanguageMapping],
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Prelude.Maybe Prelude.Int,
    -- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
    -- ADVANCED
    imageBasedTrickPlaySettings :: Prelude.Maybe HlsImageBasedTrickPlaySettings,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Prelude.Maybe Prelude.Natural,
    -- | Disable this setting only when your workflow requires the
    -- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
    -- (ENABLED) and control caching in your video distribution set up. For
    -- example, use the Cache-Control http header.
    clientCache :: Prelude.Maybe HlsClientCache,
    -- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
    -- and you encounter playback issues. Keep the default value, Include
    -- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
    -- remove the audio-only headers from your audio segments.
    audioOnlyHeader :: Prelude.Maybe HlsAudioOnlyHeader,
    -- | When set, Minimum Segment Size is enforced by looking ahead and back
    -- within the specified range for a nearby avail and extending the segment
    -- size if needed.
    minSegmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Applies only to 608 Embedded output captions. Insert: Include
    -- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
    -- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
    -- Language Code you specify. Make sure to specify the languages in the
    -- order in which they appear in the original source (if the source is
    -- embedded format) or the order of the caption selectors (if the source is
    -- other than embedded). Otherwise, languages in the manifest will not
    -- match up properly with the output captions. None: Include
    -- CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any
    -- CLOSED-CAPTIONS line from the manifest.
    captionLanguageSetting :: Prelude.Maybe HlsCaptionLanguageSetting,
    -- | When set to LEGACY, the segment target duration is always rounded up to
    -- the nearest integer value above its current value in seconds. When set
    -- to SPEC\\\\_COMPLIANT, the segment target duration is rounded up to the
    -- nearest integer value if fraction seconds are greater than or equal to
    -- 0.5 (>= 0.5) and rounded down if less than 0.5 (\< 0.5). You may need to
    -- use LEGACY if your client needs to ensure that the target duration is
    -- always longer than the actual duration of the segment. Some older
    -- players may experience interrupted playback when the actual duration of
    -- a track in a segment is longer than the target duration.
    targetDurationCompatibilityMode :: Prelude.Maybe HlsTargetDurationCompatibilityMode,
    -- | Indicates whether the output manifest should use floating point values
    -- for segment duration.
    manifestDurationFormat :: Prelude.Maybe HlsManifestDurationFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentLength', 'hlsGroupSettings_segmentLength' - Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 10. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (HlsSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
--
-- 'timedMetadataId3Frame', 'hlsGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'outputSelection', 'hlsGroupSettings_outputSelection' - Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
--
-- 'timedMetadataId3Period', 'hlsGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- 'segmentControl', 'hlsGroupSettings_segmentControl' - When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- 'directoryStructure', 'hlsGroupSettings_directoryStructure' - Indicates whether segments should be placed in subdirectories.
--
-- 'adMarkers', 'hlsGroupSettings_adMarkers' - Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
--
-- 'manifestCompression', 'hlsGroupSettings_manifestCompression' - When set to GZIP, compresses HLS playlist.
--
-- 'baseUrl', 'hlsGroupSettings_baseUrl' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'codecSpecification', 'hlsGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'streamInfResolution', 'hlsGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
--
-- 'segmentLengthControl', 'hlsGroupSettings_segmentLengthControl' - Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'programDateTime', 'hlsGroupSettings_programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
--
-- 'additionalManifests', 'hlsGroupSettings_additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
--
-- 'imageBasedTrickPlay', 'hlsGroupSettings_imageBasedTrickPlay' - Specify whether MediaConvert generates images for trick play. Keep the
-- default value, None (NONE), to not generate any images. Choose Thumbnail
-- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
-- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
-- full-resolution images of single frames. MediaConvert creates a child
-- manifest for each set of images that you generate and adds corresponding
-- entries to the parent manifest. A common application for these images is
-- Roku trick mode. The thumbnails and full-frame images that MediaConvert
-- creates with this feature are compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
--
-- 'segmentsPerSubdirectory', 'hlsGroupSettings_segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
-- effect.
--
-- 'encryption', 'hlsGroupSettings_encryption' - DRM settings.
--
-- 'destination', 'hlsGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'minFinalSegmentLength', 'hlsGroupSettings_minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are
-- troubleshooting a problem with how devices play back the end of your
-- video asset. If you know that player devices are hanging on the final
-- segment of your video because the length of your final segment is too
-- short, use this setting to specify a minimum final segment length, in
-- seconds. Choose a value that is greater than or equal to 1 and less than
-- your segment length. When you specify a value for this setting, the
-- encoder will combine any final segment that is shorter than the length
-- that you specify with the previous segment. For example, your segment
-- length is 3 seconds and your final segment is .5 seconds without a
-- minimum final segment length; when you set the minimum final segment
-- length to 1, your final segment is 3.5 seconds.
--
-- 'destinationSettings', 'hlsGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'captionLanguageMappings', 'hlsGroupSettings_captionLanguageMappings' - Language to be used on Caption outputs
--
-- 'timestampDeltaMilliseconds', 'hlsGroupSettings_timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- 'imageBasedTrickPlaySettings', 'hlsGroupSettings_imageBasedTrickPlaySettings' - Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
--
-- 'programDateTimePeriod', 'hlsGroupSettings_programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- 'clientCache', 'hlsGroupSettings_clientCache' - Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
--
-- 'audioOnlyHeader', 'hlsGroupSettings_audioOnlyHeader' - Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
--
-- 'minSegmentLength', 'hlsGroupSettings_minSegmentLength' - When set, Minimum Segment Size is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
--
-- 'captionLanguageSetting', 'hlsGroupSettings_captionLanguageSetting' - Applies only to 608 Embedded output captions. Insert: Include
-- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
-- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
-- Language Code you specify. Make sure to specify the languages in the
-- order in which they appear in the original source (if the source is
-- embedded format) or the order of the caption selectors (if the source is
-- other than embedded). Otherwise, languages in the manifest will not
-- match up properly with the output captions. None: Include
-- CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any
-- CLOSED-CAPTIONS line from the manifest.
--
-- 'targetDurationCompatibilityMode', 'hlsGroupSettings_targetDurationCompatibilityMode' - When set to LEGACY, the segment target duration is always rounded up to
-- the nearest integer value above its current value in seconds. When set
-- to SPEC\\\\_COMPLIANT, the segment target duration is rounded up to the
-- nearest integer value if fraction seconds are greater than or equal to
-- 0.5 (>= 0.5) and rounded down if less than 0.5 (\< 0.5). You may need to
-- use LEGACY if your client needs to ensure that the target duration is
-- always longer than the actual duration of the segment. Some older
-- players may experience interrupted playback when the actual duration of
-- a track in a segment is longer than the target duration.
--
-- 'manifestDurationFormat', 'hlsGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point values
-- for segment duration.
newHlsGroupSettings ::
  HlsGroupSettings
newHlsGroupSettings =
  HlsGroupSettings'
    { segmentLength = Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      outputSelection = Prelude.Nothing,
      timedMetadataId3Period = Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      directoryStructure = Prelude.Nothing,
      adMarkers = Prelude.Nothing,
      manifestCompression = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      codecSpecification = Prelude.Nothing,
      streamInfResolution = Prelude.Nothing,
      segmentLengthControl = Prelude.Nothing,
      programDateTime = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      imageBasedTrickPlay = Prelude.Nothing,
      segmentsPerSubdirectory = Prelude.Nothing,
      encryption = Prelude.Nothing,
      destination = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      captionLanguageMappings = Prelude.Nothing,
      timestampDeltaMilliseconds = Prelude.Nothing,
      imageBasedTrickPlaySettings = Prelude.Nothing,
      programDateTimePeriod = Prelude.Nothing,
      clientCache = Prelude.Nothing,
      audioOnlyHeader = Prelude.Nothing,
      minSegmentLength = Prelude.Nothing,
      captionLanguageSetting = Prelude.Nothing,
      targetDurationCompatibilityMode = Prelude.Nothing,
      manifestDurationFormat = Prelude.Nothing
    }

-- | Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 10. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (HlsSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | Indicates ID3 frame that has the timecode.
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | Timed Metadata interval in seconds.
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Int)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
hlsGroupSettings_segmentControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentControl)
hlsGroupSettings_segmentControl = Lens.lens (\HlsGroupSettings' {segmentControl} -> segmentControl) (\s@HlsGroupSettings' {} a -> s {segmentControl = a} :: HlsGroupSettings)

-- | Indicates whether segments should be placed in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | When set to GZIP, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrl = Lens.lens (\HlsGroupSettings' {baseUrl} -> baseUrl) (\s@HlsGroupSettings' {} a -> s {baseUrl = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

-- | Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
hlsGroupSettings_segmentLengthControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentLengthControl)
hlsGroupSettings_segmentLengthControl = Lens.lens (\HlsGroupSettings' {segmentLengthControl} -> segmentLengthControl) (\s@HlsGroupSettings' {} a -> s {segmentLengthControl = a} :: HlsGroupSettings)

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

-- | By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
hlsGroupSettings_additionalManifests :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdditionalManifest])
hlsGroupSettings_additionalManifests = Lens.lens (\HlsGroupSettings' {additionalManifests} -> additionalManifests) (\s@HlsGroupSettings' {} a -> s {additionalManifests = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Specify whether MediaConvert generates images for trick play. Keep the
-- default value, None (NONE), to not generate any images. Choose Thumbnail
-- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
-- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
-- full-resolution images of single frames. MediaConvert creates a child
-- manifest for each set of images that you generate and adds corresponding
-- entries to the parent manifest. A common application for these images is
-- Roku trick mode. The thumbnails and full-frame images that MediaConvert
-- creates with this feature are compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
hlsGroupSettings_imageBasedTrickPlay :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsImageBasedTrickPlay)
hlsGroupSettings_imageBasedTrickPlay = Lens.lens (\HlsGroupSettings' {imageBasedTrickPlay} -> imageBasedTrickPlay) (\s@HlsGroupSettings' {} a -> s {imageBasedTrickPlay = a} :: HlsGroupSettings)

-- | Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
-- effect.
hlsGroupSettings_segmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentsPerSubdirectory = Lens.lens (\HlsGroupSettings' {segmentsPerSubdirectory} -> segmentsPerSubdirectory) (\s@HlsGroupSettings' {} a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)

-- | DRM settings.
hlsGroupSettings_encryption :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsEncryptionSettings)
hlsGroupSettings_encryption = Lens.lens (\HlsGroupSettings' {encryption} -> encryption) (\s@HlsGroupSettings' {} a -> s {encryption = a} :: HlsGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
hlsGroupSettings_destination :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_destination = Lens.lens (\HlsGroupSettings' {destination} -> destination) (\s@HlsGroupSettings' {} a -> s {destination = a} :: HlsGroupSettings)

-- | Keep this setting at the default value of 0, unless you are
-- troubleshooting a problem with how devices play back the end of your
-- video asset. If you know that player devices are hanging on the final
-- segment of your video because the length of your final segment is too
-- short, use this setting to specify a minimum final segment length, in
-- seconds. Choose a value that is greater than or equal to 1 and less than
-- your segment length. When you specify a value for this setting, the
-- encoder will combine any final segment that is shorter than the length
-- that you specify with the previous segment. For example, your segment
-- length is 3 seconds and your final segment is .5 seconds without a
-- minimum final segment length; when you set the minimum final segment
-- length to 1, your final segment is 3.5 seconds.
hlsGroupSettings_minFinalSegmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Double)
hlsGroupSettings_minFinalSegmentLength = Lens.lens (\HlsGroupSettings' {minFinalSegmentLength} -> minFinalSegmentLength) (\s@HlsGroupSettings' {} a -> s {minFinalSegmentLength = a} :: HlsGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
hlsGroupSettings_destinationSettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe DestinationSettings)
hlsGroupSettings_destinationSettings = Lens.lens (\HlsGroupSettings' {destinationSettings} -> destinationSettings) (\s@HlsGroupSettings' {} a -> s {destinationSettings = a} :: HlsGroupSettings)

-- | Language to be used on Caption outputs
hlsGroupSettings_captionLanguageMappings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsCaptionLanguageMapping])
hlsGroupSettings_captionLanguageMappings = Lens.lens (\HlsGroupSettings' {captionLanguageMappings} -> captionLanguageMappings) (\s@HlsGroupSettings' {} a -> s {captionLanguageMappings = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hlsGroupSettings_timestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Int)
hlsGroupSettings_timestampDeltaMilliseconds = Lens.lens (\HlsGroupSettings' {timestampDeltaMilliseconds} -> timestampDeltaMilliseconds) (\s@HlsGroupSettings' {} a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
hlsGroupSettings_imageBasedTrickPlaySettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsImageBasedTrickPlaySettings)
hlsGroupSettings_imageBasedTrickPlaySettings = Lens.lens (\HlsGroupSettings' {imageBasedTrickPlaySettings} -> imageBasedTrickPlaySettings) (\s@HlsGroupSettings' {} a -> s {imageBasedTrickPlaySettings = a} :: HlsGroupSettings)

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hlsGroupSettings_programDateTimePeriod :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_programDateTimePeriod = Lens.lens (\HlsGroupSettings' {programDateTimePeriod} -> programDateTimePeriod) (\s@HlsGroupSettings' {} a -> s {programDateTimePeriod = a} :: HlsGroupSettings)

-- | Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
hlsGroupSettings_clientCache :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsClientCache)
hlsGroupSettings_clientCache = Lens.lens (\HlsGroupSettings' {clientCache} -> clientCache) (\s@HlsGroupSettings' {} a -> s {clientCache = a} :: HlsGroupSettings)

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
hlsGroupSettings_audioOnlyHeader :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsAudioOnlyHeader)
hlsGroupSettings_audioOnlyHeader = Lens.lens (\HlsGroupSettings' {audioOnlyHeader} -> audioOnlyHeader) (\s@HlsGroupSettings' {} a -> s {audioOnlyHeader = a} :: HlsGroupSettings)

-- | When set, Minimum Segment Size is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
hlsGroupSettings_minSegmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_minSegmentLength = Lens.lens (\HlsGroupSettings' {minSegmentLength} -> minSegmentLength) (\s@HlsGroupSettings' {} a -> s {minSegmentLength = a} :: HlsGroupSettings)

-- | Applies only to 608 Embedded output captions. Insert: Include
-- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
-- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
-- Language Code you specify. Make sure to specify the languages in the
-- order in which they appear in the original source (if the source is
-- embedded format) or the order of the caption selectors (if the source is
-- other than embedded). Otherwise, languages in the manifest will not
-- match up properly with the output captions. None: Include
-- CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any
-- CLOSED-CAPTIONS line from the manifest.
hlsGroupSettings_captionLanguageSetting :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCaptionLanguageSetting)
hlsGroupSettings_captionLanguageSetting = Lens.lens (\HlsGroupSettings' {captionLanguageSetting} -> captionLanguageSetting) (\s@HlsGroupSettings' {} a -> s {captionLanguageSetting = a} :: HlsGroupSettings)

-- | When set to LEGACY, the segment target duration is always rounded up to
-- the nearest integer value above its current value in seconds. When set
-- to SPEC\\\\_COMPLIANT, the segment target duration is rounded up to the
-- nearest integer value if fraction seconds are greater than or equal to
-- 0.5 (>= 0.5) and rounded down if less than 0.5 (\< 0.5). You may need to
-- use LEGACY if your client needs to ensure that the target duration is
-- always longer than the actual duration of the segment. Some older
-- players may experience interrupted playback when the actual duration of
-- a track in a segment is longer than the target duration.
hlsGroupSettings_targetDurationCompatibilityMode :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTargetDurationCompatibilityMode)
hlsGroupSettings_targetDurationCompatibilityMode = Lens.lens (\HlsGroupSettings' {targetDurationCompatibilityMode} -> targetDurationCompatibilityMode) (\s@HlsGroupSettings' {} a -> s {targetDurationCompatibilityMode = a} :: HlsGroupSettings)

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestDurationFormat)
hlsGroupSettings_manifestDurationFormat = Lens.lens (\HlsGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@HlsGroupSettings' {} a -> s {manifestDurationFormat = a} :: HlsGroupSettings)

instance Core.FromJSON HlsGroupSettings where
  parseJSON =
    Core.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Prelude.<$> (x Core..:? "segmentLength")
            Prelude.<*> (x Core..:? "timedMetadataId3Frame")
            Prelude.<*> (x Core..:? "outputSelection")
            Prelude.<*> (x Core..:? "timedMetadataId3Period")
            Prelude.<*> (x Core..:? "segmentControl")
            Prelude.<*> (x Core..:? "directoryStructure")
            Prelude.<*> (x Core..:? "adMarkers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "manifestCompression")
            Prelude.<*> (x Core..:? "baseUrl")
            Prelude.<*> (x Core..:? "codecSpecification")
            Prelude.<*> (x Core..:? "streamInfResolution")
            Prelude.<*> (x Core..:? "segmentLengthControl")
            Prelude.<*> (x Core..:? "programDateTime")
            Prelude.<*> ( x Core..:? "additionalManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "imageBasedTrickPlay")
            Prelude.<*> (x Core..:? "segmentsPerSubdirectory")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "minFinalSegmentLength")
            Prelude.<*> (x Core..:? "destinationSettings")
            Prelude.<*> ( x Core..:? "captionLanguageMappings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timestampDeltaMilliseconds")
            Prelude.<*> (x Core..:? "imageBasedTrickPlaySettings")
            Prelude.<*> (x Core..:? "programDateTimePeriod")
            Prelude.<*> (x Core..:? "clientCache")
            Prelude.<*> (x Core..:? "audioOnlyHeader")
            Prelude.<*> (x Core..:? "minSegmentLength")
            Prelude.<*> (x Core..:? "captionLanguageSetting")
            Prelude.<*> (x Core..:? "targetDurationCompatibilityMode")
            Prelude.<*> (x Core..:? "manifestDurationFormat")
      )

instance Prelude.Hashable HlsGroupSettings

instance Prelude.NFData HlsGroupSettings

instance Core.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentLength" Core..=) Prelude.<$> segmentLength,
            ("timedMetadataId3Frame" Core..=)
              Prelude.<$> timedMetadataId3Frame,
            ("outputSelection" Core..=)
              Prelude.<$> outputSelection,
            ("timedMetadataId3Period" Core..=)
              Prelude.<$> timedMetadataId3Period,
            ("segmentControl" Core..=)
              Prelude.<$> segmentControl,
            ("directoryStructure" Core..=)
              Prelude.<$> directoryStructure,
            ("adMarkers" Core..=) Prelude.<$> adMarkers,
            ("manifestCompression" Core..=)
              Prelude.<$> manifestCompression,
            ("baseUrl" Core..=) Prelude.<$> baseUrl,
            ("codecSpecification" Core..=)
              Prelude.<$> codecSpecification,
            ("streamInfResolution" Core..=)
              Prelude.<$> streamInfResolution,
            ("segmentLengthControl" Core..=)
              Prelude.<$> segmentLengthControl,
            ("programDateTime" Core..=)
              Prelude.<$> programDateTime,
            ("additionalManifests" Core..=)
              Prelude.<$> additionalManifests,
            ("imageBasedTrickPlay" Core..=)
              Prelude.<$> imageBasedTrickPlay,
            ("segmentsPerSubdirectory" Core..=)
              Prelude.<$> segmentsPerSubdirectory,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("destination" Core..=) Prelude.<$> destination,
            ("minFinalSegmentLength" Core..=)
              Prelude.<$> minFinalSegmentLength,
            ("destinationSettings" Core..=)
              Prelude.<$> destinationSettings,
            ("captionLanguageMappings" Core..=)
              Prelude.<$> captionLanguageMappings,
            ("timestampDeltaMilliseconds" Core..=)
              Prelude.<$> timestampDeltaMilliseconds,
            ("imageBasedTrickPlaySettings" Core..=)
              Prelude.<$> imageBasedTrickPlaySettings,
            ("programDateTimePeriod" Core..=)
              Prelude.<$> programDateTimePeriod,
            ("clientCache" Core..=) Prelude.<$> clientCache,
            ("audioOnlyHeader" Core..=)
              Prelude.<$> audioOnlyHeader,
            ("minSegmentLength" Core..=)
              Prelude.<$> minSegmentLength,
            ("captionLanguageSetting" Core..=)
              Prelude.<$> captionLanguageSetting,
            ("targetDurationCompatibilityMode" Core..=)
              Prelude.<$> targetDurationCompatibilityMode,
            ("manifestDurationFormat" Core..=)
              Prelude.<$> manifestDurationFormat
          ]
      )

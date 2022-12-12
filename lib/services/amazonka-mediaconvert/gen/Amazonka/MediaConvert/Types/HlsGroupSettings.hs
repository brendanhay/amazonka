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
-- Module      : Amazonka.MediaConvert.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DestinationSettings
import Amazonka.MediaConvert.Types.HlsAdMarkers
import Amazonka.MediaConvert.Types.HlsAdditionalManifest
import Amazonka.MediaConvert.Types.HlsAudioOnlyHeader
import Amazonka.MediaConvert.Types.HlsCaptionLanguageMapping
import Amazonka.MediaConvert.Types.HlsCaptionLanguageSetting
import Amazonka.MediaConvert.Types.HlsCaptionSegmentLengthControl
import Amazonka.MediaConvert.Types.HlsClientCache
import Amazonka.MediaConvert.Types.HlsCodecSpecification
import Amazonka.MediaConvert.Types.HlsDirectoryStructure
import Amazonka.MediaConvert.Types.HlsEncryptionSettings
import Amazonka.MediaConvert.Types.HlsImageBasedTrickPlay
import Amazonka.MediaConvert.Types.HlsImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.HlsManifestCompression
import Amazonka.MediaConvert.Types.HlsManifestDurationFormat
import Amazonka.MediaConvert.Types.HlsOutputSelection
import Amazonka.MediaConvert.Types.HlsProgramDateTime
import Amazonka.MediaConvert.Types.HlsSegmentControl
import Amazonka.MediaConvert.Types.HlsSegmentLengthControl
import Amazonka.MediaConvert.Types.HlsStreamInfResolution
import Amazonka.MediaConvert.Types.HlsTargetDurationCompatibilityMode
import Amazonka.MediaConvert.Types.HlsTimedMetadataId3Frame
import qualified Amazonka.Prelude as Prelude

-- | Settings related to your HLS output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to HLS_GROUP_SETTINGS.
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Choose one or more ad marker types to decorate your Apple HLS manifest.
    -- This setting does not determine whether SCTE-35 markers appear in the
    -- outputs themselves.
    adMarkers :: Prelude.Maybe [HlsAdMarkers],
    -- | By default, the service creates one top-level .m3u8 HLS manifest for
    -- each HLS output group in your job. This default manifest references
    -- every output in the output group. To create additional top-level
    -- manifests that reference a subset of the outputs in the output group,
    -- specify a list of them here.
    additionalManifests :: Prelude.Maybe [HlsAdditionalManifest],
    -- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
    -- and you encounter playback issues. Keep the default value, Include
    -- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
    -- remove the audio-only headers from your audio segments.
    audioOnlyHeader :: Prelude.Maybe HlsAudioOnlyHeader,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Language to be used on Caption outputs
    captionLanguageMappings :: Prelude.Maybe [HlsCaptionLanguageMapping],
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
    -- | Set Caption segment length control (CaptionSegmentLengthControl) to
    -- Match video (MATCH_VIDEO) to create caption segments that align with the
    -- video segments from the first video output in this output group. For
    -- example, if the video segments are 2 seconds long, your WebVTT segments
    -- will also be 2 seconds long. Keep the default setting, Large segments
    -- (LARGE_SEGMENTS) to create caption segments that are 300 seconds long.
    captionSegmentLengthControl :: Prelude.Maybe HlsCaptionSegmentLengthControl,
    -- | Disable this setting only when your workflow requires the
    -- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
    -- (ENABLED) and control caching in your video distribution set up. For
    -- example, use the Cache-Control http header.
    clientCache :: Prelude.Maybe HlsClientCache,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Prelude.Maybe HlsCodecSpecification,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Prelude.Maybe HlsDirectoryStructure,
    -- | DRM settings.
    encryption :: Prelude.Maybe HlsEncryptionSettings,
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
    -- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
    -- ADVANCED
    imageBasedTrickPlaySettings :: Prelude.Maybe HlsImageBasedTrickPlaySettings,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Prelude.Maybe HlsManifestCompression,
    -- | Indicates whether the output manifest should use floating point values
    -- for segment duration.
    manifestDurationFormat :: Prelude.Maybe HlsManifestDurationFormat,
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
    -- | When set, Minimum Segment Size is enforced by looking ahead and back
    -- within the specified range for a nearby avail and extending the segment
    -- size if needed.
    minSegmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the .m3u8 manifest file should be generated for this
    -- HLS output group.
    outputSelection :: Prelude.Maybe HlsOutputSelection,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated as follows: either the program date and
    -- time are initialized using the input timecode source, or the time is
    -- initialized using the input timecode source and the date is initialized
    -- using the timestamp_offset.
    programDateTime :: Prelude.Maybe HlsProgramDateTime,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Prelude.Maybe Prelude.Natural,
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
    -- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Prelude.Maybe HlsSegmentControl,
    -- | Specify the length, in whole seconds, of each segment. When you don\'t
    -- specify a value, MediaConvert defaults to 10. Related settings: Use
    -- Segment length control (SegmentLengthControl) to specify whether the
    -- encoder enforces this value strictly. Use Segment control
    -- (HlsSegmentControl) to specify whether MediaConvert creates separate
    -- segment files or one content file that has metadata to mark the segment
    -- boundaries.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Specify how you want MediaConvert to determine the segment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Segment length (SegmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    segmentLengthControl :: Prelude.Maybe HlsSegmentLengthControl,
    -- | Number of segments to write to a subdirectory before starting a new one.
    -- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
    -- effect.
    segmentsPerSubdirectory :: Prelude.Maybe Prelude.Natural,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Prelude.Maybe HlsStreamInfResolution,
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
    -- | Specify the type of the ID3 frame (timedMetadataId3Frame) to use for ID3
    -- timestamps (timedMetadataId3Period) in your output. To include ID3
    -- timestamps: Specify PRIV (PRIV) or TDRL (TDRL) and set ID3 metadata
    -- (timedMetadata) to Passthrough (PASSTHROUGH). To exclude ID3 timestamps:
    -- Set ID3 timestamp frame type to None (NONE).
    timedMetadataId3Frame :: Prelude.Maybe HlsTimedMetadataId3Frame,
    -- | Specify the interval in seconds to write ID3 timestamps in your output.
    -- The first timestamp starts at the output timecode and date, and
    -- increases incrementally with each ID3 timestamp. To use the default
    -- interval of 10 seconds: Leave blank. To include this metadata in your
    -- output: Set ID3 timestamp frame type (timedMetadataId3Frame) to PRIV
    -- (PRIV) or TDRL (TDRL), and set ID3 metadata (timedMetadata) to
    -- Passthrough (PASSTHROUGH).
    timedMetadataId3Period :: Prelude.Maybe Prelude.Int,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Prelude.Maybe Prelude.Int
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
-- 'adMarkers', 'hlsGroupSettings_adMarkers' - Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
--
-- 'additionalManifests', 'hlsGroupSettings_additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
--
-- 'audioOnlyHeader', 'hlsGroupSettings_audioOnlyHeader' - Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
--
-- 'baseUrl', 'hlsGroupSettings_baseUrl' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'captionLanguageMappings', 'hlsGroupSettings_captionLanguageMappings' - Language to be used on Caption outputs
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
-- 'captionSegmentLengthControl', 'hlsGroupSettings_captionSegmentLengthControl' - Set Caption segment length control (CaptionSegmentLengthControl) to
-- Match video (MATCH_VIDEO) to create caption segments that align with the
-- video segments from the first video output in this output group. For
-- example, if the video segments are 2 seconds long, your WebVTT segments
-- will also be 2 seconds long. Keep the default setting, Large segments
-- (LARGE_SEGMENTS) to create caption segments that are 300 seconds long.
--
-- 'clientCache', 'hlsGroupSettings_clientCache' - Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
--
-- 'codecSpecification', 'hlsGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'destination', 'hlsGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'destinationSettings', 'hlsGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'directoryStructure', 'hlsGroupSettings_directoryStructure' - Indicates whether segments should be placed in subdirectories.
--
-- 'encryption', 'hlsGroupSettings_encryption' - DRM settings.
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
-- 'imageBasedTrickPlaySettings', 'hlsGroupSettings_imageBasedTrickPlaySettings' - Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
--
-- 'manifestCompression', 'hlsGroupSettings_manifestCompression' - When set to GZIP, compresses HLS playlist.
--
-- 'manifestDurationFormat', 'hlsGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point values
-- for segment duration.
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
-- 'minSegmentLength', 'hlsGroupSettings_minSegmentLength' - When set, Minimum Segment Size is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
--
-- 'outputSelection', 'hlsGroupSettings_outputSelection' - Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
--
-- 'programDateTime', 'hlsGroupSettings_programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
--
-- 'programDateTimePeriod', 'hlsGroupSettings_programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- 'segmentControl', 'hlsGroupSettings_segmentControl' - When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- 'segmentLength', 'hlsGroupSettings_segmentLength' - Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 10. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (HlsSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
--
-- 'segmentLengthControl', 'hlsGroupSettings_segmentLengthControl' - Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'segmentsPerSubdirectory', 'hlsGroupSettings_segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
-- effect.
--
-- 'streamInfResolution', 'hlsGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
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
-- 'timedMetadataId3Frame', 'hlsGroupSettings_timedMetadataId3Frame' - Specify the type of the ID3 frame (timedMetadataId3Frame) to use for ID3
-- timestamps (timedMetadataId3Period) in your output. To include ID3
-- timestamps: Specify PRIV (PRIV) or TDRL (TDRL) and set ID3 metadata
-- (timedMetadata) to Passthrough (PASSTHROUGH). To exclude ID3 timestamps:
-- Set ID3 timestamp frame type to None (NONE).
--
-- 'timedMetadataId3Period', 'hlsGroupSettings_timedMetadataId3Period' - Specify the interval in seconds to write ID3 timestamps in your output.
-- The first timestamp starts at the output timecode and date, and
-- increases incrementally with each ID3 timestamp. To use the default
-- interval of 10 seconds: Leave blank. To include this metadata in your
-- output: Set ID3 timestamp frame type (timedMetadataId3Frame) to PRIV
-- (PRIV) or TDRL (TDRL), and set ID3 metadata (timedMetadata) to
-- Passthrough (PASSTHROUGH).
--
-- 'timestampDeltaMilliseconds', 'hlsGroupSettings_timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
newHlsGroupSettings ::
  HlsGroupSettings
newHlsGroupSettings =
  HlsGroupSettings'
    { adMarkers = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      audioOnlyHeader = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      captionLanguageMappings = Prelude.Nothing,
      captionLanguageSetting = Prelude.Nothing,
      captionSegmentLengthControl = Prelude.Nothing,
      clientCache = Prelude.Nothing,
      codecSpecification = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      directoryStructure = Prelude.Nothing,
      encryption = Prelude.Nothing,
      imageBasedTrickPlay = Prelude.Nothing,
      imageBasedTrickPlaySettings = Prelude.Nothing,
      manifestCompression = Prelude.Nothing,
      manifestDurationFormat = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      minSegmentLength = Prelude.Nothing,
      outputSelection = Prelude.Nothing,
      programDateTime = Prelude.Nothing,
      programDateTimePeriod = Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      segmentLength = Prelude.Nothing,
      segmentLengthControl = Prelude.Nothing,
      segmentsPerSubdirectory = Prelude.Nothing,
      streamInfResolution = Prelude.Nothing,
      targetDurationCompatibilityMode = Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      timedMetadataId3Period = Prelude.Nothing,
      timestampDeltaMilliseconds = Prelude.Nothing
    }

-- | Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens.coerced

-- | By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
hlsGroupSettings_additionalManifests :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdditionalManifest])
hlsGroupSettings_additionalManifests = Lens.lens (\HlsGroupSettings' {additionalManifests} -> additionalManifests) (\s@HlsGroupSettings' {} a -> s {additionalManifests = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens.coerced

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
hlsGroupSettings_audioOnlyHeader :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsAudioOnlyHeader)
hlsGroupSettings_audioOnlyHeader = Lens.lens (\HlsGroupSettings' {audioOnlyHeader} -> audioOnlyHeader) (\s@HlsGroupSettings' {} a -> s {audioOnlyHeader = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrl = Lens.lens (\HlsGroupSettings' {baseUrl} -> baseUrl) (\s@HlsGroupSettings' {} a -> s {baseUrl = a} :: HlsGroupSettings)

-- | Language to be used on Caption outputs
hlsGroupSettings_captionLanguageMappings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsCaptionLanguageMapping])
hlsGroupSettings_captionLanguageMappings = Lens.lens (\HlsGroupSettings' {captionLanguageMappings} -> captionLanguageMappings) (\s@HlsGroupSettings' {} a -> s {captionLanguageMappings = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens.coerced

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

-- | Set Caption segment length control (CaptionSegmentLengthControl) to
-- Match video (MATCH_VIDEO) to create caption segments that align with the
-- video segments from the first video output in this output group. For
-- example, if the video segments are 2 seconds long, your WebVTT segments
-- will also be 2 seconds long. Keep the default setting, Large segments
-- (LARGE_SEGMENTS) to create caption segments that are 300 seconds long.
hlsGroupSettings_captionSegmentLengthControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCaptionSegmentLengthControl)
hlsGroupSettings_captionSegmentLengthControl = Lens.lens (\HlsGroupSettings' {captionSegmentLengthControl} -> captionSegmentLengthControl) (\s@HlsGroupSettings' {} a -> s {captionSegmentLengthControl = a} :: HlsGroupSettings)

-- | Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
hlsGroupSettings_clientCache :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsClientCache)
hlsGroupSettings_clientCache = Lens.lens (\HlsGroupSettings' {clientCache} -> clientCache) (\s@HlsGroupSettings' {} a -> s {clientCache = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
hlsGroupSettings_destination :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_destination = Lens.lens (\HlsGroupSettings' {destination} -> destination) (\s@HlsGroupSettings' {} a -> s {destination = a} :: HlsGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
hlsGroupSettings_destinationSettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe DestinationSettings)
hlsGroupSettings_destinationSettings = Lens.lens (\HlsGroupSettings' {destinationSettings} -> destinationSettings) (\s@HlsGroupSettings' {} a -> s {destinationSettings = a} :: HlsGroupSettings)

-- | Indicates whether segments should be placed in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | DRM settings.
hlsGroupSettings_encryption :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsEncryptionSettings)
hlsGroupSettings_encryption = Lens.lens (\HlsGroupSettings' {encryption} -> encryption) (\s@HlsGroupSettings' {} a -> s {encryption = a} :: HlsGroupSettings)

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

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
hlsGroupSettings_imageBasedTrickPlaySettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsImageBasedTrickPlaySettings)
hlsGroupSettings_imageBasedTrickPlaySettings = Lens.lens (\HlsGroupSettings' {imageBasedTrickPlaySettings} -> imageBasedTrickPlaySettings) (\s@HlsGroupSettings' {} a -> s {imageBasedTrickPlaySettings = a} :: HlsGroupSettings)

-- | When set to GZIP, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestDurationFormat)
hlsGroupSettings_manifestDurationFormat = Lens.lens (\HlsGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@HlsGroupSettings' {} a -> s {manifestDurationFormat = a} :: HlsGroupSettings)

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

-- | When set, Minimum Segment Size is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
hlsGroupSettings_minSegmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_minSegmentLength = Lens.lens (\HlsGroupSettings' {minSegmentLength} -> minSegmentLength) (\s@HlsGroupSettings' {} a -> s {minSegmentLength = a} :: HlsGroupSettings)

-- | Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hlsGroupSettings_programDateTimePeriod :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_programDateTimePeriod = Lens.lens (\HlsGroupSettings' {programDateTimePeriod} -> programDateTimePeriod) (\s@HlsGroupSettings' {} a -> s {programDateTimePeriod = a} :: HlsGroupSettings)

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
hlsGroupSettings_segmentControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentControl)
hlsGroupSettings_segmentControl = Lens.lens (\HlsGroupSettings' {segmentControl} -> segmentControl) (\s@HlsGroupSettings' {} a -> s {segmentControl = a} :: HlsGroupSettings)

-- | Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 10. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (HlsSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
hlsGroupSettings_segmentLengthControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentLengthControl)
hlsGroupSettings_segmentLengthControl = Lens.lens (\HlsGroupSettings' {segmentLengthControl} -> segmentLengthControl) (\s@HlsGroupSettings' {} a -> s {segmentLengthControl = a} :: HlsGroupSettings)

-- | Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
-- effect.
hlsGroupSettings_segmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentsPerSubdirectory = Lens.lens (\HlsGroupSettings' {segmentsPerSubdirectory} -> segmentsPerSubdirectory) (\s@HlsGroupSettings' {} a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

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

-- | Specify the type of the ID3 frame (timedMetadataId3Frame) to use for ID3
-- timestamps (timedMetadataId3Period) in your output. To include ID3
-- timestamps: Specify PRIV (PRIV) or TDRL (TDRL) and set ID3 metadata
-- (timedMetadata) to Passthrough (PASSTHROUGH). To exclude ID3 timestamps:
-- Set ID3 timestamp frame type to None (NONE).
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Specify the interval in seconds to write ID3 timestamps in your output.
-- The first timestamp starts at the output timecode and date, and
-- increases incrementally with each ID3 timestamp. To use the default
-- interval of 10 seconds: Leave blank. To include this metadata in your
-- output: Set ID3 timestamp frame type (timedMetadataId3Frame) to PRIV
-- (PRIV) or TDRL (TDRL), and set ID3 metadata (timedMetadata) to
-- Passthrough (PASSTHROUGH).
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Int)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hlsGroupSettings_timestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Int)
hlsGroupSettings_timestampDeltaMilliseconds = Lens.lens (\HlsGroupSettings' {timestampDeltaMilliseconds} -> timestampDeltaMilliseconds) (\s@HlsGroupSettings' {} a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)

instance Data.FromJSON HlsGroupSettings where
  parseJSON =
    Data.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Prelude.<$> (x Data..:? "adMarkers" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "additionalManifests"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "audioOnlyHeader")
            Prelude.<*> (x Data..:? "baseUrl")
            Prelude.<*> ( x Data..:? "captionLanguageMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "captionLanguageSetting")
            Prelude.<*> (x Data..:? "captionSegmentLengthControl")
            Prelude.<*> (x Data..:? "clientCache")
            Prelude.<*> (x Data..:? "codecSpecification")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "destinationSettings")
            Prelude.<*> (x Data..:? "directoryStructure")
            Prelude.<*> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "imageBasedTrickPlay")
            Prelude.<*> (x Data..:? "imageBasedTrickPlaySettings")
            Prelude.<*> (x Data..:? "manifestCompression")
            Prelude.<*> (x Data..:? "manifestDurationFormat")
            Prelude.<*> (x Data..:? "minFinalSegmentLength")
            Prelude.<*> (x Data..:? "minSegmentLength")
            Prelude.<*> (x Data..:? "outputSelection")
            Prelude.<*> (x Data..:? "programDateTime")
            Prelude.<*> (x Data..:? "programDateTimePeriod")
            Prelude.<*> (x Data..:? "segmentControl")
            Prelude.<*> (x Data..:? "segmentLength")
            Prelude.<*> (x Data..:? "segmentLengthControl")
            Prelude.<*> (x Data..:? "segmentsPerSubdirectory")
            Prelude.<*> (x Data..:? "streamInfResolution")
            Prelude.<*> (x Data..:? "targetDurationCompatibilityMode")
            Prelude.<*> (x Data..:? "timedMetadataId3Frame")
            Prelude.<*> (x Data..:? "timedMetadataId3Period")
            Prelude.<*> (x Data..:? "timestampDeltaMilliseconds")
      )

instance Prelude.Hashable HlsGroupSettings where
  hashWithSalt _salt HlsGroupSettings' {..} =
    _salt `Prelude.hashWithSalt` adMarkers
      `Prelude.hashWithSalt` additionalManifests
      `Prelude.hashWithSalt` audioOnlyHeader
      `Prelude.hashWithSalt` baseUrl
      `Prelude.hashWithSalt` captionLanguageMappings
      `Prelude.hashWithSalt` captionLanguageSetting
      `Prelude.hashWithSalt` captionSegmentLengthControl
      `Prelude.hashWithSalt` clientCache
      `Prelude.hashWithSalt` codecSpecification
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationSettings
      `Prelude.hashWithSalt` directoryStructure
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` imageBasedTrickPlay
      `Prelude.hashWithSalt` imageBasedTrickPlaySettings
      `Prelude.hashWithSalt` manifestCompression
      `Prelude.hashWithSalt` manifestDurationFormat
      `Prelude.hashWithSalt` minFinalSegmentLength
      `Prelude.hashWithSalt` minSegmentLength
      `Prelude.hashWithSalt` outputSelection
      `Prelude.hashWithSalt` programDateTime
      `Prelude.hashWithSalt` programDateTimePeriod
      `Prelude.hashWithSalt` segmentControl
      `Prelude.hashWithSalt` segmentLength
      `Prelude.hashWithSalt` segmentLengthControl
      `Prelude.hashWithSalt` segmentsPerSubdirectory
      `Prelude.hashWithSalt` streamInfResolution
      `Prelude.hashWithSalt` targetDurationCompatibilityMode
      `Prelude.hashWithSalt` timedMetadataId3Frame
      `Prelude.hashWithSalt` timedMetadataId3Period
      `Prelude.hashWithSalt` timestampDeltaMilliseconds

instance Prelude.NFData HlsGroupSettings where
  rnf HlsGroupSettings' {..} =
    Prelude.rnf adMarkers
      `Prelude.seq` Prelude.rnf additionalManifests
      `Prelude.seq` Prelude.rnf audioOnlyHeader
      `Prelude.seq` Prelude.rnf baseUrl
      `Prelude.seq` Prelude.rnf captionLanguageMappings
      `Prelude.seq` Prelude.rnf captionLanguageSetting
      `Prelude.seq` Prelude.rnf captionSegmentLengthControl
      `Prelude.seq` Prelude.rnf clientCache
      `Prelude.seq` Prelude.rnf codecSpecification
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationSettings
      `Prelude.seq` Prelude.rnf directoryStructure
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf imageBasedTrickPlay
      `Prelude.seq` Prelude.rnf imageBasedTrickPlaySettings
      `Prelude.seq` Prelude.rnf manifestCompression
      `Prelude.seq` Prelude.rnf manifestDurationFormat
      `Prelude.seq` Prelude.rnf minFinalSegmentLength
      `Prelude.seq` Prelude.rnf minSegmentLength
      `Prelude.seq` Prelude.rnf outputSelection
      `Prelude.seq` Prelude.rnf programDateTime
      `Prelude.seq` Prelude.rnf
        programDateTimePeriod
      `Prelude.seq` Prelude.rnf
        segmentControl
      `Prelude.seq` Prelude.rnf
        segmentLength
      `Prelude.seq` Prelude.rnf
        segmentLengthControl
      `Prelude.seq` Prelude.rnf
        segmentsPerSubdirectory
      `Prelude.seq` Prelude.rnf
        streamInfResolution
      `Prelude.seq` Prelude.rnf
        targetDurationCompatibilityMode
      `Prelude.seq` Prelude.rnf
        timedMetadataId3Frame
      `Prelude.seq` Prelude.rnf
        timedMetadataId3Period
      `Prelude.seq` Prelude.rnf
        timestampDeltaMilliseconds

instance Data.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adMarkers" Data..=) Prelude.<$> adMarkers,
            ("additionalManifests" Data..=)
              Prelude.<$> additionalManifests,
            ("audioOnlyHeader" Data..=)
              Prelude.<$> audioOnlyHeader,
            ("baseUrl" Data..=) Prelude.<$> baseUrl,
            ("captionLanguageMappings" Data..=)
              Prelude.<$> captionLanguageMappings,
            ("captionLanguageSetting" Data..=)
              Prelude.<$> captionLanguageSetting,
            ("captionSegmentLengthControl" Data..=)
              Prelude.<$> captionSegmentLengthControl,
            ("clientCache" Data..=) Prelude.<$> clientCache,
            ("codecSpecification" Data..=)
              Prelude.<$> codecSpecification,
            ("destination" Data..=) Prelude.<$> destination,
            ("destinationSettings" Data..=)
              Prelude.<$> destinationSettings,
            ("directoryStructure" Data..=)
              Prelude.<$> directoryStructure,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("imageBasedTrickPlay" Data..=)
              Prelude.<$> imageBasedTrickPlay,
            ("imageBasedTrickPlaySettings" Data..=)
              Prelude.<$> imageBasedTrickPlaySettings,
            ("manifestCompression" Data..=)
              Prelude.<$> manifestCompression,
            ("manifestDurationFormat" Data..=)
              Prelude.<$> manifestDurationFormat,
            ("minFinalSegmentLength" Data..=)
              Prelude.<$> minFinalSegmentLength,
            ("minSegmentLength" Data..=)
              Prelude.<$> minSegmentLength,
            ("outputSelection" Data..=)
              Prelude.<$> outputSelection,
            ("programDateTime" Data..=)
              Prelude.<$> programDateTime,
            ("programDateTimePeriod" Data..=)
              Prelude.<$> programDateTimePeriod,
            ("segmentControl" Data..=)
              Prelude.<$> segmentControl,
            ("segmentLength" Data..=) Prelude.<$> segmentLength,
            ("segmentLengthControl" Data..=)
              Prelude.<$> segmentLengthControl,
            ("segmentsPerSubdirectory" Data..=)
              Prelude.<$> segmentsPerSubdirectory,
            ("streamInfResolution" Data..=)
              Prelude.<$> streamInfResolution,
            ("targetDurationCompatibilityMode" Data..=)
              Prelude.<$> targetDurationCompatibilityMode,
            ("timedMetadataId3Frame" Data..=)
              Prelude.<$> timedMetadataId3Frame,
            ("timedMetadataId3Period" Data..=)
              Prelude.<$> timedMetadataId3Period,
            ("timestampDeltaMilliseconds" Data..=)
              Prelude.<$> timestampDeltaMilliseconds
          ]
      )

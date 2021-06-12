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
import Network.AWS.MediaConvert.Types.HlsManifestCompression
import Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
import Network.AWS.MediaConvert.Types.HlsOutputSelection
import Network.AWS.MediaConvert.Types.HlsProgramDateTime
import Network.AWS.MediaConvert.Types.HlsSegmentControl
import Network.AWS.MediaConvert.Types.HlsStreamInfResolution
import Network.AWS.MediaConvert.Types.HlsTimedMetadataId3Frame

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to HLS_GROUP_SETTINGS.
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Indicates whether the .m3u8 manifest file should be generated for this
    -- HLS output group.
    outputSelection :: Core.Maybe HlsOutputSelection,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Core.Maybe Core.Int,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
    -- that segments will end on the next keyframe after this number of
    -- seconds, so actual segment length may be longer.
    segmentLength :: Core.Maybe Core.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Core.Maybe HlsTimedMetadataId3Frame,
    -- | Choose one or more ad marker types to decorate your Apple HLS manifest.
    -- This setting does not determine whether SCTE-35 markers appear in the
    -- outputs themselves.
    adMarkers :: Core.Maybe [HlsAdMarkers],
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
    -- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Core.Maybe HlsSegmentControl,
    -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Core.Maybe HlsDirectoryStructure,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Core.Maybe HlsManifestCompression,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrl :: Core.Maybe Core.Text,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Core.Maybe HlsStreamInfResolution,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Core.Maybe HlsCodecSpecification,
    -- | By default, the service creates one top-level .m3u8 HLS manifest for
    -- each HLS output group in your job. This default manifest references
    -- every output in the output group. To create additional top-level
    -- manifests that reference a subset of the outputs in the output group,
    -- specify a list of them here.
    additionalManifests :: Core.Maybe [HlsAdditionalManifest],
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated as follows: either the program date and
    -- time are initialized using the input timecode source, or the time is
    -- initialized using the input timecode source and the date is initialized
    -- using the timestamp_offset.
    programDateTime :: Core.Maybe HlsProgramDateTime,
    -- | Number of segments to write to a subdirectory before starting a new one.
    -- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
    -- effect.
    segmentsPerSubdirectory :: Core.Maybe Core.Natural,
    -- | DRM settings.
    encryption :: Core.Maybe HlsEncryptionSettings,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
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
    minFinalSegmentLength :: Core.Maybe Core.Double,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Core.Maybe DestinationSettings,
    -- | Language to be used on Caption outputs
    captionLanguageMappings :: Core.Maybe [HlsCaptionLanguageMapping],
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Core.Maybe Core.Int,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Core.Maybe Core.Natural,
    -- | Disable this setting only when your workflow requires the
    -- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
    -- (ENABLED) and control caching in your video distribution set up. For
    -- example, use the Cache-Control http header.
    clientCache :: Core.Maybe HlsClientCache,
    -- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
    -- and you encounter playback issues. Keep the default value, Include
    -- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
    -- remove the audio-only headers from your audio segments.
    audioOnlyHeader :: Core.Maybe HlsAudioOnlyHeader,
    -- | When set, Minimum Segment Size is enforced by looking ahead and back
    -- within the specified range for a nearby avail and extending the segment
    -- size if needed.
    minSegmentLength :: Core.Maybe Core.Natural,
    -- | Indicates whether the output manifest should use floating point values
    -- for segment duration.
    manifestDurationFormat :: Core.Maybe HlsManifestDurationFormat,
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
    captionLanguageSetting :: Core.Maybe HlsCaptionLanguageSetting
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSelection', 'hlsGroupSettings_outputSelection' - Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
--
-- 'timedMetadataId3Period', 'hlsGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- 'segmentLength', 'hlsGroupSettings_segmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note
-- that segments will end on the next keyframe after this number of
-- seconds, so actual segment length may be longer.
--
-- 'timedMetadataId3Frame', 'hlsGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'adMarkers', 'hlsGroupSettings_adMarkers' - Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
--
-- 'segmentControl', 'hlsGroupSettings_segmentControl' - When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- 'directoryStructure', 'hlsGroupSettings_directoryStructure' - Indicates whether segments should be placed in subdirectories.
--
-- 'manifestCompression', 'hlsGroupSettings_manifestCompression' - When set to GZIP, compresses HLS playlist.
--
-- 'baseUrl', 'hlsGroupSettings_baseUrl' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'streamInfResolution', 'hlsGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
--
-- 'codecSpecification', 'hlsGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'additionalManifests', 'hlsGroupSettings_additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
--
-- 'programDateTime', 'hlsGroupSettings_programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
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
-- 'manifestDurationFormat', 'hlsGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point values
-- for segment duration.
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
newHlsGroupSettings ::
  HlsGroupSettings
newHlsGroupSettings =
  HlsGroupSettings'
    { outputSelection = Core.Nothing,
      timedMetadataId3Period = Core.Nothing,
      segmentLength = Core.Nothing,
      timedMetadataId3Frame = Core.Nothing,
      adMarkers = Core.Nothing,
      segmentControl = Core.Nothing,
      directoryStructure = Core.Nothing,
      manifestCompression = Core.Nothing,
      baseUrl = Core.Nothing,
      streamInfResolution = Core.Nothing,
      codecSpecification = Core.Nothing,
      additionalManifests = Core.Nothing,
      programDateTime = Core.Nothing,
      segmentsPerSubdirectory = Core.Nothing,
      encryption = Core.Nothing,
      destination = Core.Nothing,
      minFinalSegmentLength = Core.Nothing,
      destinationSettings = Core.Nothing,
      captionLanguageMappings = Core.Nothing,
      timestampDeltaMilliseconds = Core.Nothing,
      programDateTimePeriod = Core.Nothing,
      clientCache = Core.Nothing,
      audioOnlyHeader = Core.Nothing,
      minSegmentLength = Core.Nothing,
      manifestDurationFormat = Core.Nothing,
      captionLanguageSetting = Core.Nothing
    }

-- | Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | Timed Metadata interval in seconds.
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Int)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
-- that segments will end on the next keyframe after this number of
-- seconds, so actual segment length may be longer.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | Indicates ID3 frame that has the timecode.
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Core.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
hlsGroupSettings_segmentControl :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsSegmentControl)
hlsGroupSettings_segmentControl = Lens.lens (\HlsGroupSettings' {segmentControl} -> segmentControl) (\s@HlsGroupSettings' {} a -> s {segmentControl = a} :: HlsGroupSettings)

-- | Indicates whether segments should be placed in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | When set to GZIP, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrl :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_baseUrl = Lens.lens (\HlsGroupSettings' {baseUrl} -> baseUrl) (\s@HlsGroupSettings' {} a -> s {baseUrl = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
hlsGroupSettings_additionalManifests :: Lens.Lens' HlsGroupSettings (Core.Maybe [HlsAdditionalManifest])
hlsGroupSettings_additionalManifests = Lens.lens (\HlsGroupSettings' {additionalManifests} -> additionalManifests) (\s@HlsGroupSettings' {} a -> s {additionalManifests = a} :: HlsGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

-- | Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be SINGLE_DIRECTORY for this setting to have an
-- effect.
hlsGroupSettings_segmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_segmentsPerSubdirectory = Lens.lens (\HlsGroupSettings' {segmentsPerSubdirectory} -> segmentsPerSubdirectory) (\s@HlsGroupSettings' {} a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)

-- | DRM settings.
hlsGroupSettings_encryption :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsEncryptionSettings)
hlsGroupSettings_encryption = Lens.lens (\HlsGroupSettings' {encryption} -> encryption) (\s@HlsGroupSettings' {} a -> s {encryption = a} :: HlsGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
hlsGroupSettings_destination :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
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
hlsGroupSettings_minFinalSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Double)
hlsGroupSettings_minFinalSegmentLength = Lens.lens (\HlsGroupSettings' {minFinalSegmentLength} -> minFinalSegmentLength) (\s@HlsGroupSettings' {} a -> s {minFinalSegmentLength = a} :: HlsGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
hlsGroupSettings_destinationSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe DestinationSettings)
hlsGroupSettings_destinationSettings = Lens.lens (\HlsGroupSettings' {destinationSettings} -> destinationSettings) (\s@HlsGroupSettings' {} a -> s {destinationSettings = a} :: HlsGroupSettings)

-- | Language to be used on Caption outputs
hlsGroupSettings_captionLanguageMappings :: Lens.Lens' HlsGroupSettings (Core.Maybe [HlsCaptionLanguageMapping])
hlsGroupSettings_captionLanguageMappings = Lens.lens (\HlsGroupSettings' {captionLanguageMappings} -> captionLanguageMappings) (\s@HlsGroupSettings' {} a -> s {captionLanguageMappings = a} :: HlsGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hlsGroupSettings_timestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Int)
hlsGroupSettings_timestampDeltaMilliseconds = Lens.lens (\HlsGroupSettings' {timestampDeltaMilliseconds} -> timestampDeltaMilliseconds) (\s@HlsGroupSettings' {} a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hlsGroupSettings_programDateTimePeriod :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_programDateTimePeriod = Lens.lens (\HlsGroupSettings' {programDateTimePeriod} -> programDateTimePeriod) (\s@HlsGroupSettings' {} a -> s {programDateTimePeriod = a} :: HlsGroupSettings)

-- | Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
hlsGroupSettings_clientCache :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsClientCache)
hlsGroupSettings_clientCache = Lens.lens (\HlsGroupSettings' {clientCache} -> clientCache) (\s@HlsGroupSettings' {} a -> s {clientCache = a} :: HlsGroupSettings)

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix
-- and you encounter playback issues. Keep the default value, Include
-- (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to
-- remove the audio-only headers from your audio segments.
hlsGroupSettings_audioOnlyHeader :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsAudioOnlyHeader)
hlsGroupSettings_audioOnlyHeader = Lens.lens (\HlsGroupSettings' {audioOnlyHeader} -> audioOnlyHeader) (\s@HlsGroupSettings' {} a -> s {audioOnlyHeader = a} :: HlsGroupSettings)

-- | When set, Minimum Segment Size is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
hlsGroupSettings_minSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_minSegmentLength = Lens.lens (\HlsGroupSettings' {minSegmentLength} -> minSegmentLength) (\s@HlsGroupSettings' {} a -> s {minSegmentLength = a} :: HlsGroupSettings)

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsManifestDurationFormat)
hlsGroupSettings_manifestDurationFormat = Lens.lens (\HlsGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@HlsGroupSettings' {} a -> s {manifestDurationFormat = a} :: HlsGroupSettings)

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
hlsGroupSettings_captionLanguageSetting :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsCaptionLanguageSetting)
hlsGroupSettings_captionLanguageSetting = Lens.lens (\HlsGroupSettings' {captionLanguageSetting} -> captionLanguageSetting) (\s@HlsGroupSettings' {} a -> s {captionLanguageSetting = a} :: HlsGroupSettings)

instance Core.FromJSON HlsGroupSettings where
  parseJSON =
    Core.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Core.<$> (x Core..:? "outputSelection")
            Core.<*> (x Core..:? "timedMetadataId3Period")
            Core.<*> (x Core..:? "segmentLength")
            Core.<*> (x Core..:? "timedMetadataId3Frame")
            Core.<*> (x Core..:? "adMarkers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "segmentControl")
            Core.<*> (x Core..:? "directoryStructure")
            Core.<*> (x Core..:? "manifestCompression")
            Core.<*> (x Core..:? "baseUrl")
            Core.<*> (x Core..:? "streamInfResolution")
            Core.<*> (x Core..:? "codecSpecification")
            Core.<*> ( x Core..:? "additionalManifests"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "programDateTime")
            Core.<*> (x Core..:? "segmentsPerSubdirectory")
            Core.<*> (x Core..:? "encryption")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "minFinalSegmentLength")
            Core.<*> (x Core..:? "destinationSettings")
            Core.<*> ( x Core..:? "captionLanguageMappings"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "timestampDeltaMilliseconds")
            Core.<*> (x Core..:? "programDateTimePeriod")
            Core.<*> (x Core..:? "clientCache")
            Core.<*> (x Core..:? "audioOnlyHeader")
            Core.<*> (x Core..:? "minSegmentLength")
            Core.<*> (x Core..:? "manifestDurationFormat")
            Core.<*> (x Core..:? "captionLanguageSetting")
      )

instance Core.Hashable HlsGroupSettings

instance Core.NFData HlsGroupSettings

instance Core.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputSelection" Core..=)
              Core.<$> outputSelection,
            ("timedMetadataId3Period" Core..=)
              Core.<$> timedMetadataId3Period,
            ("segmentLength" Core..=) Core.<$> segmentLength,
            ("timedMetadataId3Frame" Core..=)
              Core.<$> timedMetadataId3Frame,
            ("adMarkers" Core..=) Core.<$> adMarkers,
            ("segmentControl" Core..=) Core.<$> segmentControl,
            ("directoryStructure" Core..=)
              Core.<$> directoryStructure,
            ("manifestCompression" Core..=)
              Core.<$> manifestCompression,
            ("baseUrl" Core..=) Core.<$> baseUrl,
            ("streamInfResolution" Core..=)
              Core.<$> streamInfResolution,
            ("codecSpecification" Core..=)
              Core.<$> codecSpecification,
            ("additionalManifests" Core..=)
              Core.<$> additionalManifests,
            ("programDateTime" Core..=) Core.<$> programDateTime,
            ("segmentsPerSubdirectory" Core..=)
              Core.<$> segmentsPerSubdirectory,
            ("encryption" Core..=) Core.<$> encryption,
            ("destination" Core..=) Core.<$> destination,
            ("minFinalSegmentLength" Core..=)
              Core.<$> minFinalSegmentLength,
            ("destinationSettings" Core..=)
              Core.<$> destinationSettings,
            ("captionLanguageMappings" Core..=)
              Core.<$> captionLanguageMappings,
            ("timestampDeltaMilliseconds" Core..=)
              Core.<$> timestampDeltaMilliseconds,
            ("programDateTimePeriod" Core..=)
              Core.<$> programDateTimePeriod,
            ("clientCache" Core..=) Core.<$> clientCache,
            ("audioOnlyHeader" Core..=) Core.<$> audioOnlyHeader,
            ("minSegmentLength" Core..=)
              Core.<$> minSegmentLength,
            ("manifestDurationFormat" Core..=)
              Core.<$> manifestDurationFormat,
            ("captionLanguageSetting" Core..=)
              Core.<$> captionLanguageSetting
          ]
      )

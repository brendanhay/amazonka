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
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to HLS_GROUP_SETTINGS.
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Indicates whether the .m3u8 manifest file should be generated for this
    -- HLS output group.
    outputSelection :: Prelude.Maybe HlsOutputSelection,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Prelude.Maybe Prelude.Int,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
    -- that segments will end on the next keyframe after this number of
    -- seconds, so actual segment length may be longer.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Prelude.Maybe HlsTimedMetadataId3Frame,
    -- | Choose one or more ad marker types to decorate your Apple HLS manifest.
    -- This setting does not determine whether SCTE-35 markers appear in the
    -- outputs themselves.
    adMarkers :: Prelude.Maybe [HlsAdMarkers],
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
    -- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Prelude.Maybe HlsSegmentControl,
    -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Prelude.Maybe HlsDirectoryStructure,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Prelude.Maybe HlsManifestCompression,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Prelude.Maybe HlsStreamInfResolution,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Prelude.Maybe HlsCodecSpecification,
    -- | By default, the service creates one top-level .m3u8 HLS manifest for
    -- each HLS output group in your job. This default manifest references
    -- every output in the output group. To create additional top-level
    -- manifests that reference a subset of the outputs in the output group,
    -- specify a list of them here.
    additionalManifests :: Prelude.Maybe [HlsAdditionalManifest],
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated as follows: either the program date and
    -- time are initialized using the input timecode source, or the time is
    -- initialized using the input timecode source and the date is initialized
    -- using the timestamp_offset.
    programDateTime :: Prelude.Maybe HlsProgramDateTime,
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
    -- | Indicates whether the output manifest should use floating point values
    -- for segment duration.
    manifestDurationFormat :: Prelude.Maybe HlsManifestDurationFormat,
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
    captionLanguageSetting :: Prelude.Maybe HlsCaptionLanguageSetting
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
    { outputSelection =
        Prelude.Nothing,
      timedMetadataId3Period = Prelude.Nothing,
      segmentLength = Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      adMarkers = Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      directoryStructure = Prelude.Nothing,
      manifestCompression = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      streamInfResolution = Prelude.Nothing,
      codecSpecification = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      programDateTime = Prelude.Nothing,
      segmentsPerSubdirectory = Prelude.Nothing,
      encryption = Prelude.Nothing,
      destination = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      captionLanguageMappings = Prelude.Nothing,
      timestampDeltaMilliseconds = Prelude.Nothing,
      programDateTimePeriod = Prelude.Nothing,
      clientCache = Prelude.Nothing,
      audioOnlyHeader = Prelude.Nothing,
      minSegmentLength = Prelude.Nothing,
      manifestDurationFormat = Prelude.Nothing,
      captionLanguageSetting = Prelude.Nothing
    }

-- | Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | Timed Metadata interval in seconds.
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Int)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
-- that segments will end on the next keyframe after this number of
-- seconds, so actual segment length may be longer.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | Indicates ID3 frame that has the timecode.
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Choose one or more ad marker types to decorate your Apple HLS manifest.
-- This setting does not determine whether SCTE-35 markers appear in the
-- outputs themselves.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts)
-- file, uses #EXT-X-BYTERANGE tags to index segment for playback.
hlsGroupSettings_segmentControl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentControl)
hlsGroupSettings_segmentControl = Lens.lens (\HlsGroupSettings' {segmentControl} -> segmentControl) (\s@HlsGroupSettings' {} a -> s {segmentControl = a} :: HlsGroupSettings)

-- | Indicates whether segments should be placed in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | When set to GZIP, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrl :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrl = Lens.lens (\HlsGroupSettings' {baseUrl} -> baseUrl) (\s@HlsGroupSettings' {} a -> s {baseUrl = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | By default, the service creates one top-level .m3u8 HLS manifest for
-- each HLS output group in your job. This default manifest references
-- every output in the output group. To create additional top-level
-- manifests that reference a subset of the outputs in the output group,
-- specify a list of them here.
hlsGroupSettings_additionalManifests :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdditionalManifest])
hlsGroupSettings_additionalManifests = Lens.lens (\HlsGroupSettings' {additionalManifests} -> additionalManifests) (\s@HlsGroupSettings' {} a -> s {additionalManifests = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestamp_offset.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

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

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestDurationFormat)
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
hlsGroupSettings_captionLanguageSetting :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCaptionLanguageSetting)
hlsGroupSettings_captionLanguageSetting = Lens.lens (\HlsGroupSettings' {captionLanguageSetting} -> captionLanguageSetting) (\s@HlsGroupSettings' {} a -> s {captionLanguageSetting = a} :: HlsGroupSettings)

instance Core.FromJSON HlsGroupSettings where
  parseJSON =
    Core.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Prelude.<$> (x Core..:? "outputSelection")
            Prelude.<*> (x Core..:? "timedMetadataId3Period")
            Prelude.<*> (x Core..:? "segmentLength")
            Prelude.<*> (x Core..:? "timedMetadataId3Frame")
            Prelude.<*> (x Core..:? "adMarkers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "segmentControl")
            Prelude.<*> (x Core..:? "directoryStructure")
            Prelude.<*> (x Core..:? "manifestCompression")
            Prelude.<*> (x Core..:? "baseUrl")
            Prelude.<*> (x Core..:? "streamInfResolution")
            Prelude.<*> (x Core..:? "codecSpecification")
            Prelude.<*> ( x Core..:? "additionalManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "programDateTime")
            Prelude.<*> (x Core..:? "segmentsPerSubdirectory")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "minFinalSegmentLength")
            Prelude.<*> (x Core..:? "destinationSettings")
            Prelude.<*> ( x Core..:? "captionLanguageMappings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timestampDeltaMilliseconds")
            Prelude.<*> (x Core..:? "programDateTimePeriod")
            Prelude.<*> (x Core..:? "clientCache")
            Prelude.<*> (x Core..:? "audioOnlyHeader")
            Prelude.<*> (x Core..:? "minSegmentLength")
            Prelude.<*> (x Core..:? "manifestDurationFormat")
            Prelude.<*> (x Core..:? "captionLanguageSetting")
      )

instance Prelude.Hashable HlsGroupSettings

instance Prelude.NFData HlsGroupSettings

instance Core.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("outputSelection" Core..=)
              Prelude.<$> outputSelection,
            ("timedMetadataId3Period" Core..=)
              Prelude.<$> timedMetadataId3Period,
            ("segmentLength" Core..=) Prelude.<$> segmentLength,
            ("timedMetadataId3Frame" Core..=)
              Prelude.<$> timedMetadataId3Frame,
            ("adMarkers" Core..=) Prelude.<$> adMarkers,
            ("segmentControl" Core..=)
              Prelude.<$> segmentControl,
            ("directoryStructure" Core..=)
              Prelude.<$> directoryStructure,
            ("manifestCompression" Core..=)
              Prelude.<$> manifestCompression,
            ("baseUrl" Core..=) Prelude.<$> baseUrl,
            ("streamInfResolution" Core..=)
              Prelude.<$> streamInfResolution,
            ("codecSpecification" Core..=)
              Prelude.<$> codecSpecification,
            ("additionalManifests" Core..=)
              Prelude.<$> additionalManifests,
            ("programDateTime" Core..=)
              Prelude.<$> programDateTime,
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
            ("programDateTimePeriod" Core..=)
              Prelude.<$> programDateTimePeriod,
            ("clientCache" Core..=) Prelude.<$> clientCache,
            ("audioOnlyHeader" Core..=)
              Prelude.<$> audioOnlyHeader,
            ("minSegmentLength" Core..=)
              Prelude.<$> minSegmentLength,
            ("manifestDurationFormat" Core..=)
              Prelude.<$> manifestDurationFormat,
            ("captionLanguageSetting" Core..=)
              Prelude.<$> captionLanguageSetting
          ]
      )

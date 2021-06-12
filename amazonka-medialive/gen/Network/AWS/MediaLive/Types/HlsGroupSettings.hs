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
-- Module      : Network.AWS.MediaLive.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionLanguageMapping
import Network.AWS.MediaLive.Types.HlsAdMarkers
import Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
import Network.AWS.MediaLive.Types.HlsCdnSettings
import Network.AWS.MediaLive.Types.HlsClientCache
import Network.AWS.MediaLive.Types.HlsCodecSpecification
import Network.AWS.MediaLive.Types.HlsDirectoryStructure
import Network.AWS.MediaLive.Types.HlsDiscontinuityTags
import Network.AWS.MediaLive.Types.HlsEncryptionType
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
import Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
import Network.AWS.MediaLive.Types.HlsIvInManifest
import Network.AWS.MediaLive.Types.HlsIvSource
import Network.AWS.MediaLive.Types.HlsManifestCompression
import Network.AWS.MediaLive.Types.HlsManifestDurationFormat
import Network.AWS.MediaLive.Types.HlsMode
import Network.AWS.MediaLive.Types.HlsOutputSelection
import Network.AWS.MediaLive.Types.HlsProgramDateTime
import Network.AWS.MediaLive.Types.HlsRedundantManifest
import Network.AWS.MediaLive.Types.HlsSegmentationMode
import Network.AWS.MediaLive.Types.HlsStreamInfResolution
import Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame
import Network.AWS.MediaLive.Types.HlsTsFileMode
import Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType
import Network.AWS.MediaLive.Types.InputLossActionForHlsOut
import Network.AWS.MediaLive.Types.KeyProviderSettings
import Network.AWS.MediaLive.Types.OutputLocationRef

-- | Hls Group Settings
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
    -- applicable, and media manifests) for this output group.
    -- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
    -- output group, but not a master manifest. SEGMENTS_ONLY: Does not
    -- generate any manifests for this output group.
    outputSelection :: Core.Maybe HlsOutputSelection,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
    -- number used in conjunction with the key for encrypting blocks. If set to
    -- \"include\", IV is listed in the manifest, otherwise the IV is not in
    -- the manifest.
    ivInManifest :: Core.Maybe HlsIvInManifest,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Core.Maybe Core.Natural,
    -- | Encrypts the segments with the given encryption scheme. Exclude this
    -- parameter if no encryption is desired.
    encryptionType :: Core.Maybe HlsEncryptionType,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
    -- that segments will end on the next keyframe after this number of
    -- seconds, so actual segment length may be longer.
    segmentLength :: Core.Maybe Core.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Core.Maybe HlsTimedMetadataId3Frame,
    -- | Choose one or more ad marker types to pass SCTE35 signals through to
    -- this group of Apple HLS outputs.
    adMarkers :: Core.Maybe [HlsAdMarkers],
    -- | The value specifies how the key is represented in the resource
    -- identified by the URI. If parameter is absent, an implicit value of
    -- \"identity\" is used. A reverse DNS string can also be given.
    keyFormat :: Core.Maybe Core.Text,
    -- | Place segments in subdirectories.
    directoryStructure :: Core.Maybe HlsDirectoryStructure,
    -- | For use with encryptionType. This is a 128-bit, 16-byte hex value
    -- represented by a 32-character text string. If ivSource is set to
    -- \"explicit\" then this parameter is required and is used as the IV for
    -- encryption.
    constantIv :: Core.Maybe Core.Text,
    -- | When set to gzip, compresses HLS playlist.
    manifestCompression :: Core.Maybe HlsManifestCompression,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Core.Maybe HlsStreamInfResolution,
    -- | If \"vod\", all segments are indexed and kept permanently in the
    -- destination and manifest. If \"live\", only the number segments
    -- specified in keepSegments and indexNSegments are kept; newer segments
    -- replace older segments, which may prevent players from rewinding all the
    -- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
    -- of EVENT while the channel is running, converting it to a \"VOD\" type
    -- manifest on completion of the stream.
    mode :: Core.Maybe HlsMode,
    -- | Parameters that control interactions with the CDN.
    hlsCdnSettings :: Core.Maybe HlsCdnSettings,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Core.Maybe HlsCodecSpecification,
    -- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes
    -- information about both pipelines: first its own media files, then the
    -- media files of the other pipeline. This feature allows playout device
    -- that support stale manifest detection to switch from one manifest to the
    -- other, when the current manifest seems to be stale. There are still two
    -- destinations and two master manifests, but both master manifests
    -- reference the media files from both pipelines. DISABLED: The master
    -- manifest (.m3u8 file) for each pipeline includes information about its
    -- own pipeline only. For an HLS output group with MediaPackage as the
    -- destination, the DISABLED behavior is always followed. MediaPackage
    -- regenerates the manifests it serves to players so a redundant manifest
    -- from MediaLive is irrelevant.
    redundantManifest :: Core.Maybe HlsRedundantManifest,
    -- | Applies only if Mode field is LIVE. Specifies the maximum number of
    -- segments in the media manifest file. After this maximum, older segments
    -- are removed from the media manifest. This number must be smaller than
    -- the number in the Keep Segments field.
    indexNSegments :: Core.Maybe Core.Natural,
    -- | DISABLED: Do not create an I-frame-only manifest, but do create the
    -- master and media manifests (according to the Output Selection field).
    -- STANDARD: Create an I-frame-only manifest for each output that contains
    -- video, as well as the other manifests (according to the Output Selection
    -- field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to
    -- indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries
    -- identifying the I-frame position. For example,
    -- #EXT-X-BYTERANGE:160364\@1461888\"
    iFrameOnlyPlaylists :: Core.Maybe IFrameOnlyPlaylistType,
    -- | useInputSegmentation has been deprecated. The configured segment size is
    -- always used.
    segmentationMode :: Core.Maybe HlsSegmentationMode,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated as follows: either the program date and
    -- time are initialized using the input timecode source, or the time is
    -- initialized using the input timecode source and the date is initialized
    -- using the timestampOffset.
    programDateTime :: Core.Maybe HlsProgramDateTime,
    -- | Number of segments to write to a subdirectory before starting a new one.
    -- directoryStructure must be subdirectoryPerStream for this setting to
    -- have an effect.
    segmentsPerSubdirectory :: Core.Maybe Core.Natural,
    -- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media
    -- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
    -- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
    -- tags to index segments for playback. A typical use for this value is
    -- when sending the output to AWS Elemental MediaConvert, which can accept
    -- only a single media file. Playback while the channel is running is not
    -- guaranteed due to HTTP server caching.
    tsFileMode :: Core.Maybe HlsTsFileMode,
    -- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
    -- manifests for this output group. Typically, choose Insert because these
    -- tags are required in the manifest (according to the HLS specification)
    -- and serve an important purpose. Choose Never Insert only if the
    -- downstream system is doing real-time failover (without using the
    -- MediaLive automatic failover feature) and only if that downstream system
    -- has advised you to exclude the tags.
    discontinuityTags :: Core.Maybe HlsDiscontinuityTags,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrlContent :: Core.Maybe Core.Text,
    -- | State of HLS ID3 Segment Tagging
    hlsId3SegmentTagging :: Core.Maybe HlsId3SegmentTaggingState,
    -- | Specifies whether to include the final (incomplete) segment in the media
    -- output when the pipeline stops producing output because of a channel
    -- stop, a channel pause or a loss of input to the pipeline. Auto means
    -- that MediaLive decides whether to include the final segment, depending
    -- on the channel class and the types of output groups. Suppress means to
    -- never include the incomplete segment. We recommend you choose Auto and
    -- let MediaLive control the behavior.
    incompleteSegmentBehavior :: Core.Maybe HlsIncompleteSegmentBehavior,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrlManifest :: Core.Maybe Core.Text,
    -- | Optional. One value per output group. This field is required only if you
    -- are completing Base URL content A, and the downstream system has
    -- notified you that the media files for pipeline 1 of all outputs are in a
    -- location different from the media files for pipeline 0.
    baseUrlContent1 :: Core.Maybe Core.Text,
    -- | Mapping of up to 4 caption channels to caption languages. Is only
    -- meaningful if captionLanguageSetting is set to \"insert\".
    captionLanguageMappings :: Core.Maybe [CaptionLanguageMapping],
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Core.Maybe InputLossActionForHlsOut,
    -- | The key provider settings.
    keyProviderSettings :: Core.Maybe KeyProviderSettings,
    -- | Applies only if Mode field is LIVE. Specifies the number of media
    -- segments to retain in the destination directory. This number should be
    -- bigger than indexNSegments (Num segments). We recommend (value = (2 x
    -- indexNsegments) + 1). If this \"keep segments\" number is too low, the
    -- following might happen: the player is still reading a media manifest
    -- file that lists this segment, but that segment has been removed from the
    -- destination directory (as directed by indexNSegments). This situation
    -- would result in a 404 HTTP error on the player.
    keepSegments :: Core.Maybe Core.Natural,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Core.Maybe Core.Natural,
    -- | Optional. One value per output group. Complete this field only if you
    -- are completing Base URL manifest A, and the downstream system has
    -- notified you that the child manifest files for pipeline 1 of all outputs
    -- are in a location different from the child manifest files for pipeline
    -- 0.
    baseUrlManifest1 :: Core.Maybe Core.Text,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Core.Maybe Core.Natural,
    -- | Either a single positive integer version value or a slash delimited list
    -- of version values (1\/2\/3).
    keyFormatVersions :: Core.Maybe Core.Text,
    -- | When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
    -- manifest, which prevents clients from saving media segments for later
    -- replay.
    clientCache :: Core.Maybe HlsClientCache,
    -- | When set, minimumSegmentLength is enforced by looking ahead and back
    -- within the specified range for a nearby avail and extending the segment
    -- size if needed.
    minSegmentLength :: Core.Maybe Core.Natural,
    -- | Indicates whether the output manifest should use floating point or
    -- integer values for segment duration.
    manifestDurationFormat :: Core.Maybe HlsManifestDurationFormat,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
    -- number used in conjunction with the key for encrypting blocks. If this
    -- setting is \"followsSegmentNumber\", it will cause the IV to change
    -- every segment (to match the segment number). If this is set to
    -- \"explicit\", you must enter a constantIv value.
    ivSource :: Core.Maybe HlsIvSource,
    -- | Applies only to 608 Embedded output captions. insert: Include
    -- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
    -- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
    -- Language Code you specify. Make sure to specify the languages in the
    -- order in which they appear in the original source (if the source is
    -- embedded format) or the order of the caption selectors (if the source is
    -- other than embedded). Otherwise, languages in the manifest will not
    -- match up properly with the output captions. none: Include
    -- CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any
    -- CLOSED-CAPTIONS line from the manifest.
    captionLanguageSetting :: Core.Maybe HlsCaptionLanguageSetting,
    -- | A directory or HTTP destination for the HLS segments, manifest files,
    -- and encryption keys (if enabled).
    destination :: OutputLocationRef
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
-- 'outputSelection', 'hlsGroupSettings_outputSelection' - MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
-- applicable, and media manifests) for this output group.
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
-- output group, but not a master manifest. SEGMENTS_ONLY: Does not
-- generate any manifests for this output group.
--
-- 'ivInManifest', 'hlsGroupSettings_ivInManifest' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If set to
-- \"include\", IV is listed in the manifest, otherwise the IV is not in
-- the manifest.
--
-- 'timedMetadataId3Period', 'hlsGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- 'encryptionType', 'hlsGroupSettings_encryptionType' - Encrypts the segments with the given encryption scheme. Exclude this
-- parameter if no encryption is desired.
--
-- 'segmentLength', 'hlsGroupSettings_segmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note
-- that segments will end on the next keyframe after this number of
-- seconds, so actual segment length may be longer.
--
-- 'timedMetadataId3Frame', 'hlsGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'adMarkers', 'hlsGroupSettings_adMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to
-- this group of Apple HLS outputs.
--
-- 'keyFormat', 'hlsGroupSettings_keyFormat' - The value specifies how the key is represented in the resource
-- identified by the URI. If parameter is absent, an implicit value of
-- \"identity\" is used. A reverse DNS string can also be given.
--
-- 'directoryStructure', 'hlsGroupSettings_directoryStructure' - Place segments in subdirectories.
--
-- 'constantIv', 'hlsGroupSettings_constantIv' - For use with encryptionType. This is a 128-bit, 16-byte hex value
-- represented by a 32-character text string. If ivSource is set to
-- \"explicit\" then this parameter is required and is used as the IV for
-- encryption.
--
-- 'manifestCompression', 'hlsGroupSettings_manifestCompression' - When set to gzip, compresses HLS playlist.
--
-- 'streamInfResolution', 'hlsGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
--
-- 'mode', 'hlsGroupSettings_mode' - If \"vod\", all segments are indexed and kept permanently in the
-- destination and manifest. If \"live\", only the number segments
-- specified in keepSegments and indexNSegments are kept; newer segments
-- replace older segments, which may prevent players from rewinding all the
-- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
-- of EVENT while the channel is running, converting it to a \"VOD\" type
-- manifest on completion of the stream.
--
-- 'hlsCdnSettings', 'hlsGroupSettings_hlsCdnSettings' - Parameters that control interactions with the CDN.
--
-- 'codecSpecification', 'hlsGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'redundantManifest', 'hlsGroupSettings_redundantManifest' - ENABLED: The master manifest (.m3u8 file) for each pipeline includes
-- information about both pipelines: first its own media files, then the
-- media files of the other pipeline. This feature allows playout device
-- that support stale manifest detection to switch from one manifest to the
-- other, when the current manifest seems to be stale. There are still two
-- destinations and two master manifests, but both master manifests
-- reference the media files from both pipelines. DISABLED: The master
-- manifest (.m3u8 file) for each pipeline includes information about its
-- own pipeline only. For an HLS output group with MediaPackage as the
-- destination, the DISABLED behavior is always followed. MediaPackage
-- regenerates the manifests it serves to players so a redundant manifest
-- from MediaLive is irrelevant.
--
-- 'indexNSegments', 'hlsGroupSettings_indexNSegments' - Applies only if Mode field is LIVE. Specifies the maximum number of
-- segments in the media manifest file. After this maximum, older segments
-- are removed from the media manifest. This number must be smaller than
-- the number in the Keep Segments field.
--
-- 'iFrameOnlyPlaylists', 'hlsGroupSettings_iFrameOnlyPlaylists' - DISABLED: Do not create an I-frame-only manifest, but do create the
-- master and media manifests (according to the Output Selection field).
-- STANDARD: Create an I-frame-only manifest for each output that contains
-- video, as well as the other manifests (according to the Output Selection
-- field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to
-- indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries
-- identifying the I-frame position. For example,
-- #EXT-X-BYTERANGE:160364\@1461888\"
--
-- 'segmentationMode', 'hlsGroupSettings_segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is
-- always used.
--
-- 'programDateTime', 'hlsGroupSettings_programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestampOffset.
--
-- 'segmentsPerSubdirectory', 'hlsGroupSettings_segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be subdirectoryPerStream for this setting to
-- have an effect.
--
-- 'tsFileMode', 'hlsGroupSettings_tsFileMode' - SEGMENTED_FILES: Emit the program as segments - multiple .ts media
-- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
-- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
-- tags to index segments for playback. A typical use for this value is
-- when sending the output to AWS Elemental MediaConvert, which can accept
-- only a single media file. Playback while the channel is running is not
-- guaranteed due to HTTP server caching.
--
-- 'discontinuityTags', 'hlsGroupSettings_discontinuityTags' - Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
-- manifests for this output group. Typically, choose Insert because these
-- tags are required in the manifest (according to the HLS specification)
-- and serve an important purpose. Choose Never Insert only if the
-- downstream system is doing real-time failover (without using the
-- MediaLive automatic failover feature) and only if that downstream system
-- has advised you to exclude the tags.
--
-- 'baseUrlContent', 'hlsGroupSettings_baseUrlContent' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'hlsId3SegmentTagging', 'hlsGroupSettings_hlsId3SegmentTagging' - State of HLS ID3 Segment Tagging
--
-- 'incompleteSegmentBehavior', 'hlsGroupSettings_incompleteSegmentBehavior' - Specifies whether to include the final (incomplete) segment in the media
-- output when the pipeline stops producing output because of a channel
-- stop, a channel pause or a loss of input to the pipeline. Auto means
-- that MediaLive decides whether to include the final segment, depending
-- on the channel class and the types of output groups. Suppress means to
-- never include the incomplete segment. We recommend you choose Auto and
-- let MediaLive control the behavior.
--
-- 'baseUrlManifest', 'hlsGroupSettings_baseUrlManifest' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'baseUrlContent1', 'hlsGroupSettings_baseUrlContent1' - Optional. One value per output group. This field is required only if you
-- are completing Base URL content A, and the downstream system has
-- notified you that the media files for pipeline 1 of all outputs are in a
-- location different from the media files for pipeline 0.
--
-- 'captionLanguageMappings', 'hlsGroupSettings_captionLanguageMappings' - Mapping of up to 4 caption channels to caption languages. Is only
-- meaningful if captionLanguageSetting is set to \"insert\".
--
-- 'inputLossAction', 'hlsGroupSettings_inputLossAction' - Parameter that control output group behavior on input loss.
--
-- 'keyProviderSettings', 'hlsGroupSettings_keyProviderSettings' - The key provider settings.
--
-- 'keepSegments', 'hlsGroupSettings_keepSegments' - Applies only if Mode field is LIVE. Specifies the number of media
-- segments to retain in the destination directory. This number should be
-- bigger than indexNSegments (Num segments). We recommend (value = (2 x
-- indexNsegments) + 1). If this \"keep segments\" number is too low, the
-- following might happen: the player is still reading a media manifest
-- file that lists this segment, but that segment has been removed from the
-- destination directory (as directed by indexNSegments). This situation
-- would result in a 404 HTTP error on the player.
--
-- 'timestampDeltaMilliseconds', 'hlsGroupSettings_timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- 'baseUrlManifest1', 'hlsGroupSettings_baseUrlManifest1' - Optional. One value per output group. Complete this field only if you
-- are completing Base URL manifest A, and the downstream system has
-- notified you that the child manifest files for pipeline 1 of all outputs
-- are in a location different from the child manifest files for pipeline
-- 0.
--
-- 'programDateTimePeriod', 'hlsGroupSettings_programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- 'keyFormatVersions', 'hlsGroupSettings_keyFormatVersions' - Either a single positive integer version value or a slash delimited list
-- of version values (1\/2\/3).
--
-- 'clientCache', 'hlsGroupSettings_clientCache' - When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
-- manifest, which prevents clients from saving media segments for later
-- replay.
--
-- 'minSegmentLength', 'hlsGroupSettings_minSegmentLength' - When set, minimumSegmentLength is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
--
-- 'manifestDurationFormat', 'hlsGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point or
-- integer values for segment duration.
--
-- 'ivSource', 'hlsGroupSettings_ivSource' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If this
-- setting is \"followsSegmentNumber\", it will cause the IV to change
-- every segment (to match the segment number). If this is set to
-- \"explicit\", you must enter a constantIv value.
--
-- 'captionLanguageSetting', 'hlsGroupSettings_captionLanguageSetting' - Applies only to 608 Embedded output captions. insert: Include
-- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
-- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
-- Language Code you specify. Make sure to specify the languages in the
-- order in which they appear in the original source (if the source is
-- embedded format) or the order of the caption selectors (if the source is
-- other than embedded). Otherwise, languages in the manifest will not
-- match up properly with the output captions. none: Include
-- CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any
-- CLOSED-CAPTIONS line from the manifest.
--
-- 'destination', 'hlsGroupSettings_destination' - A directory or HTTP destination for the HLS segments, manifest files,
-- and encryption keys (if enabled).
newHlsGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  HlsGroupSettings
newHlsGroupSettings pDestination_ =
  HlsGroupSettings'
    { outputSelection = Core.Nothing,
      ivInManifest = Core.Nothing,
      timedMetadataId3Period = Core.Nothing,
      encryptionType = Core.Nothing,
      segmentLength = Core.Nothing,
      timedMetadataId3Frame = Core.Nothing,
      adMarkers = Core.Nothing,
      keyFormat = Core.Nothing,
      directoryStructure = Core.Nothing,
      constantIv = Core.Nothing,
      manifestCompression = Core.Nothing,
      streamInfResolution = Core.Nothing,
      mode = Core.Nothing,
      hlsCdnSettings = Core.Nothing,
      codecSpecification = Core.Nothing,
      redundantManifest = Core.Nothing,
      indexNSegments = Core.Nothing,
      iFrameOnlyPlaylists = Core.Nothing,
      segmentationMode = Core.Nothing,
      programDateTime = Core.Nothing,
      segmentsPerSubdirectory = Core.Nothing,
      tsFileMode = Core.Nothing,
      discontinuityTags = Core.Nothing,
      baseUrlContent = Core.Nothing,
      hlsId3SegmentTagging = Core.Nothing,
      incompleteSegmentBehavior = Core.Nothing,
      baseUrlManifest = Core.Nothing,
      baseUrlContent1 = Core.Nothing,
      captionLanguageMappings = Core.Nothing,
      inputLossAction = Core.Nothing,
      keyProviderSettings = Core.Nothing,
      keepSegments = Core.Nothing,
      timestampDeltaMilliseconds = Core.Nothing,
      baseUrlManifest1 = Core.Nothing,
      programDateTimePeriod = Core.Nothing,
      keyFormatVersions = Core.Nothing,
      clientCache = Core.Nothing,
      minSegmentLength = Core.Nothing,
      manifestDurationFormat = Core.Nothing,
      ivSource = Core.Nothing,
      captionLanguageSetting = Core.Nothing,
      destination = pDestination_
    }

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
-- applicable, and media manifests) for this output group.
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
-- output group, but not a master manifest. SEGMENTS_ONLY: Does not
-- generate any manifests for this output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If set to
-- \"include\", IV is listed in the manifest, otherwise the IV is not in
-- the manifest.
hlsGroupSettings_ivInManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsIvInManifest)
hlsGroupSettings_ivInManifest = Lens.lens (\HlsGroupSettings' {ivInManifest} -> ivInManifest) (\s@HlsGroupSettings' {} a -> s {ivInManifest = a} :: HlsGroupSettings)

-- | Timed Metadata interval in seconds.
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | Encrypts the segments with the given encryption scheme. Exclude this
-- parameter if no encryption is desired.
hlsGroupSettings_encryptionType :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsEncryptionType)
hlsGroupSettings_encryptionType = Lens.lens (\HlsGroupSettings' {encryptionType} -> encryptionType) (\s@HlsGroupSettings' {} a -> s {encryptionType = a} :: HlsGroupSettings)

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note
-- that segments will end on the next keyframe after this number of
-- seconds, so actual segment length may be longer.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | Indicates ID3 frame that has the timecode.
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Choose one or more ad marker types to pass SCTE35 signals through to
-- this group of Apple HLS outputs.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Core.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | The value specifies how the key is represented in the resource
-- identified by the URI. If parameter is absent, an implicit value of
-- \"identity\" is used. A reverse DNS string can also be given.
hlsGroupSettings_keyFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_keyFormat = Lens.lens (\HlsGroupSettings' {keyFormat} -> keyFormat) (\s@HlsGroupSettings' {} a -> s {keyFormat = a} :: HlsGroupSettings)

-- | Place segments in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value
-- represented by a 32-character text string. If ivSource is set to
-- \"explicit\" then this parameter is required and is used as the IV for
-- encryption.
hlsGroupSettings_constantIv :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_constantIv = Lens.lens (\HlsGroupSettings' {constantIv} -> constantIv) (\s@HlsGroupSettings' {} a -> s {constantIv = a} :: HlsGroupSettings)

-- | When set to gzip, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

-- | If \"vod\", all segments are indexed and kept permanently in the
-- destination and manifest. If \"live\", only the number segments
-- specified in keepSegments and indexNSegments are kept; newer segments
-- replace older segments, which may prevent players from rewinding all the
-- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
-- of EVENT while the channel is running, converting it to a \"VOD\" type
-- manifest on completion of the stream.
hlsGroupSettings_mode :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsMode)
hlsGroupSettings_mode = Lens.lens (\HlsGroupSettings' {mode} -> mode) (\s@HlsGroupSettings' {} a -> s {mode = a} :: HlsGroupSettings)

-- | Parameters that control interactions with the CDN.
hlsGroupSettings_hlsCdnSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsCdnSettings)
hlsGroupSettings_hlsCdnSettings = Lens.lens (\HlsGroupSettings' {hlsCdnSettings} -> hlsCdnSettings) (\s@HlsGroupSettings' {} a -> s {hlsCdnSettings = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes
-- information about both pipelines: first its own media files, then the
-- media files of the other pipeline. This feature allows playout device
-- that support stale manifest detection to switch from one manifest to the
-- other, when the current manifest seems to be stale. There are still two
-- destinations and two master manifests, but both master manifests
-- reference the media files from both pipelines. DISABLED: The master
-- manifest (.m3u8 file) for each pipeline includes information about its
-- own pipeline only. For an HLS output group with MediaPackage as the
-- destination, the DISABLED behavior is always followed. MediaPackage
-- regenerates the manifests it serves to players so a redundant manifest
-- from MediaLive is irrelevant.
hlsGroupSettings_redundantManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsRedundantManifest)
hlsGroupSettings_redundantManifest = Lens.lens (\HlsGroupSettings' {redundantManifest} -> redundantManifest) (\s@HlsGroupSettings' {} a -> s {redundantManifest = a} :: HlsGroupSettings)

-- | Applies only if Mode field is LIVE. Specifies the maximum number of
-- segments in the media manifest file. After this maximum, older segments
-- are removed from the media manifest. This number must be smaller than
-- the number in the Keep Segments field.
hlsGroupSettings_indexNSegments :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_indexNSegments = Lens.lens (\HlsGroupSettings' {indexNSegments} -> indexNSegments) (\s@HlsGroupSettings' {} a -> s {indexNSegments = a} :: HlsGroupSettings)

-- | DISABLED: Do not create an I-frame-only manifest, but do create the
-- master and media manifests (according to the Output Selection field).
-- STANDARD: Create an I-frame-only manifest for each output that contains
-- video, as well as the other manifests (according to the Output Selection
-- field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to
-- indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries
-- identifying the I-frame position. For example,
-- #EXT-X-BYTERANGE:160364\@1461888\"
hlsGroupSettings_iFrameOnlyPlaylists :: Lens.Lens' HlsGroupSettings (Core.Maybe IFrameOnlyPlaylistType)
hlsGroupSettings_iFrameOnlyPlaylists = Lens.lens (\HlsGroupSettings' {iFrameOnlyPlaylists} -> iFrameOnlyPlaylists) (\s@HlsGroupSettings' {} a -> s {iFrameOnlyPlaylists = a} :: HlsGroupSettings)

-- | useInputSegmentation has been deprecated. The configured segment size is
-- always used.
hlsGroupSettings_segmentationMode :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsSegmentationMode)
hlsGroupSettings_segmentationMode = Lens.lens (\HlsGroupSettings' {segmentationMode} -> segmentationMode) (\s@HlsGroupSettings' {} a -> s {segmentationMode = a} :: HlsGroupSettings)

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated as follows: either the program date and
-- time are initialized using the input timecode source, or the time is
-- initialized using the input timecode source and the date is initialized
-- using the timestampOffset.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

-- | Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be subdirectoryPerStream for this setting to
-- have an effect.
hlsGroupSettings_segmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_segmentsPerSubdirectory = Lens.lens (\HlsGroupSettings' {segmentsPerSubdirectory} -> segmentsPerSubdirectory) (\s@HlsGroupSettings' {} a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media
-- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
-- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
-- tags to index segments for playback. A typical use for this value is
-- when sending the output to AWS Elemental MediaConvert, which can accept
-- only a single media file. Playback while the channel is running is not
-- guaranteed due to HTTP server caching.
hlsGroupSettings_tsFileMode :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsTsFileMode)
hlsGroupSettings_tsFileMode = Lens.lens (\HlsGroupSettings' {tsFileMode} -> tsFileMode) (\s@HlsGroupSettings' {} a -> s {tsFileMode = a} :: HlsGroupSettings)

-- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
-- manifests for this output group. Typically, choose Insert because these
-- tags are required in the manifest (according to the HLS specification)
-- and serve an important purpose. Choose Never Insert only if the
-- downstream system is doing real-time failover (without using the
-- MediaLive automatic failover feature) and only if that downstream system
-- has advised you to exclude the tags.
hlsGroupSettings_discontinuityTags :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsDiscontinuityTags)
hlsGroupSettings_discontinuityTags = Lens.lens (\HlsGroupSettings' {discontinuityTags} -> discontinuityTags) (\s@HlsGroupSettings' {} a -> s {discontinuityTags = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrlContent :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_baseUrlContent = Lens.lens (\HlsGroupSettings' {baseUrlContent} -> baseUrlContent) (\s@HlsGroupSettings' {} a -> s {baseUrlContent = a} :: HlsGroupSettings)

-- | State of HLS ID3 Segment Tagging
hlsGroupSettings_hlsId3SegmentTagging :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsId3SegmentTaggingState)
hlsGroupSettings_hlsId3SegmentTagging = Lens.lens (\HlsGroupSettings' {hlsId3SegmentTagging} -> hlsId3SegmentTagging) (\s@HlsGroupSettings' {} a -> s {hlsId3SegmentTagging = a} :: HlsGroupSettings)

-- | Specifies whether to include the final (incomplete) segment in the media
-- output when the pipeline stops producing output because of a channel
-- stop, a channel pause or a loss of input to the pipeline. Auto means
-- that MediaLive decides whether to include the final segment, depending
-- on the channel class and the types of output groups. Suppress means to
-- never include the incomplete segment. We recommend you choose Auto and
-- let MediaLive control the behavior.
hlsGroupSettings_incompleteSegmentBehavior :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsIncompleteSegmentBehavior)
hlsGroupSettings_incompleteSegmentBehavior = Lens.lens (\HlsGroupSettings' {incompleteSegmentBehavior} -> incompleteSegmentBehavior) (\s@HlsGroupSettings' {} a -> s {incompleteSegmentBehavior = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrlManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_baseUrlManifest = Lens.lens (\HlsGroupSettings' {baseUrlManifest} -> baseUrlManifest) (\s@HlsGroupSettings' {} a -> s {baseUrlManifest = a} :: HlsGroupSettings)

-- | Optional. One value per output group. This field is required only if you
-- are completing Base URL content A, and the downstream system has
-- notified you that the media files for pipeline 1 of all outputs are in a
-- location different from the media files for pipeline 0.
hlsGroupSettings_baseUrlContent1 :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_baseUrlContent1 = Lens.lens (\HlsGroupSettings' {baseUrlContent1} -> baseUrlContent1) (\s@HlsGroupSettings' {} a -> s {baseUrlContent1 = a} :: HlsGroupSettings)

-- | Mapping of up to 4 caption channels to caption languages. Is only
-- meaningful if captionLanguageSetting is set to \"insert\".
hlsGroupSettings_captionLanguageMappings :: Lens.Lens' HlsGroupSettings (Core.Maybe [CaptionLanguageMapping])
hlsGroupSettings_captionLanguageMappings = Lens.lens (\HlsGroupSettings' {captionLanguageMappings} -> captionLanguageMappings) (\s@HlsGroupSettings' {} a -> s {captionLanguageMappings = a} :: HlsGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | Parameter that control output group behavior on input loss.
hlsGroupSettings_inputLossAction :: Lens.Lens' HlsGroupSettings (Core.Maybe InputLossActionForHlsOut)
hlsGroupSettings_inputLossAction = Lens.lens (\HlsGroupSettings' {inputLossAction} -> inputLossAction) (\s@HlsGroupSettings' {} a -> s {inputLossAction = a} :: HlsGroupSettings)

-- | The key provider settings.
hlsGroupSettings_keyProviderSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe KeyProviderSettings)
hlsGroupSettings_keyProviderSettings = Lens.lens (\HlsGroupSettings' {keyProviderSettings} -> keyProviderSettings) (\s@HlsGroupSettings' {} a -> s {keyProviderSettings = a} :: HlsGroupSettings)

-- | Applies only if Mode field is LIVE. Specifies the number of media
-- segments to retain in the destination directory. This number should be
-- bigger than indexNSegments (Num segments). We recommend (value = (2 x
-- indexNsegments) + 1). If this \"keep segments\" number is too low, the
-- following might happen: the player is still reading a media manifest
-- file that lists this segment, but that segment has been removed from the
-- destination directory (as directed by indexNSegments). This situation
-- would result in a 404 HTTP error on the player.
hlsGroupSettings_keepSegments :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_keepSegments = Lens.lens (\HlsGroupSettings' {keepSegments} -> keepSegments) (\s@HlsGroupSettings' {} a -> s {keepSegments = a} :: HlsGroupSettings)

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hlsGroupSettings_timestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_timestampDeltaMilliseconds = Lens.lens (\HlsGroupSettings' {timestampDeltaMilliseconds} -> timestampDeltaMilliseconds) (\s@HlsGroupSettings' {} a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)

-- | Optional. One value per output group. Complete this field only if you
-- are completing Base URL manifest A, and the downstream system has
-- notified you that the child manifest files for pipeline 1 of all outputs
-- are in a location different from the child manifest files for pipeline
-- 0.
hlsGroupSettings_baseUrlManifest1 :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_baseUrlManifest1 = Lens.lens (\HlsGroupSettings' {baseUrlManifest1} -> baseUrlManifest1) (\s@HlsGroupSettings' {} a -> s {baseUrlManifest1 = a} :: HlsGroupSettings)

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hlsGroupSettings_programDateTimePeriod :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_programDateTimePeriod = Lens.lens (\HlsGroupSettings' {programDateTimePeriod} -> programDateTimePeriod) (\s@HlsGroupSettings' {} a -> s {programDateTimePeriod = a} :: HlsGroupSettings)

-- | Either a single positive integer version value or a slash delimited list
-- of version values (1\/2\/3).
hlsGroupSettings_keyFormatVersions :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hlsGroupSettings_keyFormatVersions = Lens.lens (\HlsGroupSettings' {keyFormatVersions} -> keyFormatVersions) (\s@HlsGroupSettings' {} a -> s {keyFormatVersions = a} :: HlsGroupSettings)

-- | When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
-- manifest, which prevents clients from saving media segments for later
-- replay.
hlsGroupSettings_clientCache :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsClientCache)
hlsGroupSettings_clientCache = Lens.lens (\HlsGroupSettings' {clientCache} -> clientCache) (\s@HlsGroupSettings' {} a -> s {clientCache = a} :: HlsGroupSettings)

-- | When set, minimumSegmentLength is enforced by looking ahead and back
-- within the specified range for a nearby avail and extending the segment
-- size if needed.
hlsGroupSettings_minSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hlsGroupSettings_minSegmentLength = Lens.lens (\HlsGroupSettings' {minSegmentLength} -> minSegmentLength) (\s@HlsGroupSettings' {} a -> s {minSegmentLength = a} :: HlsGroupSettings)

-- | Indicates whether the output manifest should use floating point or
-- integer values for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsManifestDurationFormat)
hlsGroupSettings_manifestDurationFormat = Lens.lens (\HlsGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@HlsGroupSettings' {} a -> s {manifestDurationFormat = a} :: HlsGroupSettings)

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If this
-- setting is \"followsSegmentNumber\", it will cause the IV to change
-- every segment (to match the segment number). If this is set to
-- \"explicit\", you must enter a constantIv value.
hlsGroupSettings_ivSource :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsIvSource)
hlsGroupSettings_ivSource = Lens.lens (\HlsGroupSettings' {ivSource} -> ivSource) (\s@HlsGroupSettings' {} a -> s {ivSource = a} :: HlsGroupSettings)

-- | Applies only to 608 Embedded output captions. insert: Include
-- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
-- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
-- Language Code you specify. Make sure to specify the languages in the
-- order in which they appear in the original source (if the source is
-- embedded format) or the order of the caption selectors (if the source is
-- other than embedded). Otherwise, languages in the manifest will not
-- match up properly with the output captions. none: Include
-- CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any
-- CLOSED-CAPTIONS line from the manifest.
hlsGroupSettings_captionLanguageSetting :: Lens.Lens' HlsGroupSettings (Core.Maybe HlsCaptionLanguageSetting)
hlsGroupSettings_captionLanguageSetting = Lens.lens (\HlsGroupSettings' {captionLanguageSetting} -> captionLanguageSetting) (\s@HlsGroupSettings' {} a -> s {captionLanguageSetting = a} :: HlsGroupSettings)

-- | A directory or HTTP destination for the HLS segments, manifest files,
-- and encryption keys (if enabled).
hlsGroupSettings_destination :: Lens.Lens' HlsGroupSettings OutputLocationRef
hlsGroupSettings_destination = Lens.lens (\HlsGroupSettings' {destination} -> destination) (\s@HlsGroupSettings' {} a -> s {destination = a} :: HlsGroupSettings)

instance Core.FromJSON HlsGroupSettings where
  parseJSON =
    Core.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Core.<$> (x Core..:? "outputSelection")
            Core.<*> (x Core..:? "ivInManifest")
            Core.<*> (x Core..:? "timedMetadataId3Period")
            Core.<*> (x Core..:? "encryptionType")
            Core.<*> (x Core..:? "segmentLength")
            Core.<*> (x Core..:? "timedMetadataId3Frame")
            Core.<*> (x Core..:? "adMarkers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "keyFormat")
            Core.<*> (x Core..:? "directoryStructure")
            Core.<*> (x Core..:? "constantIv")
            Core.<*> (x Core..:? "manifestCompression")
            Core.<*> (x Core..:? "streamInfResolution")
            Core.<*> (x Core..:? "mode")
            Core.<*> (x Core..:? "hlsCdnSettings")
            Core.<*> (x Core..:? "codecSpecification")
            Core.<*> (x Core..:? "redundantManifest")
            Core.<*> (x Core..:? "indexNSegments")
            Core.<*> (x Core..:? "iFrameOnlyPlaylists")
            Core.<*> (x Core..:? "segmentationMode")
            Core.<*> (x Core..:? "programDateTime")
            Core.<*> (x Core..:? "segmentsPerSubdirectory")
            Core.<*> (x Core..:? "tsFileMode")
            Core.<*> (x Core..:? "discontinuityTags")
            Core.<*> (x Core..:? "baseUrlContent")
            Core.<*> (x Core..:? "hlsId3SegmentTagging")
            Core.<*> (x Core..:? "incompleteSegmentBehavior")
            Core.<*> (x Core..:? "baseUrlManifest")
            Core.<*> (x Core..:? "baseUrlContent1")
            Core.<*> ( x Core..:? "captionLanguageMappings"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "inputLossAction")
            Core.<*> (x Core..:? "keyProviderSettings")
            Core.<*> (x Core..:? "keepSegments")
            Core.<*> (x Core..:? "timestampDeltaMilliseconds")
            Core.<*> (x Core..:? "baseUrlManifest1")
            Core.<*> (x Core..:? "programDateTimePeriod")
            Core.<*> (x Core..:? "keyFormatVersions")
            Core.<*> (x Core..:? "clientCache")
            Core.<*> (x Core..:? "minSegmentLength")
            Core.<*> (x Core..:? "manifestDurationFormat")
            Core.<*> (x Core..:? "ivSource")
            Core.<*> (x Core..:? "captionLanguageSetting")
            Core.<*> (x Core..: "destination")
      )

instance Core.Hashable HlsGroupSettings

instance Core.NFData HlsGroupSettings

instance Core.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputSelection" Core..=)
              Core.<$> outputSelection,
            ("ivInManifest" Core..=) Core.<$> ivInManifest,
            ("timedMetadataId3Period" Core..=)
              Core.<$> timedMetadataId3Period,
            ("encryptionType" Core..=) Core.<$> encryptionType,
            ("segmentLength" Core..=) Core.<$> segmentLength,
            ("timedMetadataId3Frame" Core..=)
              Core.<$> timedMetadataId3Frame,
            ("adMarkers" Core..=) Core.<$> adMarkers,
            ("keyFormat" Core..=) Core.<$> keyFormat,
            ("directoryStructure" Core..=)
              Core.<$> directoryStructure,
            ("constantIv" Core..=) Core.<$> constantIv,
            ("manifestCompression" Core..=)
              Core.<$> manifestCompression,
            ("streamInfResolution" Core..=)
              Core.<$> streamInfResolution,
            ("mode" Core..=) Core.<$> mode,
            ("hlsCdnSettings" Core..=) Core.<$> hlsCdnSettings,
            ("codecSpecification" Core..=)
              Core.<$> codecSpecification,
            ("redundantManifest" Core..=)
              Core.<$> redundantManifest,
            ("indexNSegments" Core..=) Core.<$> indexNSegments,
            ("iFrameOnlyPlaylists" Core..=)
              Core.<$> iFrameOnlyPlaylists,
            ("segmentationMode" Core..=)
              Core.<$> segmentationMode,
            ("programDateTime" Core..=) Core.<$> programDateTime,
            ("segmentsPerSubdirectory" Core..=)
              Core.<$> segmentsPerSubdirectory,
            ("tsFileMode" Core..=) Core.<$> tsFileMode,
            ("discontinuityTags" Core..=)
              Core.<$> discontinuityTags,
            ("baseUrlContent" Core..=) Core.<$> baseUrlContent,
            ("hlsId3SegmentTagging" Core..=)
              Core.<$> hlsId3SegmentTagging,
            ("incompleteSegmentBehavior" Core..=)
              Core.<$> incompleteSegmentBehavior,
            ("baseUrlManifest" Core..=) Core.<$> baseUrlManifest,
            ("baseUrlContent1" Core..=) Core.<$> baseUrlContent1,
            ("captionLanguageMappings" Core..=)
              Core.<$> captionLanguageMappings,
            ("inputLossAction" Core..=) Core.<$> inputLossAction,
            ("keyProviderSettings" Core..=)
              Core.<$> keyProviderSettings,
            ("keepSegments" Core..=) Core.<$> keepSegments,
            ("timestampDeltaMilliseconds" Core..=)
              Core.<$> timestampDeltaMilliseconds,
            ("baseUrlManifest1" Core..=)
              Core.<$> baseUrlManifest1,
            ("programDateTimePeriod" Core..=)
              Core.<$> programDateTimePeriod,
            ("keyFormatVersions" Core..=)
              Core.<$> keyFormatVersions,
            ("clientCache" Core..=) Core.<$> clientCache,
            ("minSegmentLength" Core..=)
              Core.<$> minSegmentLength,
            ("manifestDurationFormat" Core..=)
              Core.<$> manifestDurationFormat,
            ("ivSource" Core..=) Core.<$> ivSource,
            ("captionLanguageSetting" Core..=)
              Core.<$> captionLanguageSetting,
            Core.Just ("destination" Core..= destination)
          ]
      )

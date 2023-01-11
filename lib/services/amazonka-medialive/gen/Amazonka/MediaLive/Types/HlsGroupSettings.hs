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
-- Module      : Amazonka.MediaLive.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.CaptionLanguageMapping
import Amazonka.MediaLive.Types.HlsAdMarkers
import Amazonka.MediaLive.Types.HlsCaptionLanguageSetting
import Amazonka.MediaLive.Types.HlsCdnSettings
import Amazonka.MediaLive.Types.HlsClientCache
import Amazonka.MediaLive.Types.HlsCodecSpecification
import Amazonka.MediaLive.Types.HlsDirectoryStructure
import Amazonka.MediaLive.Types.HlsDiscontinuityTags
import Amazonka.MediaLive.Types.HlsEncryptionType
import Amazonka.MediaLive.Types.HlsId3SegmentTaggingState
import Amazonka.MediaLive.Types.HlsIncompleteSegmentBehavior
import Amazonka.MediaLive.Types.HlsIvInManifest
import Amazonka.MediaLive.Types.HlsIvSource
import Amazonka.MediaLive.Types.HlsManifestCompression
import Amazonka.MediaLive.Types.HlsManifestDurationFormat
import Amazonka.MediaLive.Types.HlsMode
import Amazonka.MediaLive.Types.HlsOutputSelection
import Amazonka.MediaLive.Types.HlsProgramDateTime
import Amazonka.MediaLive.Types.HlsProgramDateTimeClock
import Amazonka.MediaLive.Types.HlsRedundantManifest
import Amazonka.MediaLive.Types.HlsSegmentationMode
import Amazonka.MediaLive.Types.HlsStreamInfResolution
import Amazonka.MediaLive.Types.HlsTimedMetadataId3Frame
import Amazonka.MediaLive.Types.HlsTsFileMode
import Amazonka.MediaLive.Types.IFrameOnlyPlaylistType
import Amazonka.MediaLive.Types.InputLossActionForHlsOut
import Amazonka.MediaLive.Types.KeyProviderSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import qualified Amazonka.Prelude as Prelude

-- | Hls Group Settings
--
-- /See:/ 'newHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Choose one or more ad marker types to pass SCTE35 signals through to
    -- this group of Apple HLS outputs.
    adMarkers :: Prelude.Maybe [HlsAdMarkers],
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrlContent :: Prelude.Maybe Prelude.Text,
    -- | Optional. One value per output group. This field is required only if you
    -- are completing Base URL content A, and the downstream system has
    -- notified you that the media files for pipeline 1 of all outputs are in a
    -- location different from the media files for pipeline 0.
    baseUrlContent1 :: Prelude.Maybe Prelude.Text,
    -- | A partial URI prefix that will be prepended to each output in the media
    -- .m3u8 file. Can be used if base manifest is delivered from a different
    -- URL than the main .m3u8 file.
    baseUrlManifest :: Prelude.Maybe Prelude.Text,
    -- | Optional. One value per output group. Complete this field only if you
    -- are completing Base URL manifest A, and the downstream system has
    -- notified you that the child manifest files for pipeline 1 of all outputs
    -- are in a location different from the child manifest files for pipeline
    -- 0.
    baseUrlManifest1 :: Prelude.Maybe Prelude.Text,
    -- | Mapping of up to 4 caption channels to caption languages. Is only
    -- meaningful if captionLanguageSetting is set to \"insert\".
    captionLanguageMappings :: Prelude.Maybe [CaptionLanguageMapping],
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
    captionLanguageSetting :: Prelude.Maybe HlsCaptionLanguageSetting,
    -- | When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
    -- manifest, which prevents clients from saving media segments for later
    -- replay.
    clientCache :: Prelude.Maybe HlsClientCache,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Prelude.Maybe HlsCodecSpecification,
    -- | For use with encryptionType. This is a 128-bit, 16-byte hex value
    -- represented by a 32-character text string. If ivSource is set to
    -- \"explicit\" then this parameter is required and is used as the IV for
    -- encryption.
    constantIv :: Prelude.Maybe Prelude.Text,
    -- | Place segments in subdirectories.
    directoryStructure :: Prelude.Maybe HlsDirectoryStructure,
    -- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
    -- manifests for this output group. Typically, choose Insert because these
    -- tags are required in the manifest (according to the HLS specification)
    -- and serve an important purpose. Choose Never Insert only if the
    -- downstream system is doing real-time failover (without using the
    -- MediaLive automatic failover feature) and only if that downstream system
    -- has advised you to exclude the tags.
    discontinuityTags :: Prelude.Maybe HlsDiscontinuityTags,
    -- | Encrypts the segments with the given encryption scheme. Exclude this
    -- parameter if no encryption is desired.
    encryptionType :: Prelude.Maybe HlsEncryptionType,
    -- | Parameters that control interactions with the CDN.
    hlsCdnSettings :: Prelude.Maybe HlsCdnSettings,
    -- | State of HLS ID3 Segment Tagging
    hlsId3SegmentTagging :: Prelude.Maybe HlsId3SegmentTaggingState,
    -- | DISABLED: Do not create an I-frame-only manifest, but do create the
    -- master and media manifests (according to the Output Selection field).
    -- STANDARD: Create an I-frame-only manifest for each output that contains
    -- video, as well as the other manifests (according to the Output Selection
    -- field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to
    -- indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries
    -- identifying the I-frame position. For example,
    -- #EXT-X-BYTERANGE:160364\@1461888\"
    iFrameOnlyPlaylists :: Prelude.Maybe IFrameOnlyPlaylistType,
    -- | Specifies whether to include the final (incomplete) segment in the media
    -- output when the pipeline stops producing output because of a channel
    -- stop, a channel pause or a loss of input to the pipeline. Auto means
    -- that MediaLive decides whether to include the final segment, depending
    -- on the channel class and the types of output groups. Suppress means to
    -- never include the incomplete segment. We recommend you choose Auto and
    -- let MediaLive control the behavior.
    incompleteSegmentBehavior :: Prelude.Maybe HlsIncompleteSegmentBehavior,
    -- | Applies only if Mode field is LIVE. Specifies the maximum number of
    -- segments in the media manifest file. After this maximum, older segments
    -- are removed from the media manifest. This number must be smaller than
    -- the number in the Keep Segments field.
    indexNSegments :: Prelude.Maybe Prelude.Natural,
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Prelude.Maybe InputLossActionForHlsOut,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
    -- number used in conjunction with the key for encrypting blocks. If set to
    -- \"include\", IV is listed in the manifest, otherwise the IV is not in
    -- the manifest.
    ivInManifest :: Prelude.Maybe HlsIvInManifest,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
    -- number used in conjunction with the key for encrypting blocks. If this
    -- setting is \"followsSegmentNumber\", it will cause the IV to change
    -- every segment (to match the segment number). If this is set to
    -- \"explicit\", you must enter a constantIv value.
    ivSource :: Prelude.Maybe HlsIvSource,
    -- | Applies only if Mode field is LIVE. Specifies the number of media
    -- segments to retain in the destination directory. This number should be
    -- bigger than indexNSegments (Num segments). We recommend (value = (2 x
    -- indexNsegments) + 1). If this \"keep segments\" number is too low, the
    -- following might happen: the player is still reading a media manifest
    -- file that lists this segment, but that segment has been removed from the
    -- destination directory (as directed by indexNSegments). This situation
    -- would result in a 404 HTTP error on the player.
    keepSegments :: Prelude.Maybe Prelude.Natural,
    -- | The value specifies how the key is represented in the resource
    -- identified by the URI. If parameter is absent, an implicit value of
    -- \"identity\" is used. A reverse DNS string can also be given.
    keyFormat :: Prelude.Maybe Prelude.Text,
    -- | Either a single positive integer version value or a slash delimited list
    -- of version values (1\/2\/3).
    keyFormatVersions :: Prelude.Maybe Prelude.Text,
    -- | The key provider settings.
    keyProviderSettings :: Prelude.Maybe KeyProviderSettings,
    -- | When set to gzip, compresses HLS playlist.
    manifestCompression :: Prelude.Maybe HlsManifestCompression,
    -- | Indicates whether the output manifest should use floating point or
    -- integer values for segment duration.
    manifestDurationFormat :: Prelude.Maybe HlsManifestDurationFormat,
    -- | Minimum length of MPEG-2 Transport Stream segments in seconds. When set,
    -- minimum segment length is enforced by looking ahead and back within the
    -- specified range for a nearby avail and extending the segment size if
    -- needed.
    minSegmentLength :: Prelude.Maybe Prelude.Natural,
    -- | If \"vod\", all segments are indexed and kept permanently in the
    -- destination and manifest. If \"live\", only the number segments
    -- specified in keepSegments and indexNSegments are kept; newer segments
    -- replace older segments, which may prevent players from rewinding all the
    -- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
    -- of EVENT while the channel is running, converting it to a \"VOD\" type
    -- manifest on completion of the stream.
    mode :: Prelude.Maybe HlsMode,
    -- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
    -- applicable, and media manifests) for this output group.
    -- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
    -- output group, but not a master manifest. SEGMENTS_ONLY: Does not
    -- generate any manifests for this output group.
    outputSelection :: Prelude.Maybe HlsOutputSelection,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
    -- files. The value is calculated using the program date time clock.
    programDateTime :: Prelude.Maybe HlsProgramDateTime,
    -- | Specifies the algorithm used to drive the HLS EXT-X-PROGRAM-DATE-TIME
    -- clock. Options include: INITIALIZE_FROM_OUTPUT_TIMECODE: The PDT clock
    -- is initialized as a function of the first output timecode, then
    -- incremented by the EXTINF duration of each encoded segment.
    -- SYSTEM_CLOCK: The PDT clock is initialized as a function of the UTC wall
    -- clock, then incremented by the EXTINF duration of each encoded segment.
    -- If the PDT clock diverges from the wall clock by more than 500ms, it is
    -- resynchronized to the wall clock.
    programDateTimeClock :: Prelude.Maybe HlsProgramDateTimeClock,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Prelude.Maybe Prelude.Natural,
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
    redundantManifest :: Prelude.Maybe HlsRedundantManifest,
    -- | Length of MPEG-2 Transport Stream segments to create in seconds. Note
    -- that segments will end on the next keyframe after this duration, so
    -- actual segment length may be longer.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | useInputSegmentation has been deprecated. The configured segment size is
    -- always used.
    segmentationMode :: Prelude.Maybe HlsSegmentationMode,
    -- | Number of segments to write to a subdirectory before starting a new one.
    -- directoryStructure must be subdirectoryPerStream for this setting to
    -- have an effect.
    segmentsPerSubdirectory :: Prelude.Maybe Prelude.Natural,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Prelude.Maybe HlsStreamInfResolution,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Prelude.Maybe HlsTimedMetadataId3Frame,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Prelude.Maybe Prelude.Natural,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Prelude.Maybe Prelude.Natural,
    -- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media
    -- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
    -- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
    -- tags to index segments for playback. A typical use for this value is
    -- when sending the output to AWS Elemental MediaConvert, which can accept
    -- only a single media file. Playback while the channel is running is not
    -- guaranteed due to HTTP server caching.
    tsFileMode :: Prelude.Maybe HlsTsFileMode,
    -- | A directory or HTTP destination for the HLS segments, manifest files,
    -- and encryption keys (if enabled).
    destination :: OutputLocationRef
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
-- 'adMarkers', 'hlsGroupSettings_adMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to
-- this group of Apple HLS outputs.
--
-- 'baseUrlContent', 'hlsGroupSettings_baseUrlContent' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'baseUrlContent1', 'hlsGroupSettings_baseUrlContent1' - Optional. One value per output group. This field is required only if you
-- are completing Base URL content A, and the downstream system has
-- notified you that the media files for pipeline 1 of all outputs are in a
-- location different from the media files for pipeline 0.
--
-- 'baseUrlManifest', 'hlsGroupSettings_baseUrlManifest' - A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
--
-- 'baseUrlManifest1', 'hlsGroupSettings_baseUrlManifest1' - Optional. One value per output group. Complete this field only if you
-- are completing Base URL manifest A, and the downstream system has
-- notified you that the child manifest files for pipeline 1 of all outputs
-- are in a location different from the child manifest files for pipeline
-- 0.
--
-- 'captionLanguageMappings', 'hlsGroupSettings_captionLanguageMappings' - Mapping of up to 4 caption channels to caption languages. Is only
-- meaningful if captionLanguageSetting is set to \"insert\".
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
-- 'clientCache', 'hlsGroupSettings_clientCache' - When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
-- manifest, which prevents clients from saving media segments for later
-- replay.
--
-- 'codecSpecification', 'hlsGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'constantIv', 'hlsGroupSettings_constantIv' - For use with encryptionType. This is a 128-bit, 16-byte hex value
-- represented by a 32-character text string. If ivSource is set to
-- \"explicit\" then this parameter is required and is used as the IV for
-- encryption.
--
-- 'directoryStructure', 'hlsGroupSettings_directoryStructure' - Place segments in subdirectories.
--
-- 'discontinuityTags', 'hlsGroupSettings_discontinuityTags' - Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
-- manifests for this output group. Typically, choose Insert because these
-- tags are required in the manifest (according to the HLS specification)
-- and serve an important purpose. Choose Never Insert only if the
-- downstream system is doing real-time failover (without using the
-- MediaLive automatic failover feature) and only if that downstream system
-- has advised you to exclude the tags.
--
-- 'encryptionType', 'hlsGroupSettings_encryptionType' - Encrypts the segments with the given encryption scheme. Exclude this
-- parameter if no encryption is desired.
--
-- 'hlsCdnSettings', 'hlsGroupSettings_hlsCdnSettings' - Parameters that control interactions with the CDN.
--
-- 'hlsId3SegmentTagging', 'hlsGroupSettings_hlsId3SegmentTagging' - State of HLS ID3 Segment Tagging
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
-- 'incompleteSegmentBehavior', 'hlsGroupSettings_incompleteSegmentBehavior' - Specifies whether to include the final (incomplete) segment in the media
-- output when the pipeline stops producing output because of a channel
-- stop, a channel pause or a loss of input to the pipeline. Auto means
-- that MediaLive decides whether to include the final segment, depending
-- on the channel class and the types of output groups. Suppress means to
-- never include the incomplete segment. We recommend you choose Auto and
-- let MediaLive control the behavior.
--
-- 'indexNSegments', 'hlsGroupSettings_indexNSegments' - Applies only if Mode field is LIVE. Specifies the maximum number of
-- segments in the media manifest file. After this maximum, older segments
-- are removed from the media manifest. This number must be smaller than
-- the number in the Keep Segments field.
--
-- 'inputLossAction', 'hlsGroupSettings_inputLossAction' - Parameter that control output group behavior on input loss.
--
-- 'ivInManifest', 'hlsGroupSettings_ivInManifest' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If set to
-- \"include\", IV is listed in the manifest, otherwise the IV is not in
-- the manifest.
--
-- 'ivSource', 'hlsGroupSettings_ivSource' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If this
-- setting is \"followsSegmentNumber\", it will cause the IV to change
-- every segment (to match the segment number). If this is set to
-- \"explicit\", you must enter a constantIv value.
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
-- 'keyFormat', 'hlsGroupSettings_keyFormat' - The value specifies how the key is represented in the resource
-- identified by the URI. If parameter is absent, an implicit value of
-- \"identity\" is used. A reverse DNS string can also be given.
--
-- 'keyFormatVersions', 'hlsGroupSettings_keyFormatVersions' - Either a single positive integer version value or a slash delimited list
-- of version values (1\/2\/3).
--
-- 'keyProviderSettings', 'hlsGroupSettings_keyProviderSettings' - The key provider settings.
--
-- 'manifestCompression', 'hlsGroupSettings_manifestCompression' - When set to gzip, compresses HLS playlist.
--
-- 'manifestDurationFormat', 'hlsGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point or
-- integer values for segment duration.
--
-- 'minSegmentLength', 'hlsGroupSettings_minSegmentLength' - Minimum length of MPEG-2 Transport Stream segments in seconds. When set,
-- minimum segment length is enforced by looking ahead and back within the
-- specified range for a nearby avail and extending the segment size if
-- needed.
--
-- 'mode', 'hlsGroupSettings_mode' - If \"vod\", all segments are indexed and kept permanently in the
-- destination and manifest. If \"live\", only the number segments
-- specified in keepSegments and indexNSegments are kept; newer segments
-- replace older segments, which may prevent players from rewinding all the
-- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
-- of EVENT while the channel is running, converting it to a \"VOD\" type
-- manifest on completion of the stream.
--
-- 'outputSelection', 'hlsGroupSettings_outputSelection' - MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
-- applicable, and media manifests) for this output group.
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
-- output group, but not a master manifest. SEGMENTS_ONLY: Does not
-- generate any manifests for this output group.
--
-- 'programDateTime', 'hlsGroupSettings_programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated using the program date time clock.
--
-- 'programDateTimeClock', 'hlsGroupSettings_programDateTimeClock' - Specifies the algorithm used to drive the HLS EXT-X-PROGRAM-DATE-TIME
-- clock. Options include: INITIALIZE_FROM_OUTPUT_TIMECODE: The PDT clock
-- is initialized as a function of the first output timecode, then
-- incremented by the EXTINF duration of each encoded segment.
-- SYSTEM_CLOCK: The PDT clock is initialized as a function of the UTC wall
-- clock, then incremented by the EXTINF duration of each encoded segment.
-- If the PDT clock diverges from the wall clock by more than 500ms, it is
-- resynchronized to the wall clock.
--
-- 'programDateTimePeriod', 'hlsGroupSettings_programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
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
-- 'segmentLength', 'hlsGroupSettings_segmentLength' - Length of MPEG-2 Transport Stream segments to create in seconds. Note
-- that segments will end on the next keyframe after this duration, so
-- actual segment length may be longer.
--
-- 'segmentationMode', 'hlsGroupSettings_segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is
-- always used.
--
-- 'segmentsPerSubdirectory', 'hlsGroupSettings_segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be subdirectoryPerStream for this setting to
-- have an effect.
--
-- 'streamInfResolution', 'hlsGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
--
-- 'timedMetadataId3Frame', 'hlsGroupSettings_timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- 'timedMetadataId3Period', 'hlsGroupSettings_timedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- 'timestampDeltaMilliseconds', 'hlsGroupSettings_timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- 'tsFileMode', 'hlsGroupSettings_tsFileMode' - SEGMENTED_FILES: Emit the program as segments - multiple .ts media
-- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
-- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
-- tags to index segments for playback. A typical use for this value is
-- when sending the output to AWS Elemental MediaConvert, which can accept
-- only a single media file. Playback while the channel is running is not
-- guaranteed due to HTTP server caching.
--
-- 'destination', 'hlsGroupSettings_destination' - A directory or HTTP destination for the HLS segments, manifest files,
-- and encryption keys (if enabled).
newHlsGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  HlsGroupSettings
newHlsGroupSettings pDestination_ =
  HlsGroupSettings'
    { adMarkers = Prelude.Nothing,
      baseUrlContent = Prelude.Nothing,
      baseUrlContent1 = Prelude.Nothing,
      baseUrlManifest = Prelude.Nothing,
      baseUrlManifest1 = Prelude.Nothing,
      captionLanguageMappings = Prelude.Nothing,
      captionLanguageSetting = Prelude.Nothing,
      clientCache = Prelude.Nothing,
      codecSpecification = Prelude.Nothing,
      constantIv = Prelude.Nothing,
      directoryStructure = Prelude.Nothing,
      discontinuityTags = Prelude.Nothing,
      encryptionType = Prelude.Nothing,
      hlsCdnSettings = Prelude.Nothing,
      hlsId3SegmentTagging = Prelude.Nothing,
      iFrameOnlyPlaylists = Prelude.Nothing,
      incompleteSegmentBehavior = Prelude.Nothing,
      indexNSegments = Prelude.Nothing,
      inputLossAction = Prelude.Nothing,
      ivInManifest = Prelude.Nothing,
      ivSource = Prelude.Nothing,
      keepSegments = Prelude.Nothing,
      keyFormat = Prelude.Nothing,
      keyFormatVersions = Prelude.Nothing,
      keyProviderSettings = Prelude.Nothing,
      manifestCompression = Prelude.Nothing,
      manifestDurationFormat = Prelude.Nothing,
      minSegmentLength = Prelude.Nothing,
      mode = Prelude.Nothing,
      outputSelection = Prelude.Nothing,
      programDateTime = Prelude.Nothing,
      programDateTimeClock = Prelude.Nothing,
      programDateTimePeriod = Prelude.Nothing,
      redundantManifest = Prelude.Nothing,
      segmentLength = Prelude.Nothing,
      segmentationMode = Prelude.Nothing,
      segmentsPerSubdirectory = Prelude.Nothing,
      streamInfResolution = Prelude.Nothing,
      timedMetadataId3Frame = Prelude.Nothing,
      timedMetadataId3Period = Prelude.Nothing,
      timestampDeltaMilliseconds = Prelude.Nothing,
      tsFileMode = Prelude.Nothing,
      destination = pDestination_
    }

-- | Choose one or more ad marker types to pass SCTE35 signals through to
-- this group of Apple HLS outputs.
hlsGroupSettings_adMarkers :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [HlsAdMarkers])
hlsGroupSettings_adMarkers = Lens.lens (\HlsGroupSettings' {adMarkers} -> adMarkers) (\s@HlsGroupSettings' {} a -> s {adMarkers = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens.coerced

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrlContent :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrlContent = Lens.lens (\HlsGroupSettings' {baseUrlContent} -> baseUrlContent) (\s@HlsGroupSettings' {} a -> s {baseUrlContent = a} :: HlsGroupSettings)

-- | Optional. One value per output group. This field is required only if you
-- are completing Base URL content A, and the downstream system has
-- notified you that the media files for pipeline 1 of all outputs are in a
-- location different from the media files for pipeline 0.
hlsGroupSettings_baseUrlContent1 :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrlContent1 = Lens.lens (\HlsGroupSettings' {baseUrlContent1} -> baseUrlContent1) (\s@HlsGroupSettings' {} a -> s {baseUrlContent1 = a} :: HlsGroupSettings)

-- | A partial URI prefix that will be prepended to each output in the media
-- .m3u8 file. Can be used if base manifest is delivered from a different
-- URL than the main .m3u8 file.
hlsGroupSettings_baseUrlManifest :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrlManifest = Lens.lens (\HlsGroupSettings' {baseUrlManifest} -> baseUrlManifest) (\s@HlsGroupSettings' {} a -> s {baseUrlManifest = a} :: HlsGroupSettings)

-- | Optional. One value per output group. Complete this field only if you
-- are completing Base URL manifest A, and the downstream system has
-- notified you that the child manifest files for pipeline 1 of all outputs
-- are in a location different from the child manifest files for pipeline
-- 0.
hlsGroupSettings_baseUrlManifest1 :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_baseUrlManifest1 = Lens.lens (\HlsGroupSettings' {baseUrlManifest1} -> baseUrlManifest1) (\s@HlsGroupSettings' {} a -> s {baseUrlManifest1 = a} :: HlsGroupSettings)

-- | Mapping of up to 4 caption channels to caption languages. Is only
-- meaningful if captionLanguageSetting is set to \"insert\".
hlsGroupSettings_captionLanguageMappings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe [CaptionLanguageMapping])
hlsGroupSettings_captionLanguageMappings = Lens.lens (\HlsGroupSettings' {captionLanguageMappings} -> captionLanguageMappings) (\s@HlsGroupSettings' {} a -> s {captionLanguageMappings = a} :: HlsGroupSettings) Prelude.. Lens.mapping Lens.coerced

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
hlsGroupSettings_captionLanguageSetting :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCaptionLanguageSetting)
hlsGroupSettings_captionLanguageSetting = Lens.lens (\HlsGroupSettings' {captionLanguageSetting} -> captionLanguageSetting) (\s@HlsGroupSettings' {} a -> s {captionLanguageSetting = a} :: HlsGroupSettings)

-- | When set to \"disabled\", sets the #EXT-X-ALLOW-CACHE:no tag in the
-- manifest, which prevents clients from saving media segments for later
-- replay.
hlsGroupSettings_clientCache :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsClientCache)
hlsGroupSettings_clientCache = Lens.lens (\HlsGroupSettings' {clientCache} -> clientCache) (\s@HlsGroupSettings' {} a -> s {clientCache = a} :: HlsGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
hlsGroupSettings_codecSpecification :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCodecSpecification)
hlsGroupSettings_codecSpecification = Lens.lens (\HlsGroupSettings' {codecSpecification} -> codecSpecification) (\s@HlsGroupSettings' {} a -> s {codecSpecification = a} :: HlsGroupSettings)

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value
-- represented by a 32-character text string. If ivSource is set to
-- \"explicit\" then this parameter is required and is used as the IV for
-- encryption.
hlsGroupSettings_constantIv :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_constantIv = Lens.lens (\HlsGroupSettings' {constantIv} -> constantIv) (\s@HlsGroupSettings' {} a -> s {constantIv = a} :: HlsGroupSettings)

-- | Place segments in subdirectories.
hlsGroupSettings_directoryStructure :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsDirectoryStructure)
hlsGroupSettings_directoryStructure = Lens.lens (\HlsGroupSettings' {directoryStructure} -> directoryStructure) (\s@HlsGroupSettings' {} a -> s {directoryStructure = a} :: HlsGroupSettings)

-- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child
-- manifests for this output group. Typically, choose Insert because these
-- tags are required in the manifest (according to the HLS specification)
-- and serve an important purpose. Choose Never Insert only if the
-- downstream system is doing real-time failover (without using the
-- MediaLive automatic failover feature) and only if that downstream system
-- has advised you to exclude the tags.
hlsGroupSettings_discontinuityTags :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsDiscontinuityTags)
hlsGroupSettings_discontinuityTags = Lens.lens (\HlsGroupSettings' {discontinuityTags} -> discontinuityTags) (\s@HlsGroupSettings' {} a -> s {discontinuityTags = a} :: HlsGroupSettings)

-- | Encrypts the segments with the given encryption scheme. Exclude this
-- parameter if no encryption is desired.
hlsGroupSettings_encryptionType :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsEncryptionType)
hlsGroupSettings_encryptionType = Lens.lens (\HlsGroupSettings' {encryptionType} -> encryptionType) (\s@HlsGroupSettings' {} a -> s {encryptionType = a} :: HlsGroupSettings)

-- | Parameters that control interactions with the CDN.
hlsGroupSettings_hlsCdnSettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsCdnSettings)
hlsGroupSettings_hlsCdnSettings = Lens.lens (\HlsGroupSettings' {hlsCdnSettings} -> hlsCdnSettings) (\s@HlsGroupSettings' {} a -> s {hlsCdnSettings = a} :: HlsGroupSettings)

-- | State of HLS ID3 Segment Tagging
hlsGroupSettings_hlsId3SegmentTagging :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsId3SegmentTaggingState)
hlsGroupSettings_hlsId3SegmentTagging = Lens.lens (\HlsGroupSettings' {hlsId3SegmentTagging} -> hlsId3SegmentTagging) (\s@HlsGroupSettings' {} a -> s {hlsId3SegmentTagging = a} :: HlsGroupSettings)

-- | DISABLED: Do not create an I-frame-only manifest, but do create the
-- master and media manifests (according to the Output Selection field).
-- STANDARD: Create an I-frame-only manifest for each output that contains
-- video, as well as the other manifests (according to the Output Selection
-- field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to
-- indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries
-- identifying the I-frame position. For example,
-- #EXT-X-BYTERANGE:160364\@1461888\"
hlsGroupSettings_iFrameOnlyPlaylists :: Lens.Lens' HlsGroupSettings (Prelude.Maybe IFrameOnlyPlaylistType)
hlsGroupSettings_iFrameOnlyPlaylists = Lens.lens (\HlsGroupSettings' {iFrameOnlyPlaylists} -> iFrameOnlyPlaylists) (\s@HlsGroupSettings' {} a -> s {iFrameOnlyPlaylists = a} :: HlsGroupSettings)

-- | Specifies whether to include the final (incomplete) segment in the media
-- output when the pipeline stops producing output because of a channel
-- stop, a channel pause or a loss of input to the pipeline. Auto means
-- that MediaLive decides whether to include the final segment, depending
-- on the channel class and the types of output groups. Suppress means to
-- never include the incomplete segment. We recommend you choose Auto and
-- let MediaLive control the behavior.
hlsGroupSettings_incompleteSegmentBehavior :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsIncompleteSegmentBehavior)
hlsGroupSettings_incompleteSegmentBehavior = Lens.lens (\HlsGroupSettings' {incompleteSegmentBehavior} -> incompleteSegmentBehavior) (\s@HlsGroupSettings' {} a -> s {incompleteSegmentBehavior = a} :: HlsGroupSettings)

-- | Applies only if Mode field is LIVE. Specifies the maximum number of
-- segments in the media manifest file. After this maximum, older segments
-- are removed from the media manifest. This number must be smaller than
-- the number in the Keep Segments field.
hlsGroupSettings_indexNSegments :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_indexNSegments = Lens.lens (\HlsGroupSettings' {indexNSegments} -> indexNSegments) (\s@HlsGroupSettings' {} a -> s {indexNSegments = a} :: HlsGroupSettings)

-- | Parameter that control output group behavior on input loss.
hlsGroupSettings_inputLossAction :: Lens.Lens' HlsGroupSettings (Prelude.Maybe InputLossActionForHlsOut)
hlsGroupSettings_inputLossAction = Lens.lens (\HlsGroupSettings' {inputLossAction} -> inputLossAction) (\s@HlsGroupSettings' {} a -> s {inputLossAction = a} :: HlsGroupSettings)

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If set to
-- \"include\", IV is listed in the manifest, otherwise the IV is not in
-- the manifest.
hlsGroupSettings_ivInManifest :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsIvInManifest)
hlsGroupSettings_ivInManifest = Lens.lens (\HlsGroupSettings' {ivInManifest} -> ivInManifest) (\s@HlsGroupSettings' {} a -> s {ivInManifest = a} :: HlsGroupSettings)

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit
-- number used in conjunction with the key for encrypting blocks. If this
-- setting is \"followsSegmentNumber\", it will cause the IV to change
-- every segment (to match the segment number). If this is set to
-- \"explicit\", you must enter a constantIv value.
hlsGroupSettings_ivSource :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsIvSource)
hlsGroupSettings_ivSource = Lens.lens (\HlsGroupSettings' {ivSource} -> ivSource) (\s@HlsGroupSettings' {} a -> s {ivSource = a} :: HlsGroupSettings)

-- | Applies only if Mode field is LIVE. Specifies the number of media
-- segments to retain in the destination directory. This number should be
-- bigger than indexNSegments (Num segments). We recommend (value = (2 x
-- indexNsegments) + 1). If this \"keep segments\" number is too low, the
-- following might happen: the player is still reading a media manifest
-- file that lists this segment, but that segment has been removed from the
-- destination directory (as directed by indexNSegments). This situation
-- would result in a 404 HTTP error on the player.
hlsGroupSettings_keepSegments :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_keepSegments = Lens.lens (\HlsGroupSettings' {keepSegments} -> keepSegments) (\s@HlsGroupSettings' {} a -> s {keepSegments = a} :: HlsGroupSettings)

-- | The value specifies how the key is represented in the resource
-- identified by the URI. If parameter is absent, an implicit value of
-- \"identity\" is used. A reverse DNS string can also be given.
hlsGroupSettings_keyFormat :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_keyFormat = Lens.lens (\HlsGroupSettings' {keyFormat} -> keyFormat) (\s@HlsGroupSettings' {} a -> s {keyFormat = a} :: HlsGroupSettings)

-- | Either a single positive integer version value or a slash delimited list
-- of version values (1\/2\/3).
hlsGroupSettings_keyFormatVersions :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Text)
hlsGroupSettings_keyFormatVersions = Lens.lens (\HlsGroupSettings' {keyFormatVersions} -> keyFormatVersions) (\s@HlsGroupSettings' {} a -> s {keyFormatVersions = a} :: HlsGroupSettings)

-- | The key provider settings.
hlsGroupSettings_keyProviderSettings :: Lens.Lens' HlsGroupSettings (Prelude.Maybe KeyProviderSettings)
hlsGroupSettings_keyProviderSettings = Lens.lens (\HlsGroupSettings' {keyProviderSettings} -> keyProviderSettings) (\s@HlsGroupSettings' {} a -> s {keyProviderSettings = a} :: HlsGroupSettings)

-- | When set to gzip, compresses HLS playlist.
hlsGroupSettings_manifestCompression :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestCompression)
hlsGroupSettings_manifestCompression = Lens.lens (\HlsGroupSettings' {manifestCompression} -> manifestCompression) (\s@HlsGroupSettings' {} a -> s {manifestCompression = a} :: HlsGroupSettings)

-- | Indicates whether the output manifest should use floating point or
-- integer values for segment duration.
hlsGroupSettings_manifestDurationFormat :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsManifestDurationFormat)
hlsGroupSettings_manifestDurationFormat = Lens.lens (\HlsGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@HlsGroupSettings' {} a -> s {manifestDurationFormat = a} :: HlsGroupSettings)

-- | Minimum length of MPEG-2 Transport Stream segments in seconds. When set,
-- minimum segment length is enforced by looking ahead and back within the
-- specified range for a nearby avail and extending the segment size if
-- needed.
hlsGroupSettings_minSegmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_minSegmentLength = Lens.lens (\HlsGroupSettings' {minSegmentLength} -> minSegmentLength) (\s@HlsGroupSettings' {} a -> s {minSegmentLength = a} :: HlsGroupSettings)

-- | If \"vod\", all segments are indexed and kept permanently in the
-- destination and manifest. If \"live\", only the number segments
-- specified in keepSegments and indexNSegments are kept; newer segments
-- replace older segments, which may prevent players from rewinding all the
-- way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE
-- of EVENT while the channel is running, converting it to a \"VOD\" type
-- manifest on completion of the stream.
hlsGroupSettings_mode :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsMode)
hlsGroupSettings_mode = Lens.lens (\HlsGroupSettings' {mode} -> mode) (\s@HlsGroupSettings' {} a -> s {mode = a} :: HlsGroupSettings)

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if
-- applicable, and media manifests) for this output group.
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this
-- output group, but not a master manifest. SEGMENTS_ONLY: Does not
-- generate any manifests for this output group.
hlsGroupSettings_outputSelection :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsOutputSelection)
hlsGroupSettings_outputSelection = Lens.lens (\HlsGroupSettings' {outputSelection} -> outputSelection) (\s@HlsGroupSettings' {} a -> s {outputSelection = a} :: HlsGroupSettings)

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest
-- files. The value is calculated using the program date time clock.
hlsGroupSettings_programDateTime :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsProgramDateTime)
hlsGroupSettings_programDateTime = Lens.lens (\HlsGroupSettings' {programDateTime} -> programDateTime) (\s@HlsGroupSettings' {} a -> s {programDateTime = a} :: HlsGroupSettings)

-- | Specifies the algorithm used to drive the HLS EXT-X-PROGRAM-DATE-TIME
-- clock. Options include: INITIALIZE_FROM_OUTPUT_TIMECODE: The PDT clock
-- is initialized as a function of the first output timecode, then
-- incremented by the EXTINF duration of each encoded segment.
-- SYSTEM_CLOCK: The PDT clock is initialized as a function of the UTC wall
-- clock, then incremented by the EXTINF duration of each encoded segment.
-- If the PDT clock diverges from the wall clock by more than 500ms, it is
-- resynchronized to the wall clock.
hlsGroupSettings_programDateTimeClock :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsProgramDateTimeClock)
hlsGroupSettings_programDateTimeClock = Lens.lens (\HlsGroupSettings' {programDateTimeClock} -> programDateTimeClock) (\s@HlsGroupSettings' {} a -> s {programDateTimeClock = a} :: HlsGroupSettings)

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hlsGroupSettings_programDateTimePeriod :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_programDateTimePeriod = Lens.lens (\HlsGroupSettings' {programDateTimePeriod} -> programDateTimePeriod) (\s@HlsGroupSettings' {} a -> s {programDateTimePeriod = a} :: HlsGroupSettings)

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
hlsGroupSettings_redundantManifest :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsRedundantManifest)
hlsGroupSettings_redundantManifest = Lens.lens (\HlsGroupSettings' {redundantManifest} -> redundantManifest) (\s@HlsGroupSettings' {} a -> s {redundantManifest = a} :: HlsGroupSettings)

-- | Length of MPEG-2 Transport Stream segments to create in seconds. Note
-- that segments will end on the next keyframe after this duration, so
-- actual segment length may be longer.
hlsGroupSettings_segmentLength :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentLength = Lens.lens (\HlsGroupSettings' {segmentLength} -> segmentLength) (\s@HlsGroupSettings' {} a -> s {segmentLength = a} :: HlsGroupSettings)

-- | useInputSegmentation has been deprecated. The configured segment size is
-- always used.
hlsGroupSettings_segmentationMode :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsSegmentationMode)
hlsGroupSettings_segmentationMode = Lens.lens (\HlsGroupSettings' {segmentationMode} -> segmentationMode) (\s@HlsGroupSettings' {} a -> s {segmentationMode = a} :: HlsGroupSettings)

-- | Number of segments to write to a subdirectory before starting a new one.
-- directoryStructure must be subdirectoryPerStream for this setting to
-- have an effect.
hlsGroupSettings_segmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_segmentsPerSubdirectory = Lens.lens (\HlsGroupSettings' {segmentsPerSubdirectory} -> segmentsPerSubdirectory) (\s@HlsGroupSettings' {} a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
hlsGroupSettings_streamInfResolution :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsStreamInfResolution)
hlsGroupSettings_streamInfResolution = Lens.lens (\HlsGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@HlsGroupSettings' {} a -> s {streamInfResolution = a} :: HlsGroupSettings)

-- | Indicates ID3 frame that has the timecode.
hlsGroupSettings_timedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTimedMetadataId3Frame)
hlsGroupSettings_timedMetadataId3Frame = Lens.lens (\HlsGroupSettings' {timedMetadataId3Frame} -> timedMetadataId3Frame) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)

-- | Timed Metadata interval in seconds.
hlsGroupSettings_timedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_timedMetadataId3Period = Lens.lens (\HlsGroupSettings' {timedMetadataId3Period} -> timedMetadataId3Period) (\s@HlsGroupSettings' {} a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hlsGroupSettings_timestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Prelude.Maybe Prelude.Natural)
hlsGroupSettings_timestampDeltaMilliseconds = Lens.lens (\HlsGroupSettings' {timestampDeltaMilliseconds} -> timestampDeltaMilliseconds) (\s@HlsGroupSettings' {} a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media
-- files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program
-- as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE
-- tags to index segments for playback. A typical use for this value is
-- when sending the output to AWS Elemental MediaConvert, which can accept
-- only a single media file. Playback while the channel is running is not
-- guaranteed due to HTTP server caching.
hlsGroupSettings_tsFileMode :: Lens.Lens' HlsGroupSettings (Prelude.Maybe HlsTsFileMode)
hlsGroupSettings_tsFileMode = Lens.lens (\HlsGroupSettings' {tsFileMode} -> tsFileMode) (\s@HlsGroupSettings' {} a -> s {tsFileMode = a} :: HlsGroupSettings)

-- | A directory or HTTP destination for the HLS segments, manifest files,
-- and encryption keys (if enabled).
hlsGroupSettings_destination :: Lens.Lens' HlsGroupSettings OutputLocationRef
hlsGroupSettings_destination = Lens.lens (\HlsGroupSettings' {destination} -> destination) (\s@HlsGroupSettings' {} a -> s {destination = a} :: HlsGroupSettings)

instance Data.FromJSON HlsGroupSettings where
  parseJSON =
    Data.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Prelude.<$> (x Data..:? "adMarkers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "baseUrlContent")
            Prelude.<*> (x Data..:? "baseUrlContent1")
            Prelude.<*> (x Data..:? "baseUrlManifest")
            Prelude.<*> (x Data..:? "baseUrlManifest1")
            Prelude.<*> ( x Data..:? "captionLanguageMappings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "captionLanguageSetting")
            Prelude.<*> (x Data..:? "clientCache")
            Prelude.<*> (x Data..:? "codecSpecification")
            Prelude.<*> (x Data..:? "constantIv")
            Prelude.<*> (x Data..:? "directoryStructure")
            Prelude.<*> (x Data..:? "discontinuityTags")
            Prelude.<*> (x Data..:? "encryptionType")
            Prelude.<*> (x Data..:? "hlsCdnSettings")
            Prelude.<*> (x Data..:? "hlsId3SegmentTagging")
            Prelude.<*> (x Data..:? "iFrameOnlyPlaylists")
            Prelude.<*> (x Data..:? "incompleteSegmentBehavior")
            Prelude.<*> (x Data..:? "indexNSegments")
            Prelude.<*> (x Data..:? "inputLossAction")
            Prelude.<*> (x Data..:? "ivInManifest")
            Prelude.<*> (x Data..:? "ivSource")
            Prelude.<*> (x Data..:? "keepSegments")
            Prelude.<*> (x Data..:? "keyFormat")
            Prelude.<*> (x Data..:? "keyFormatVersions")
            Prelude.<*> (x Data..:? "keyProviderSettings")
            Prelude.<*> (x Data..:? "manifestCompression")
            Prelude.<*> (x Data..:? "manifestDurationFormat")
            Prelude.<*> (x Data..:? "minSegmentLength")
            Prelude.<*> (x Data..:? "mode")
            Prelude.<*> (x Data..:? "outputSelection")
            Prelude.<*> (x Data..:? "programDateTime")
            Prelude.<*> (x Data..:? "programDateTimeClock")
            Prelude.<*> (x Data..:? "programDateTimePeriod")
            Prelude.<*> (x Data..:? "redundantManifest")
            Prelude.<*> (x Data..:? "segmentLength")
            Prelude.<*> (x Data..:? "segmentationMode")
            Prelude.<*> (x Data..:? "segmentsPerSubdirectory")
            Prelude.<*> (x Data..:? "streamInfResolution")
            Prelude.<*> (x Data..:? "timedMetadataId3Frame")
            Prelude.<*> (x Data..:? "timedMetadataId3Period")
            Prelude.<*> (x Data..:? "timestampDeltaMilliseconds")
            Prelude.<*> (x Data..:? "tsFileMode")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable HlsGroupSettings where
  hashWithSalt _salt HlsGroupSettings' {..} =
    _salt `Prelude.hashWithSalt` adMarkers
      `Prelude.hashWithSalt` baseUrlContent
      `Prelude.hashWithSalt` baseUrlContent1
      `Prelude.hashWithSalt` baseUrlManifest
      `Prelude.hashWithSalt` baseUrlManifest1
      `Prelude.hashWithSalt` captionLanguageMappings
      `Prelude.hashWithSalt` captionLanguageSetting
      `Prelude.hashWithSalt` clientCache
      `Prelude.hashWithSalt` codecSpecification
      `Prelude.hashWithSalt` constantIv
      `Prelude.hashWithSalt` directoryStructure
      `Prelude.hashWithSalt` discontinuityTags
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` hlsCdnSettings
      `Prelude.hashWithSalt` hlsId3SegmentTagging
      `Prelude.hashWithSalt` iFrameOnlyPlaylists
      `Prelude.hashWithSalt` incompleteSegmentBehavior
      `Prelude.hashWithSalt` indexNSegments
      `Prelude.hashWithSalt` inputLossAction
      `Prelude.hashWithSalt` ivInManifest
      `Prelude.hashWithSalt` ivSource
      `Prelude.hashWithSalt` keepSegments
      `Prelude.hashWithSalt` keyFormat
      `Prelude.hashWithSalt` keyFormatVersions
      `Prelude.hashWithSalt` keyProviderSettings
      `Prelude.hashWithSalt` manifestCompression
      `Prelude.hashWithSalt` manifestDurationFormat
      `Prelude.hashWithSalt` minSegmentLength
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` outputSelection
      `Prelude.hashWithSalt` programDateTime
      `Prelude.hashWithSalt` programDateTimeClock
      `Prelude.hashWithSalt` programDateTimePeriod
      `Prelude.hashWithSalt` redundantManifest
      `Prelude.hashWithSalt` segmentLength
      `Prelude.hashWithSalt` segmentationMode
      `Prelude.hashWithSalt` segmentsPerSubdirectory
      `Prelude.hashWithSalt` streamInfResolution
      `Prelude.hashWithSalt` timedMetadataId3Frame
      `Prelude.hashWithSalt` timedMetadataId3Period
      `Prelude.hashWithSalt` timestampDeltaMilliseconds
      `Prelude.hashWithSalt` tsFileMode
      `Prelude.hashWithSalt` destination

instance Prelude.NFData HlsGroupSettings where
  rnf HlsGroupSettings' {..} =
    Prelude.rnf adMarkers
      `Prelude.seq` Prelude.rnf baseUrlContent
      `Prelude.seq` Prelude.rnf baseUrlContent1
      `Prelude.seq` Prelude.rnf baseUrlManifest
      `Prelude.seq` Prelude.rnf baseUrlManifest1
      `Prelude.seq` Prelude.rnf captionLanguageMappings
      `Prelude.seq` Prelude.rnf captionLanguageSetting
      `Prelude.seq` Prelude.rnf clientCache
      `Prelude.seq` Prelude.rnf codecSpecification
      `Prelude.seq` Prelude.rnf constantIv
      `Prelude.seq` Prelude.rnf directoryStructure
      `Prelude.seq` Prelude.rnf discontinuityTags
      `Prelude.seq` Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf hlsCdnSettings
      `Prelude.seq` Prelude.rnf hlsId3SegmentTagging
      `Prelude.seq` Prelude.rnf iFrameOnlyPlaylists
      `Prelude.seq` Prelude.rnf
        incompleteSegmentBehavior
      `Prelude.seq` Prelude.rnf indexNSegments
      `Prelude.seq` Prelude.rnf inputLossAction
      `Prelude.seq` Prelude.rnf ivInManifest
      `Prelude.seq` Prelude.rnf ivSource
      `Prelude.seq` Prelude.rnf keepSegments
      `Prelude.seq` Prelude.rnf keyFormat
      `Prelude.seq` Prelude.rnf
        keyFormatVersions
      `Prelude.seq` Prelude.rnf
        keyProviderSettings
      `Prelude.seq` Prelude.rnf
        manifestCompression
      `Prelude.seq` Prelude.rnf
        manifestDurationFormat
      `Prelude.seq` Prelude.rnf
        minSegmentLength
      `Prelude.seq` Prelude.rnf
        mode
      `Prelude.seq` Prelude.rnf
        outputSelection
      `Prelude.seq` Prelude.rnf
        programDateTime
      `Prelude.seq` Prelude.rnf
        programDateTimeClock
      `Prelude.seq` Prelude.rnf
        programDateTimePeriod
      `Prelude.seq` Prelude.rnf
        redundantManifest
      `Prelude.seq` Prelude.rnf
        segmentLength
      `Prelude.seq` Prelude.rnf
        segmentationMode
      `Prelude.seq` Prelude.rnf
        segmentsPerSubdirectory
      `Prelude.seq` Prelude.rnf
        streamInfResolution
      `Prelude.seq` Prelude.rnf
        timedMetadataId3Frame
      `Prelude.seq` Prelude.rnf
        timedMetadataId3Period
      `Prelude.seq` Prelude.rnf
        timestampDeltaMilliseconds
      `Prelude.seq` Prelude.rnf
        tsFileMode
      `Prelude.seq` Prelude.rnf
        destination

instance Data.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adMarkers" Data..=) Prelude.<$> adMarkers,
            ("baseUrlContent" Data..=)
              Prelude.<$> baseUrlContent,
            ("baseUrlContent1" Data..=)
              Prelude.<$> baseUrlContent1,
            ("baseUrlManifest" Data..=)
              Prelude.<$> baseUrlManifest,
            ("baseUrlManifest1" Data..=)
              Prelude.<$> baseUrlManifest1,
            ("captionLanguageMappings" Data..=)
              Prelude.<$> captionLanguageMappings,
            ("captionLanguageSetting" Data..=)
              Prelude.<$> captionLanguageSetting,
            ("clientCache" Data..=) Prelude.<$> clientCache,
            ("codecSpecification" Data..=)
              Prelude.<$> codecSpecification,
            ("constantIv" Data..=) Prelude.<$> constantIv,
            ("directoryStructure" Data..=)
              Prelude.<$> directoryStructure,
            ("discontinuityTags" Data..=)
              Prelude.<$> discontinuityTags,
            ("encryptionType" Data..=)
              Prelude.<$> encryptionType,
            ("hlsCdnSettings" Data..=)
              Prelude.<$> hlsCdnSettings,
            ("hlsId3SegmentTagging" Data..=)
              Prelude.<$> hlsId3SegmentTagging,
            ("iFrameOnlyPlaylists" Data..=)
              Prelude.<$> iFrameOnlyPlaylists,
            ("incompleteSegmentBehavior" Data..=)
              Prelude.<$> incompleteSegmentBehavior,
            ("indexNSegments" Data..=)
              Prelude.<$> indexNSegments,
            ("inputLossAction" Data..=)
              Prelude.<$> inputLossAction,
            ("ivInManifest" Data..=) Prelude.<$> ivInManifest,
            ("ivSource" Data..=) Prelude.<$> ivSource,
            ("keepSegments" Data..=) Prelude.<$> keepSegments,
            ("keyFormat" Data..=) Prelude.<$> keyFormat,
            ("keyFormatVersions" Data..=)
              Prelude.<$> keyFormatVersions,
            ("keyProviderSettings" Data..=)
              Prelude.<$> keyProviderSettings,
            ("manifestCompression" Data..=)
              Prelude.<$> manifestCompression,
            ("manifestDurationFormat" Data..=)
              Prelude.<$> manifestDurationFormat,
            ("minSegmentLength" Data..=)
              Prelude.<$> minSegmentLength,
            ("mode" Data..=) Prelude.<$> mode,
            ("outputSelection" Data..=)
              Prelude.<$> outputSelection,
            ("programDateTime" Data..=)
              Prelude.<$> programDateTime,
            ("programDateTimeClock" Data..=)
              Prelude.<$> programDateTimeClock,
            ("programDateTimePeriod" Data..=)
              Prelude.<$> programDateTimePeriod,
            ("redundantManifest" Data..=)
              Prelude.<$> redundantManifest,
            ("segmentLength" Data..=) Prelude.<$> segmentLength,
            ("segmentationMode" Data..=)
              Prelude.<$> segmentationMode,
            ("segmentsPerSubdirectory" Data..=)
              Prelude.<$> segmentsPerSubdirectory,
            ("streamInfResolution" Data..=)
              Prelude.<$> streamInfResolution,
            ("timedMetadataId3Frame" Data..=)
              Prelude.<$> timedMetadataId3Frame,
            ("timedMetadataId3Period" Data..=)
              Prelude.<$> timedMetadataId3Period,
            ("timestampDeltaMilliseconds" Data..=)
              Prelude.<$> timestampDeltaMilliseconds,
            ("tsFileMode" Data..=) Prelude.<$> tsFileMode,
            Prelude.Just ("destination" Data..= destination)
          ]
      )

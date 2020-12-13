{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsGroupSettings
  ( HlsGroupSettings (..),

    -- * Smart constructor
    mkHlsGroupSettings,

    -- * Lenses
    hgsDirectoryStructure,
    hgsDestination,
    hgsEncryptionType,
    hgsTimedMetadataId3Period,
    hgsIvInManifest,
    hgsDiscontinuityTags,
    hgsTsFileMode,
    hgsMinSegmentLength,
    hgsIFrameOnlyPlaylists,
    hgsProgramDateTime,
    hgsIndexNSegments,
    hgsProgramDateTimePeriod,
    hgsCodecSpecification,
    hgsHlsCdnSettings,
    hgsCaptionLanguageMappings,
    hgsInputLossAction,
    hgsMode,
    hgsKeyProviderSettings,
    hgsIncompleteSegmentBehavior,
    hgsConstantIv,
    hgsBaseURLManifest,
    hgsAdMarkers,
    hgsKeyFormat,
    hgsSegmentLength,
    hgsHlsId3SegmentTagging,
    hgsTimedMetadataId3Frame,
    hgsBaseURLContent,
    hgsOutputSelection,
    hgsCaptionLanguageSetting,
    hgsSegmentsPerSubdirectory,
    hgsManifestDurationFormat,
    hgsIvSource,
    hgsSegmentationMode,
    hgsKeyFormatVersions,
    hgsClientCache,
    hgsTimestampDeltaMilliseconds,
    hgsBaseURLManifest1,
    hgsRedundantManifest,
    hgsStreamInfResolution,
    hgsKeepSegments,
    hgsBaseURLContent1,
    hgsManifestCompression,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Hls Group Settings
--
-- /See:/ 'mkHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Place segments in subdirectories.
    directoryStructure :: Lude.Maybe HlsDirectoryStructure,
    -- | A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
    destination :: OutputLocationRef,
    -- | Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
    encryptionType :: Lude.Maybe HlsEncryptionType,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Lude.Maybe Lude.Natural,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
    ivInManifest :: Lude.Maybe HlsIvInManifest,
    -- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group.
    --
    -- Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose.
    -- Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
    discontinuityTags :: Lude.Maybe HlsDiscontinuityTags,
    -- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media files.
    --
    --
    -- SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
    tsFileMode :: Lude.Maybe HlsTsFileMode,
    -- | When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
    minSegmentLength :: Lude.Maybe Lude.Natural,
    -- | DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field).
    --
    --
    -- STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
    iFrameOnlyPlaylists :: Lude.Maybe IFrameOnlyPlaylistType,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
    programDateTime :: Lude.Maybe HlsProgramDateTime,
    -- | Applies only if Mode field is LIVE.
    --
    --
    -- Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
    indexNSegments :: Lude.Maybe Lude.Natural,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Lude.Maybe Lude.Natural,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
    codecSpecification :: Lude.Maybe HlsCodecSpecification,
    -- | Parameters that control interactions with the CDN.
    hlsCdnSettings :: Lude.Maybe HlsCdnSettings,
    -- | Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
    captionLanguageMappings :: Lude.Maybe [CaptionLanguageMapping],
    -- | Parameter that control output group behavior on input loss.
    inputLossAction :: Lude.Maybe InputLossActionForHlsOut,
    -- | If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event.
    --
    --
    -- VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
    mode :: Lude.Maybe HlsMode,
    -- | The key provider settings.
    keyProviderSettings :: Lude.Maybe KeyProviderSettings,
    -- | Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline.
    --
    -- Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups.
    -- Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
    incompleteSegmentBehavior :: Lude.Maybe HlsIncompleteSegmentBehavior,
    -- | For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
    constantIv :: Lude.Maybe Lude.Text,
    -- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
    baseURLManifest :: Lude.Maybe Lude.Text,
    -- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
    adMarkers :: Lude.Maybe [HlsAdMarkers],
    -- | The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
    keyFormat :: Lude.Maybe Lude.Text,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
    segmentLength :: Lude.Maybe Lude.Natural,
    -- | State of HLS ID3 Segment Tagging
    hlsId3SegmentTagging :: Lude.Maybe HlsId3SegmentTaggingState,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Lude.Maybe HlsTimedMetadataId3Frame,
    -- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
    baseURLContent :: Lude.Maybe Lude.Text,
    -- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group.
    --
    --
    -- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest.
    --
    -- SEGMENTS_ONLY: Does not generate any manifests for this output group.
    outputSelection :: Lude.Maybe HlsOutputSelection,
    -- | Applies only to 608 Embedded output captions.
    --
    -- insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions.
    -- none: Include CLOSED-CAPTIONS=NONE line in the manifest.
    -- omit: Omit any CLOSED-CAPTIONS line from the manifest.
    captionLanguageSetting :: Lude.Maybe HlsCaptionLanguageSetting,
    -- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
    segmentsPerSubdirectory :: Lude.Maybe Lude.Natural,
    -- | Indicates whether the output manifest should use floating point or integer values for segment duration.
    manifestDurationFormat :: Lude.Maybe HlsManifestDurationFormat,
    -- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
    ivSource :: Lude.Maybe HlsIvSource,
    -- | useInputSegmentation has been deprecated. The configured segment size is always used.
    segmentationMode :: Lude.Maybe HlsSegmentationMode,
    -- | Either a single positive integer version value or a slash delimited list of version values (1/2/3).
    keyFormatVersions :: Lude.Maybe Lude.Text,
    -- | When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
    clientCache :: Lude.Maybe HlsClientCache,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Lude.Maybe Lude.Natural,
    -- | Optional. One value per output group.
    --
    --
    -- Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
    baseURLManifest1 :: Lude.Maybe Lude.Text,
    -- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines.
    --
    --
    -- DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only.
    --
    -- For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
    redundantManifest :: Lude.Maybe HlsRedundantManifest,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
    streamInfResolution :: Lude.Maybe HlsStreamInfResolution,
    -- | Applies only if Mode field is LIVE.
    --
    --
    -- Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1).
    --
    -- If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
    keepSegments :: Lude.Maybe Lude.Natural,
    -- | Optional. One value per output group.
    --
    --
    -- This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
    baseURLContent1 :: Lude.Maybe Lude.Text,
    -- | When set to gzip, compresses HLS playlist.
    manifestCompression :: Lude.Maybe HlsManifestCompression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- * 'directoryStructure' - Place segments in subdirectories.
-- * 'destination' - A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
-- * 'encryptionType' - Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
-- * 'timedMetadataId3Period' - Timed Metadata interval in seconds.
-- * 'ivInManifest' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
-- * 'discontinuityTags' - Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group.
--
-- Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose.
-- Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
-- * 'tsFileMode' - SEGMENTED_FILES: Emit the program as segments - multiple .ts media files.
--
--
-- SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
-- * 'minSegmentLength' - When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
-- * 'iFrameOnlyPlaylists' - DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field).
--
--
-- STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
-- * 'programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
-- * 'indexNSegments' - Applies only if Mode field is LIVE.
--
--
-- Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
-- * 'programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
-- * 'codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
-- * 'hlsCdnSettings' - Parameters that control interactions with the CDN.
-- * 'captionLanguageMappings' - Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
-- * 'inputLossAction' - Parameter that control output group behavior on input loss.
-- * 'mode' - If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event.
--
--
-- VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
-- * 'keyProviderSettings' - The key provider settings.
-- * 'incompleteSegmentBehavior' - Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline.
--
-- Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups.
-- Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
-- * 'constantIv' - For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
-- * 'baseURLManifest' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
-- * 'adMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
-- * 'keyFormat' - The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
-- * 'segmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
-- * 'hlsId3SegmentTagging' - State of HLS ID3 Segment Tagging
-- * 'timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
-- * 'baseURLContent' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
-- * 'outputSelection' - MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group.
--
--
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest.
--
-- SEGMENTS_ONLY: Does not generate any manifests for this output group.
-- * 'captionLanguageSetting' - Applies only to 608 Embedded output captions.
--
-- insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions.
-- none: Include CLOSED-CAPTIONS=NONE line in the manifest.
-- omit: Omit any CLOSED-CAPTIONS line from the manifest.
-- * 'segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
-- * 'manifestDurationFormat' - Indicates whether the output manifest should use floating point or integer values for segment duration.
-- * 'ivSource' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
-- * 'segmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
-- * 'keyFormatVersions' - Either a single positive integer version value or a slash delimited list of version values (1/2/3).
-- * 'clientCache' - When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
-- * 'timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
-- * 'baseURLManifest1' - Optional. One value per output group.
--
--
-- Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
-- * 'redundantManifest' - ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines.
--
--
-- DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only.
--
-- For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
-- * 'streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
-- * 'keepSegments' - Applies only if Mode field is LIVE.
--
--
-- Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1).
--
-- If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
-- * 'baseURLContent1' - Optional. One value per output group.
--
--
-- This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
-- * 'manifestCompression' - When set to gzip, compresses HLS playlist.
mkHlsGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  HlsGroupSettings
mkHlsGroupSettings pDestination_ =
  HlsGroupSettings'
    { directoryStructure = Lude.Nothing,
      destination = pDestination_,
      encryptionType = Lude.Nothing,
      timedMetadataId3Period = Lude.Nothing,
      ivInManifest = Lude.Nothing,
      discontinuityTags = Lude.Nothing,
      tsFileMode = Lude.Nothing,
      minSegmentLength = Lude.Nothing,
      iFrameOnlyPlaylists = Lude.Nothing,
      programDateTime = Lude.Nothing,
      indexNSegments = Lude.Nothing,
      programDateTimePeriod = Lude.Nothing,
      codecSpecification = Lude.Nothing,
      hlsCdnSettings = Lude.Nothing,
      captionLanguageMappings = Lude.Nothing,
      inputLossAction = Lude.Nothing,
      mode = Lude.Nothing,
      keyProviderSettings = Lude.Nothing,
      incompleteSegmentBehavior = Lude.Nothing,
      constantIv = Lude.Nothing,
      baseURLManifest = Lude.Nothing,
      adMarkers = Lude.Nothing,
      keyFormat = Lude.Nothing,
      segmentLength = Lude.Nothing,
      hlsId3SegmentTagging = Lude.Nothing,
      timedMetadataId3Frame = Lude.Nothing,
      baseURLContent = Lude.Nothing,
      outputSelection = Lude.Nothing,
      captionLanguageSetting = Lude.Nothing,
      segmentsPerSubdirectory = Lude.Nothing,
      manifestDurationFormat = Lude.Nothing,
      ivSource = Lude.Nothing,
      segmentationMode = Lude.Nothing,
      keyFormatVersions = Lude.Nothing,
      clientCache = Lude.Nothing,
      timestampDeltaMilliseconds = Lude.Nothing,
      baseURLManifest1 = Lude.Nothing,
      redundantManifest = Lude.Nothing,
      streamInfResolution = Lude.Nothing,
      keepSegments = Lude.Nothing,
      baseURLContent1 = Lude.Nothing,
      manifestCompression = Lude.Nothing
    }

-- | Place segments in subdirectories.
--
-- /Note:/ Consider using 'directoryStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDirectoryStructure :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsDirectoryStructure)
hgsDirectoryStructure = Lens.lens (directoryStructure :: HlsGroupSettings -> Lude.Maybe HlsDirectoryStructure) (\s a -> s {directoryStructure = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDirectoryStructure "Use generic-lens or generic-optics with 'directoryStructure' instead." #-}

-- | A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestination :: Lens.Lens' HlsGroupSettings OutputLocationRef
hgsDestination = Lens.lens (destination :: HlsGroupSettings -> OutputLocationRef) (\s a -> s {destination = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsEncryptionType :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsEncryptionType)
hgsEncryptionType = Lens.lens (encryptionType :: HlsGroupSettings -> Lude.Maybe HlsEncryptionType) (\s a -> s {encryptionType = a} :: HlsGroupSettings)
{-# DEPRECATED hgsEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsTimedMetadataId3Period = Lens.lens (timedMetadataId3Period :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead." #-}

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
--
-- /Note:/ Consider using 'ivInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIvInManifest :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsIvInManifest)
hgsIvInManifest = Lens.lens (ivInManifest :: HlsGroupSettings -> Lude.Maybe HlsIvInManifest) (\s a -> s {ivInManifest = a} :: HlsGroupSettings)
{-# DEPRECATED hgsIvInManifest "Use generic-lens or generic-optics with 'ivInManifest' instead." #-}

-- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group.
--
-- Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose.
-- Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
--
-- /Note:/ Consider using 'discontinuityTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDiscontinuityTags :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsDiscontinuityTags)
hgsDiscontinuityTags = Lens.lens (discontinuityTags :: HlsGroupSettings -> Lude.Maybe HlsDiscontinuityTags) (\s a -> s {discontinuityTags = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDiscontinuityTags "Use generic-lens or generic-optics with 'discontinuityTags' instead." #-}

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media files.
--
--
-- SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
--
-- /Note:/ Consider using 'tsFileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTsFileMode :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsTsFileMode)
hgsTsFileMode = Lens.lens (tsFileMode :: HlsGroupSettings -> Lude.Maybe HlsTsFileMode) (\s a -> s {tsFileMode = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTsFileMode "Use generic-lens or generic-optics with 'tsFileMode' instead." #-}

-- | When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- /Note:/ Consider using 'minSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinSegmentLength :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsMinSegmentLength = Lens.lens (minSegmentLength :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minSegmentLength = a} :: HlsGroupSettings)
{-# DEPRECATED hgsMinSegmentLength "Use generic-lens or generic-optics with 'minSegmentLength' instead." #-}

-- | DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field).
--
--
-- STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
--
-- /Note:/ Consider using 'iFrameOnlyPlaylists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIFrameOnlyPlaylists :: Lens.Lens' HlsGroupSettings (Lude.Maybe IFrameOnlyPlaylistType)
hgsIFrameOnlyPlaylists = Lens.lens (iFrameOnlyPlaylists :: HlsGroupSettings -> Lude.Maybe IFrameOnlyPlaylistType) (\s a -> s {iFrameOnlyPlaylists = a} :: HlsGroupSettings)
{-# DEPRECATED hgsIFrameOnlyPlaylists "Use generic-lens or generic-optics with 'iFrameOnlyPlaylists' instead." #-}

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
--
-- /Note:/ Consider using 'programDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTime :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsProgramDateTime)
hgsProgramDateTime = Lens.lens (programDateTime :: HlsGroupSettings -> Lude.Maybe HlsProgramDateTime) (\s a -> s {programDateTime = a} :: HlsGroupSettings)
{-# DEPRECATED hgsProgramDateTime "Use generic-lens or generic-optics with 'programDateTime' instead." #-}

-- | Applies only if Mode field is LIVE.
--
--
-- Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
--
-- /Note:/ Consider using 'indexNSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIndexNSegments :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsIndexNSegments = Lens.lens (indexNSegments :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {indexNSegments = a} :: HlsGroupSettings)
{-# DEPRECATED hgsIndexNSegments "Use generic-lens or generic-optics with 'indexNSegments' instead." #-}

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- /Note:/ Consider using 'programDateTimePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTimePeriod :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsProgramDateTimePeriod = Lens.lens (programDateTimePeriod :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {programDateTimePeriod = a} :: HlsGroupSettings)
{-# DEPRECATED hgsProgramDateTimePeriod "Use generic-lens or generic-optics with 'programDateTimePeriod' instead." #-}

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- /Note:/ Consider using 'codecSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCodecSpecification :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsCodecSpecification)
hgsCodecSpecification = Lens.lens (codecSpecification :: HlsGroupSettings -> Lude.Maybe HlsCodecSpecification) (\s a -> s {codecSpecification = a} :: HlsGroupSettings)
{-# DEPRECATED hgsCodecSpecification "Use generic-lens or generic-optics with 'codecSpecification' instead." #-}

-- | Parameters that control interactions with the CDN.
--
-- /Note:/ Consider using 'hlsCdnSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsHlsCdnSettings :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsCdnSettings)
hgsHlsCdnSettings = Lens.lens (hlsCdnSettings :: HlsGroupSettings -> Lude.Maybe HlsCdnSettings) (\s a -> s {hlsCdnSettings = a} :: HlsGroupSettings)
{-# DEPRECATED hgsHlsCdnSettings "Use generic-lens or generic-optics with 'hlsCdnSettings' instead." #-}

-- | Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
--
-- /Note:/ Consider using 'captionLanguageMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageMappings :: Lens.Lens' HlsGroupSettings (Lude.Maybe [CaptionLanguageMapping])
hgsCaptionLanguageMappings = Lens.lens (captionLanguageMappings :: HlsGroupSettings -> Lude.Maybe [CaptionLanguageMapping]) (\s a -> s {captionLanguageMappings = a} :: HlsGroupSettings)
{-# DEPRECATED hgsCaptionLanguageMappings "Use generic-lens or generic-optics with 'captionLanguageMappings' instead." #-}

-- | Parameter that control output group behavior on input loss.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsInputLossAction :: Lens.Lens' HlsGroupSettings (Lude.Maybe InputLossActionForHlsOut)
hgsInputLossAction = Lens.lens (inputLossAction :: HlsGroupSettings -> Lude.Maybe InputLossActionForHlsOut) (\s a -> s {inputLossAction = a} :: HlsGroupSettings)
{-# DEPRECATED hgsInputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead." #-}

-- | If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event.
--
--
-- VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMode :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsMode)
hgsMode = Lens.lens (mode :: HlsGroupSettings -> Lude.Maybe HlsMode) (\s a -> s {mode = a} :: HlsGroupSettings)
{-# DEPRECATED hgsMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The key provider settings.
--
-- /Note:/ Consider using 'keyProviderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyProviderSettings :: Lens.Lens' HlsGroupSettings (Lude.Maybe KeyProviderSettings)
hgsKeyProviderSettings = Lens.lens (keyProviderSettings :: HlsGroupSettings -> Lude.Maybe KeyProviderSettings) (\s a -> s {keyProviderSettings = a} :: HlsGroupSettings)
{-# DEPRECATED hgsKeyProviderSettings "Use generic-lens or generic-optics with 'keyProviderSettings' instead." #-}

-- | Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline.
--
-- Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups.
-- Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
--
-- /Note:/ Consider using 'incompleteSegmentBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIncompleteSegmentBehavior :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsIncompleteSegmentBehavior)
hgsIncompleteSegmentBehavior = Lens.lens (incompleteSegmentBehavior :: HlsGroupSettings -> Lude.Maybe HlsIncompleteSegmentBehavior) (\s a -> s {incompleteSegmentBehavior = a} :: HlsGroupSettings)
{-# DEPRECATED hgsIncompleteSegmentBehavior "Use generic-lens or generic-optics with 'incompleteSegmentBehavior' instead." #-}

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
--
-- /Note:/ Consider using 'constantIv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsConstantIv :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsConstantIv = Lens.lens (constantIv :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {constantIv = a} :: HlsGroupSettings)
{-# DEPRECATED hgsConstantIv "Use generic-lens or generic-optics with 'constantIv' instead." #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseURLManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseURLManifest :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsBaseURLManifest = Lens.lens (baseURLManifest :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURLManifest = a} :: HlsGroupSettings)
{-# DEPRECATED hgsBaseURLManifest "Use generic-lens or generic-optics with 'baseURLManifest' instead." #-}

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdMarkers :: Lens.Lens' HlsGroupSettings (Lude.Maybe [HlsAdMarkers])
hgsAdMarkers = Lens.lens (adMarkers :: HlsGroupSettings -> Lude.Maybe [HlsAdMarkers]) (\s a -> s {adMarkers = a} :: HlsGroupSettings)
{-# DEPRECATED hgsAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
--
-- /Note:/ Consider using 'keyFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyFormat :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsKeyFormat = Lens.lens (keyFormat :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {keyFormat = a} :: HlsGroupSettings)
{-# DEPRECATED hgsKeyFormat "Use generic-lens or generic-optics with 'keyFormat' instead." #-}

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentLength :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsSegmentLength = Lens.lens (segmentLength :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentLength = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | State of HLS ID3 Segment Tagging
--
-- /Note:/ Consider using 'hlsId3SegmentTagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsHlsId3SegmentTagging :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsId3SegmentTaggingState)
hgsHlsId3SegmentTagging = Lens.lens (hlsId3SegmentTagging :: HlsGroupSettings -> Lude.Maybe HlsId3SegmentTaggingState) (\s a -> s {hlsId3SegmentTagging = a} :: HlsGroupSettings)
{-# DEPRECATED hgsHlsId3SegmentTagging "Use generic-lens or generic-optics with 'hlsId3SegmentTagging' instead." #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = Lens.lens (timedMetadataId3Frame :: HlsGroupSettings -> Lude.Maybe HlsTimedMetadataId3Frame) (\s a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead." #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseURLContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseURLContent :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsBaseURLContent = Lens.lens (baseURLContent :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURLContent = a} :: HlsGroupSettings)
{-# DEPRECATED hgsBaseURLContent "Use generic-lens or generic-optics with 'baseURLContent' instead." #-}

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group.
--
--
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest.
--
-- SEGMENTS_ONLY: Does not generate any manifests for this output group.
--
-- /Note:/ Consider using 'outputSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsOutputSelection :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsOutputSelection)
hgsOutputSelection = Lens.lens (outputSelection :: HlsGroupSettings -> Lude.Maybe HlsOutputSelection) (\s a -> s {outputSelection = a} :: HlsGroupSettings)
{-# DEPRECATED hgsOutputSelection "Use generic-lens or generic-optics with 'outputSelection' instead." #-}

-- | Applies only to 608 Embedded output captions.
--
-- insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions.
-- none: Include CLOSED-CAPTIONS=NONE line in the manifest.
-- omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- /Note:/ Consider using 'captionLanguageSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageSetting :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = Lens.lens (captionLanguageSetting :: HlsGroupSettings -> Lude.Maybe HlsCaptionLanguageSetting) (\s a -> s {captionLanguageSetting = a} :: HlsGroupSettings)
{-# DEPRECATED hgsCaptionLanguageSetting "Use generic-lens or generic-optics with 'captionLanguageSetting' instead." #-}

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
--
-- /Note:/ Consider using 'segmentsPerSubdirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsSegmentsPerSubdirectory = Lens.lens (segmentsPerSubdirectory :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentsPerSubdirectory "Use generic-lens or generic-optics with 'segmentsPerSubdirectory' instead." #-}

-- | Indicates whether the output manifest should use floating point or integer values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestDurationFormat :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = Lens.lens (manifestDurationFormat :: HlsGroupSettings -> Lude.Maybe HlsManifestDurationFormat) (\s a -> s {manifestDurationFormat = a} :: HlsGroupSettings)
{-# DEPRECATED hgsManifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead." #-}

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
--
-- /Note:/ Consider using 'ivSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIvSource :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsIvSource)
hgsIvSource = Lens.lens (ivSource :: HlsGroupSettings -> Lude.Maybe HlsIvSource) (\s a -> s {ivSource = a} :: HlsGroupSettings)
{-# DEPRECATED hgsIvSource "Use generic-lens or generic-optics with 'ivSource' instead." #-}

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- /Note:/ Consider using 'segmentationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentationMode :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsSegmentationMode)
hgsSegmentationMode = Lens.lens (segmentationMode :: HlsGroupSettings -> Lude.Maybe HlsSegmentationMode) (\s a -> s {segmentationMode = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentationMode "Use generic-lens or generic-optics with 'segmentationMode' instead." #-}

-- | Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- /Note:/ Consider using 'keyFormatVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyFormatVersions :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsKeyFormatVersions = Lens.lens (keyFormatVersions :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {keyFormatVersions = a} :: HlsGroupSettings)
{-# DEPRECATED hgsKeyFormatVersions "Use generic-lens or generic-optics with 'keyFormatVersions' instead." #-}

-- | When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsClientCache :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsClientCache)
hgsClientCache = Lens.lens (clientCache :: HlsGroupSettings -> Lude.Maybe HlsClientCache) (\s a -> s {clientCache = a} :: HlsGroupSettings)
{-# DEPRECATED hgsClientCache "Use generic-lens or generic-optics with 'clientCache' instead." #-}

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- /Note:/ Consider using 'timestampDeltaMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsTimestampDeltaMilliseconds = Lens.lens (timestampDeltaMilliseconds :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimestampDeltaMilliseconds "Use generic-lens or generic-optics with 'timestampDeltaMilliseconds' instead." #-}

-- | Optional. One value per output group.
--
--
-- Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
--
-- /Note:/ Consider using 'baseURLManifest1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseURLManifest1 :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsBaseURLManifest1 = Lens.lens (baseURLManifest1 :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURLManifest1 = a} :: HlsGroupSettings)
{-# DEPRECATED hgsBaseURLManifest1 "Use generic-lens or generic-optics with 'baseURLManifest1' instead." #-}

-- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines.
--
--
-- DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only.
--
-- For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
--
-- /Note:/ Consider using 'redundantManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsRedundantManifest :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsRedundantManifest)
hgsRedundantManifest = Lens.lens (redundantManifest :: HlsGroupSettings -> Lude.Maybe HlsRedundantManifest) (\s a -> s {redundantManifest = a} :: HlsGroupSettings)
{-# DEPRECATED hgsRedundantManifest "Use generic-lens or generic-optics with 'redundantManifest' instead." #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsStreamInfResolution :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsStreamInfResolution)
hgsStreamInfResolution = Lens.lens (streamInfResolution :: HlsGroupSettings -> Lude.Maybe HlsStreamInfResolution) (\s a -> s {streamInfResolution = a} :: HlsGroupSettings)
{-# DEPRECATED hgsStreamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead." #-}

-- | Applies only if Mode field is LIVE.
--
--
-- Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1).
--
-- If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
--
-- /Note:/ Consider using 'keepSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeepSegments :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsKeepSegments = Lens.lens (keepSegments :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {keepSegments = a} :: HlsGroupSettings)
{-# DEPRECATED hgsKeepSegments "Use generic-lens or generic-optics with 'keepSegments' instead." #-}

-- | Optional. One value per output group.
--
--
-- This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
--
-- /Note:/ Consider using 'baseURLContent1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseURLContent1 :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsBaseURLContent1 = Lens.lens (baseURLContent1 :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURLContent1 = a} :: HlsGroupSettings)
{-# DEPRECATED hgsBaseURLContent1 "Use generic-lens or generic-optics with 'baseURLContent1' instead." #-}

-- | When set to gzip, compresses HLS playlist.
--
-- /Note:/ Consider using 'manifestCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestCompression :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsManifestCompression)
hgsManifestCompression = Lens.lens (manifestCompression :: HlsGroupSettings -> Lude.Maybe HlsManifestCompression) (\s a -> s {manifestCompression = a} :: HlsGroupSettings)
{-# DEPRECATED hgsManifestCompression "Use generic-lens or generic-optics with 'manifestCompression' instead." #-}

instance Lude.FromJSON HlsGroupSettings where
  parseJSON =
    Lude.withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            Lude.<$> (x Lude..:? "directoryStructure")
            Lude.<*> (x Lude..: "destination")
            Lude.<*> (x Lude..:? "encryptionType")
            Lude.<*> (x Lude..:? "timedMetadataId3Period")
            Lude.<*> (x Lude..:? "ivInManifest")
            Lude.<*> (x Lude..:? "discontinuityTags")
            Lude.<*> (x Lude..:? "tsFileMode")
            Lude.<*> (x Lude..:? "minSegmentLength")
            Lude.<*> (x Lude..:? "iFrameOnlyPlaylists")
            Lude.<*> (x Lude..:? "programDateTime")
            Lude.<*> (x Lude..:? "indexNSegments")
            Lude.<*> (x Lude..:? "programDateTimePeriod")
            Lude.<*> (x Lude..:? "codecSpecification")
            Lude.<*> (x Lude..:? "hlsCdnSettings")
            Lude.<*> (x Lude..:? "captionLanguageMappings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inputLossAction")
            Lude.<*> (x Lude..:? "mode")
            Lude.<*> (x Lude..:? "keyProviderSettings")
            Lude.<*> (x Lude..:? "incompleteSegmentBehavior")
            Lude.<*> (x Lude..:? "constantIv")
            Lude.<*> (x Lude..:? "baseUrlManifest")
            Lude.<*> (x Lude..:? "adMarkers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "keyFormat")
            Lude.<*> (x Lude..:? "segmentLength")
            Lude.<*> (x Lude..:? "hlsId3SegmentTagging")
            Lude.<*> (x Lude..:? "timedMetadataId3Frame")
            Lude.<*> (x Lude..:? "baseUrlContent")
            Lude.<*> (x Lude..:? "outputSelection")
            Lude.<*> (x Lude..:? "captionLanguageSetting")
            Lude.<*> (x Lude..:? "segmentsPerSubdirectory")
            Lude.<*> (x Lude..:? "manifestDurationFormat")
            Lude.<*> (x Lude..:? "ivSource")
            Lude.<*> (x Lude..:? "segmentationMode")
            Lude.<*> (x Lude..:? "keyFormatVersions")
            Lude.<*> (x Lude..:? "clientCache")
            Lude.<*> (x Lude..:? "timestampDeltaMilliseconds")
            Lude.<*> (x Lude..:? "baseUrlManifest1")
            Lude.<*> (x Lude..:? "redundantManifest")
            Lude.<*> (x Lude..:? "streamInfResolution")
            Lude.<*> (x Lude..:? "keepSegments")
            Lude.<*> (x Lude..:? "baseUrlContent1")
            Lude.<*> (x Lude..:? "manifestCompression")
      )

instance Lude.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("directoryStructure" Lude..=) Lude.<$> directoryStructure,
            Lude.Just ("destination" Lude..= destination),
            ("encryptionType" Lude..=) Lude.<$> encryptionType,
            ("timedMetadataId3Period" Lude..=) Lude.<$> timedMetadataId3Period,
            ("ivInManifest" Lude..=) Lude.<$> ivInManifest,
            ("discontinuityTags" Lude..=) Lude.<$> discontinuityTags,
            ("tsFileMode" Lude..=) Lude.<$> tsFileMode,
            ("minSegmentLength" Lude..=) Lude.<$> minSegmentLength,
            ("iFrameOnlyPlaylists" Lude..=) Lude.<$> iFrameOnlyPlaylists,
            ("programDateTime" Lude..=) Lude.<$> programDateTime,
            ("indexNSegments" Lude..=) Lude.<$> indexNSegments,
            ("programDateTimePeriod" Lude..=) Lude.<$> programDateTimePeriod,
            ("codecSpecification" Lude..=) Lude.<$> codecSpecification,
            ("hlsCdnSettings" Lude..=) Lude.<$> hlsCdnSettings,
            ("captionLanguageMappings" Lude..=)
              Lude.<$> captionLanguageMappings,
            ("inputLossAction" Lude..=) Lude.<$> inputLossAction,
            ("mode" Lude..=) Lude.<$> mode,
            ("keyProviderSettings" Lude..=) Lude.<$> keyProviderSettings,
            ("incompleteSegmentBehavior" Lude..=)
              Lude.<$> incompleteSegmentBehavior,
            ("constantIv" Lude..=) Lude.<$> constantIv,
            ("baseUrlManifest" Lude..=) Lude.<$> baseURLManifest,
            ("adMarkers" Lude..=) Lude.<$> adMarkers,
            ("keyFormat" Lude..=) Lude.<$> keyFormat,
            ("segmentLength" Lude..=) Lude.<$> segmentLength,
            ("hlsId3SegmentTagging" Lude..=) Lude.<$> hlsId3SegmentTagging,
            ("timedMetadataId3Frame" Lude..=) Lude.<$> timedMetadataId3Frame,
            ("baseUrlContent" Lude..=) Lude.<$> baseURLContent,
            ("outputSelection" Lude..=) Lude.<$> outputSelection,
            ("captionLanguageSetting" Lude..=) Lude.<$> captionLanguageSetting,
            ("segmentsPerSubdirectory" Lude..=)
              Lude.<$> segmentsPerSubdirectory,
            ("manifestDurationFormat" Lude..=) Lude.<$> manifestDurationFormat,
            ("ivSource" Lude..=) Lude.<$> ivSource,
            ("segmentationMode" Lude..=) Lude.<$> segmentationMode,
            ("keyFormatVersions" Lude..=) Lude.<$> keyFormatVersions,
            ("clientCache" Lude..=) Lude.<$> clientCache,
            ("timestampDeltaMilliseconds" Lude..=)
              Lude.<$> timestampDeltaMilliseconds,
            ("baseUrlManifest1" Lude..=) Lude.<$> baseURLManifest1,
            ("redundantManifest" Lude..=) Lude.<$> redundantManifest,
            ("streamInfResolution" Lude..=) Lude.<$> streamInfResolution,
            ("keepSegments" Lude..=) Lude.<$> keepSegments,
            ("baseUrlContent1" Lude..=) Lude.<$> baseURLContent1,
            ("manifestCompression" Lude..=) Lude.<$> manifestCompression
          ]
      )

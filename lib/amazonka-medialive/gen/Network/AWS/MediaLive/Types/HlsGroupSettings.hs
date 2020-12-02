{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsGroupSettings where

import Network.AWS.Lens
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
import Network.AWS.Prelude

-- | Hls Group Settings
--
-- /See:/ 'hlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { _hgsDirectoryStructure ::
      !(Maybe HlsDirectoryStructure),
    _hgsEncryptionType :: !(Maybe HlsEncryptionType),
    _hgsTimedMetadataId3Period :: !(Maybe Nat),
    _hgsIvInManifest :: !(Maybe HlsIvInManifest),
    _hgsDiscontinuityTags :: !(Maybe HlsDiscontinuityTags),
    _hgsTsFileMode :: !(Maybe HlsTsFileMode),
    _hgsMinSegmentLength :: !(Maybe Nat),
    _hgsIFrameOnlyPlaylists ::
      !(Maybe IFrameOnlyPlaylistType),
    _hgsProgramDateTime :: !(Maybe HlsProgramDateTime),
    _hgsIndexNSegments :: !(Maybe Nat),
    _hgsProgramDateTimePeriod :: !(Maybe Nat),
    _hgsCodecSpecification :: !(Maybe HlsCodecSpecification),
    _hgsHlsCdnSettings :: !(Maybe HlsCdnSettings),
    _hgsCaptionLanguageMappings ::
      !(Maybe [CaptionLanguageMapping]),
    _hgsInputLossAction :: !(Maybe InputLossActionForHlsOut),
    _hgsMode :: !(Maybe HlsMode),
    _hgsKeyProviderSettings :: !(Maybe KeyProviderSettings),
    _hgsIncompleteSegmentBehavior ::
      !(Maybe HlsIncompleteSegmentBehavior),
    _hgsConstantIv :: !(Maybe Text),
    _hgsBaseURLManifest :: !(Maybe Text),
    _hgsAdMarkers :: !(Maybe [HlsAdMarkers]),
    _hgsKeyFormat :: !(Maybe Text),
    _hgsSegmentLength :: !(Maybe Nat),
    _hgsHlsId3SegmentTagging ::
      !(Maybe HlsId3SegmentTaggingState),
    _hgsTimedMetadataId3Frame ::
      !(Maybe HlsTimedMetadataId3Frame),
    _hgsBaseURLContent :: !(Maybe Text),
    _hgsOutputSelection :: !(Maybe HlsOutputSelection),
    _hgsCaptionLanguageSetting ::
      !(Maybe HlsCaptionLanguageSetting),
    _hgsSegmentsPerSubdirectory :: !(Maybe Nat),
    _hgsManifestDurationFormat ::
      !(Maybe HlsManifestDurationFormat),
    _hgsIvSource :: !(Maybe HlsIvSource),
    _hgsSegmentationMode :: !(Maybe HlsSegmentationMode),
    _hgsKeyFormatVersions :: !(Maybe Text),
    _hgsClientCache :: !(Maybe HlsClientCache),
    _hgsTimestampDeltaMilliseconds :: !(Maybe Nat),
    _hgsBaseURLManifest1 :: !(Maybe Text),
    _hgsRedundantManifest :: !(Maybe HlsRedundantManifest),
    _hgsStreamInfResolution ::
      !(Maybe HlsStreamInfResolution),
    _hgsKeepSegments :: !(Maybe Nat),
    _hgsBaseURLContent1 :: !(Maybe Text),
    _hgsManifestCompression ::
      !(Maybe HlsManifestCompression),
    _hgsDestination :: !OutputLocationRef
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsDirectoryStructure' - Place segments in subdirectories.
--
-- * 'hgsEncryptionType' - Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
--
-- * 'hgsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'hgsIvInManifest' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
--
-- * 'hgsDiscontinuityTags' - Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group. Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose. Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
--
-- * 'hgsTsFileMode' - SEGMENTED_FILES: Emit the program as segments - multiple .ts media files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
--
-- * 'hgsMinSegmentLength' - When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- * 'hgsIFrameOnlyPlaylists' - DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field). STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
--
-- * 'hgsProgramDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
--
-- * 'hgsIndexNSegments' - Applies only if Mode field is LIVE. Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
--
-- * 'hgsProgramDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- * 'hgsCodecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- * 'hgsHlsCdnSettings' - Parameters that control interactions with the CDN.
--
-- * 'hgsCaptionLanguageMappings' - Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
--
-- * 'hgsInputLossAction' - Parameter that control output group behavior on input loss.
--
-- * 'hgsMode' - If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
--
-- * 'hgsKeyProviderSettings' - The key provider settings.
--
-- * 'hgsIncompleteSegmentBehavior' - Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline. Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups. Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
--
-- * 'hgsConstantIv' - For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
--
-- * 'hgsBaseURLManifest' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsAdMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
--
-- * 'hgsKeyFormat' - The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
--
-- * 'hgsSegmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- * 'hgsHlsId3SegmentTagging' - State of HLS ID3 Segment Tagging
--
-- * 'hgsTimedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- * 'hgsBaseURLContent' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsOutputSelection' - MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group. VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest. SEGMENTS_ONLY: Does not generate any manifests for this output group.
--
-- * 'hgsCaptionLanguageSetting' - Applies only to 608 Embedded output captions. insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. none: Include CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- * 'hgsSegmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
--
-- * 'hgsManifestDurationFormat' - Indicates whether the output manifest should use floating point or integer values for segment duration.
--
-- * 'hgsIvSource' - For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
--
-- * 'hgsSegmentationMode' - useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- * 'hgsKeyFormatVersions' - Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- * 'hgsClientCache' - When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
--
-- * 'hgsTimestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- * 'hgsBaseURLManifest1' - Optional. One value per output group. Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
--
-- * 'hgsRedundantManifest' - ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines. DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only. For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
--
-- * 'hgsStreamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- * 'hgsKeepSegments' - Applies only if Mode field is LIVE. Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1). If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
--
-- * 'hgsBaseURLContent1' - Optional. One value per output group. This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
--
-- * 'hgsManifestCompression' - When set to gzip, compresses HLS playlist.
--
-- * 'hgsDestination' - A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
hlsGroupSettings ::
  -- | 'hgsDestination'
  OutputLocationRef ->
  HlsGroupSettings
hlsGroupSettings pDestination_ =
  HlsGroupSettings'
    { _hgsDirectoryStructure = Nothing,
      _hgsEncryptionType = Nothing,
      _hgsTimedMetadataId3Period = Nothing,
      _hgsIvInManifest = Nothing,
      _hgsDiscontinuityTags = Nothing,
      _hgsTsFileMode = Nothing,
      _hgsMinSegmentLength = Nothing,
      _hgsIFrameOnlyPlaylists = Nothing,
      _hgsProgramDateTime = Nothing,
      _hgsIndexNSegments = Nothing,
      _hgsProgramDateTimePeriod = Nothing,
      _hgsCodecSpecification = Nothing,
      _hgsHlsCdnSettings = Nothing,
      _hgsCaptionLanguageMappings = Nothing,
      _hgsInputLossAction = Nothing,
      _hgsMode = Nothing,
      _hgsKeyProviderSettings = Nothing,
      _hgsIncompleteSegmentBehavior = Nothing,
      _hgsConstantIv = Nothing,
      _hgsBaseURLManifest = Nothing,
      _hgsAdMarkers = Nothing,
      _hgsKeyFormat = Nothing,
      _hgsSegmentLength = Nothing,
      _hgsHlsId3SegmentTagging = Nothing,
      _hgsTimedMetadataId3Frame = Nothing,
      _hgsBaseURLContent = Nothing,
      _hgsOutputSelection = Nothing,
      _hgsCaptionLanguageSetting = Nothing,
      _hgsSegmentsPerSubdirectory = Nothing,
      _hgsManifestDurationFormat = Nothing,
      _hgsIvSource = Nothing,
      _hgsSegmentationMode = Nothing,
      _hgsKeyFormatVersions = Nothing,
      _hgsClientCache = Nothing,
      _hgsTimestampDeltaMilliseconds = Nothing,
      _hgsBaseURLManifest1 = Nothing,
      _hgsRedundantManifest = Nothing,
      _hgsStreamInfResolution = Nothing,
      _hgsKeepSegments = Nothing,
      _hgsBaseURLContent1 = Nothing,
      _hgsManifestCompression = Nothing,
      _hgsDestination = pDestination_
    }

-- | Place segments in subdirectories.
hgsDirectoryStructure :: Lens' HlsGroupSettings (Maybe HlsDirectoryStructure)
hgsDirectoryStructure = lens _hgsDirectoryStructure (\s a -> s {_hgsDirectoryStructure = a})

-- | Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
hgsEncryptionType :: Lens' HlsGroupSettings (Maybe HlsEncryptionType)
hgsEncryptionType = lens _hgsEncryptionType (\s a -> s {_hgsEncryptionType = a})

-- | Timed Metadata interval in seconds.
hgsTimedMetadataId3Period :: Lens' HlsGroupSettings (Maybe Natural)
hgsTimedMetadataId3Period = lens _hgsTimedMetadataId3Period (\s a -> s {_hgsTimedMetadataId3Period = a}) . mapping _Nat

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
hgsIvInManifest :: Lens' HlsGroupSettings (Maybe HlsIvInManifest)
hgsIvInManifest = lens _hgsIvInManifest (\s a -> s {_hgsIvInManifest = a})

-- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group. Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose. Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
hgsDiscontinuityTags :: Lens' HlsGroupSettings (Maybe HlsDiscontinuityTags)
hgsDiscontinuityTags = lens _hgsDiscontinuityTags (\s a -> s {_hgsDiscontinuityTags = a})

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media files. SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
hgsTsFileMode :: Lens' HlsGroupSettings (Maybe HlsTsFileMode)
hgsTsFileMode = lens _hgsTsFileMode (\s a -> s {_hgsTsFileMode = a})

-- | When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
hgsMinSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsMinSegmentLength = lens _hgsMinSegmentLength (\s a -> s {_hgsMinSegmentLength = a}) . mapping _Nat

-- | DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field). STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
hgsIFrameOnlyPlaylists :: Lens' HlsGroupSettings (Maybe IFrameOnlyPlaylistType)
hgsIFrameOnlyPlaylists = lens _hgsIFrameOnlyPlaylists (\s a -> s {_hgsIFrameOnlyPlaylists = a})

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
hgsProgramDateTime :: Lens' HlsGroupSettings (Maybe HlsProgramDateTime)
hgsProgramDateTime = lens _hgsProgramDateTime (\s a -> s {_hgsProgramDateTime = a})

-- | Applies only if Mode field is LIVE. Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
hgsIndexNSegments :: Lens' HlsGroupSettings (Maybe Natural)
hgsIndexNSegments = lens _hgsIndexNSegments (\s a -> s {_hgsIndexNSegments = a}) . mapping _Nat

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hgsProgramDateTimePeriod :: Lens' HlsGroupSettings (Maybe Natural)
hgsProgramDateTimePeriod = lens _hgsProgramDateTimePeriod (\s a -> s {_hgsProgramDateTimePeriod = a}) . mapping _Nat

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
hgsCodecSpecification :: Lens' HlsGroupSettings (Maybe HlsCodecSpecification)
hgsCodecSpecification = lens _hgsCodecSpecification (\s a -> s {_hgsCodecSpecification = a})

-- | Parameters that control interactions with the CDN.
hgsHlsCdnSettings :: Lens' HlsGroupSettings (Maybe HlsCdnSettings)
hgsHlsCdnSettings = lens _hgsHlsCdnSettings (\s a -> s {_hgsHlsCdnSettings = a})

-- | Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
hgsCaptionLanguageMappings :: Lens' HlsGroupSettings [CaptionLanguageMapping]
hgsCaptionLanguageMappings = lens _hgsCaptionLanguageMappings (\s a -> s {_hgsCaptionLanguageMappings = a}) . _Default . _Coerce

-- | Parameter that control output group behavior on input loss.
hgsInputLossAction :: Lens' HlsGroupSettings (Maybe InputLossActionForHlsOut)
hgsInputLossAction = lens _hgsInputLossAction (\s a -> s {_hgsInputLossAction = a})

-- | If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event. VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
hgsMode :: Lens' HlsGroupSettings (Maybe HlsMode)
hgsMode = lens _hgsMode (\s a -> s {_hgsMode = a})

-- | The key provider settings.
hgsKeyProviderSettings :: Lens' HlsGroupSettings (Maybe KeyProviderSettings)
hgsKeyProviderSettings = lens _hgsKeyProviderSettings (\s a -> s {_hgsKeyProviderSettings = a})

-- | Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline. Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups. Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
hgsIncompleteSegmentBehavior :: Lens' HlsGroupSettings (Maybe HlsIncompleteSegmentBehavior)
hgsIncompleteSegmentBehavior = lens _hgsIncompleteSegmentBehavior (\s a -> s {_hgsIncompleteSegmentBehavior = a})

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
hgsConstantIv :: Lens' HlsGroupSettings (Maybe Text)
hgsConstantIv = lens _hgsConstantIv (\s a -> s {_hgsConstantIv = a})

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURLManifest :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLManifest = lens _hgsBaseURLManifest (\s a -> s {_hgsBaseURLManifest = a})

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
hgsAdMarkers :: Lens' HlsGroupSettings [HlsAdMarkers]
hgsAdMarkers = lens _hgsAdMarkers (\s a -> s {_hgsAdMarkers = a}) . _Default . _Coerce

-- | The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
hgsKeyFormat :: Lens' HlsGroupSettings (Maybe Text)
hgsKeyFormat = lens _hgsKeyFormat (\s a -> s {_hgsKeyFormat = a})

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
hgsSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentLength = lens _hgsSegmentLength (\s a -> s {_hgsSegmentLength = a}) . mapping _Nat

-- | State of HLS ID3 Segment Tagging
hgsHlsId3SegmentTagging :: Lens' HlsGroupSettings (Maybe HlsId3SegmentTaggingState)
hgsHlsId3SegmentTagging = lens _hgsHlsId3SegmentTagging (\s a -> s {_hgsHlsId3SegmentTagging = a})

-- | Indicates ID3 frame that has the timecode.
hgsTimedMetadataId3Frame :: Lens' HlsGroupSettings (Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = lens _hgsTimedMetadataId3Frame (\s a -> s {_hgsTimedMetadataId3Frame = a})

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURLContent :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLContent = lens _hgsBaseURLContent (\s a -> s {_hgsBaseURLContent = a})

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group. VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest. SEGMENTS_ONLY: Does not generate any manifests for this output group.
hgsOutputSelection :: Lens' HlsGroupSettings (Maybe HlsOutputSelection)
hgsOutputSelection = lens _hgsOutputSelection (\s a -> s {_hgsOutputSelection = a})

-- | Applies only to 608 Embedded output captions. insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. none: Include CLOSED-CAPTIONS=NONE line in the manifest. omit: Omit any CLOSED-CAPTIONS line from the manifest.
hgsCaptionLanguageSetting :: Lens' HlsGroupSettings (Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = lens _hgsCaptionLanguageSetting (\s a -> s {_hgsCaptionLanguageSetting = a})

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
hgsSegmentsPerSubdirectory :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentsPerSubdirectory = lens _hgsSegmentsPerSubdirectory (\s a -> s {_hgsSegmentsPerSubdirectory = a}) . mapping _Nat

-- | Indicates whether the output manifest should use floating point or integer values for segment duration.
hgsManifestDurationFormat :: Lens' HlsGroupSettings (Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = lens _hgsManifestDurationFormat (\s a -> s {_hgsManifestDurationFormat = a})

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
hgsIvSource :: Lens' HlsGroupSettings (Maybe HlsIvSource)
hgsIvSource = lens _hgsIvSource (\s a -> s {_hgsIvSource = a})

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
hgsSegmentationMode :: Lens' HlsGroupSettings (Maybe HlsSegmentationMode)
hgsSegmentationMode = lens _hgsSegmentationMode (\s a -> s {_hgsSegmentationMode = a})

-- | Either a single positive integer version value or a slash delimited list of version values (1/2/3).
hgsKeyFormatVersions :: Lens' HlsGroupSettings (Maybe Text)
hgsKeyFormatVersions = lens _hgsKeyFormatVersions (\s a -> s {_hgsKeyFormatVersions = a})

-- | When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
hgsClientCache :: Lens' HlsGroupSettings (Maybe HlsClientCache)
hgsClientCache = lens _hgsClientCache (\s a -> s {_hgsClientCache = a})

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hgsTimestampDeltaMilliseconds :: Lens' HlsGroupSettings (Maybe Natural)
hgsTimestampDeltaMilliseconds = lens _hgsTimestampDeltaMilliseconds (\s a -> s {_hgsTimestampDeltaMilliseconds = a}) . mapping _Nat

-- | Optional. One value per output group. Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
hgsBaseURLManifest1 :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLManifest1 = lens _hgsBaseURLManifest1 (\s a -> s {_hgsBaseURLManifest1 = a})

-- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines. DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only. For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
hgsRedundantManifest :: Lens' HlsGroupSettings (Maybe HlsRedundantManifest)
hgsRedundantManifest = lens _hgsRedundantManifest (\s a -> s {_hgsRedundantManifest = a})

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
hgsStreamInfResolution :: Lens' HlsGroupSettings (Maybe HlsStreamInfResolution)
hgsStreamInfResolution = lens _hgsStreamInfResolution (\s a -> s {_hgsStreamInfResolution = a})

-- | Applies only if Mode field is LIVE. Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1). If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
hgsKeepSegments :: Lens' HlsGroupSettings (Maybe Natural)
hgsKeepSegments = lens _hgsKeepSegments (\s a -> s {_hgsKeepSegments = a}) . mapping _Nat

-- | Optional. One value per output group. This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
hgsBaseURLContent1 :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURLContent1 = lens _hgsBaseURLContent1 (\s a -> s {_hgsBaseURLContent1 = a})

-- | When set to gzip, compresses HLS playlist.
hgsManifestCompression :: Lens' HlsGroupSettings (Maybe HlsManifestCompression)
hgsManifestCompression = lens _hgsManifestCompression (\s a -> s {_hgsManifestCompression = a})

-- | A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
hgsDestination :: Lens' HlsGroupSettings OutputLocationRef
hgsDestination = lens _hgsDestination (\s a -> s {_hgsDestination = a})

instance FromJSON HlsGroupSettings where
  parseJSON =
    withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            <$> (x .:? "directoryStructure")
            <*> (x .:? "encryptionType")
            <*> (x .:? "timedMetadataId3Period")
            <*> (x .:? "ivInManifest")
            <*> (x .:? "discontinuityTags")
            <*> (x .:? "tsFileMode")
            <*> (x .:? "minSegmentLength")
            <*> (x .:? "iFrameOnlyPlaylists")
            <*> (x .:? "programDateTime")
            <*> (x .:? "indexNSegments")
            <*> (x .:? "programDateTimePeriod")
            <*> (x .:? "codecSpecification")
            <*> (x .:? "hlsCdnSettings")
            <*> (x .:? "captionLanguageMappings" .!= mempty)
            <*> (x .:? "inputLossAction")
            <*> (x .:? "mode")
            <*> (x .:? "keyProviderSettings")
            <*> (x .:? "incompleteSegmentBehavior")
            <*> (x .:? "constantIv")
            <*> (x .:? "baseUrlManifest")
            <*> (x .:? "adMarkers" .!= mempty)
            <*> (x .:? "keyFormat")
            <*> (x .:? "segmentLength")
            <*> (x .:? "hlsId3SegmentTagging")
            <*> (x .:? "timedMetadataId3Frame")
            <*> (x .:? "baseUrlContent")
            <*> (x .:? "outputSelection")
            <*> (x .:? "captionLanguageSetting")
            <*> (x .:? "segmentsPerSubdirectory")
            <*> (x .:? "manifestDurationFormat")
            <*> (x .:? "ivSource")
            <*> (x .:? "segmentationMode")
            <*> (x .:? "keyFormatVersions")
            <*> (x .:? "clientCache")
            <*> (x .:? "timestampDeltaMilliseconds")
            <*> (x .:? "baseUrlManifest1")
            <*> (x .:? "redundantManifest")
            <*> (x .:? "streamInfResolution")
            <*> (x .:? "keepSegments")
            <*> (x .:? "baseUrlContent1")
            <*> (x .:? "manifestCompression")
            <*> (x .: "destination")
      )

instance Hashable HlsGroupSettings

instance NFData HlsGroupSettings

instance ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    object
      ( catMaybes
          [ ("directoryStructure" .=) <$> _hgsDirectoryStructure,
            ("encryptionType" .=) <$> _hgsEncryptionType,
            ("timedMetadataId3Period" .=) <$> _hgsTimedMetadataId3Period,
            ("ivInManifest" .=) <$> _hgsIvInManifest,
            ("discontinuityTags" .=) <$> _hgsDiscontinuityTags,
            ("tsFileMode" .=) <$> _hgsTsFileMode,
            ("minSegmentLength" .=) <$> _hgsMinSegmentLength,
            ("iFrameOnlyPlaylists" .=) <$> _hgsIFrameOnlyPlaylists,
            ("programDateTime" .=) <$> _hgsProgramDateTime,
            ("indexNSegments" .=) <$> _hgsIndexNSegments,
            ("programDateTimePeriod" .=) <$> _hgsProgramDateTimePeriod,
            ("codecSpecification" .=) <$> _hgsCodecSpecification,
            ("hlsCdnSettings" .=) <$> _hgsHlsCdnSettings,
            ("captionLanguageMappings" .=) <$> _hgsCaptionLanguageMappings,
            ("inputLossAction" .=) <$> _hgsInputLossAction,
            ("mode" .=) <$> _hgsMode,
            ("keyProviderSettings" .=) <$> _hgsKeyProviderSettings,
            ("incompleteSegmentBehavior" .=) <$> _hgsIncompleteSegmentBehavior,
            ("constantIv" .=) <$> _hgsConstantIv,
            ("baseUrlManifest" .=) <$> _hgsBaseURLManifest,
            ("adMarkers" .=) <$> _hgsAdMarkers,
            ("keyFormat" .=) <$> _hgsKeyFormat,
            ("segmentLength" .=) <$> _hgsSegmentLength,
            ("hlsId3SegmentTagging" .=) <$> _hgsHlsId3SegmentTagging,
            ("timedMetadataId3Frame" .=) <$> _hgsTimedMetadataId3Frame,
            ("baseUrlContent" .=) <$> _hgsBaseURLContent,
            ("outputSelection" .=) <$> _hgsOutputSelection,
            ("captionLanguageSetting" .=) <$> _hgsCaptionLanguageSetting,
            ("segmentsPerSubdirectory" .=) <$> _hgsSegmentsPerSubdirectory,
            ("manifestDurationFormat" .=) <$> _hgsManifestDurationFormat,
            ("ivSource" .=) <$> _hgsIvSource,
            ("segmentationMode" .=) <$> _hgsSegmentationMode,
            ("keyFormatVersions" .=) <$> _hgsKeyFormatVersions,
            ("clientCache" .=) <$> _hgsClientCache,
            ("timestampDeltaMilliseconds" .=)
              <$> _hgsTimestampDeltaMilliseconds,
            ("baseUrlManifest1" .=) <$> _hgsBaseURLManifest1,
            ("redundantManifest" .=) <$> _hgsRedundantManifest,
            ("streamInfResolution" .=) <$> _hgsStreamInfResolution,
            ("keepSegments" .=) <$> _hgsKeepSegments,
            ("baseUrlContent1" .=) <$> _hgsBaseURLContent1,
            ("manifestCompression" .=) <$> _hgsManifestCompression,
            Just ("destination" .= _hgsDestination)
          ]
      )

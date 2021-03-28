{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsGroupSettings
  ( HlsGroupSettings (..)
  -- * Smart constructor
  , mkHlsGroupSettings
  -- * Lenses
  , hgsDestination
  , hgsAdMarkers
  , hgsBaseUrlContent
  , hgsBaseUrlContent1
  , hgsBaseUrlManifest
  , hgsBaseUrlManifest1
  , hgsCaptionLanguageMappings
  , hgsCaptionLanguageSetting
  , hgsClientCache
  , hgsCodecSpecification
  , hgsConstantIv
  , hgsDirectoryStructure
  , hgsDiscontinuityTags
  , hgsEncryptionType
  , hgsHlsCdnSettings
  , hgsHlsId3SegmentTagging
  , hgsIFrameOnlyPlaylists
  , hgsIncompleteSegmentBehavior
  , hgsIndexNSegments
  , hgsInputLossAction
  , hgsIvInManifest
  , hgsIvSource
  , hgsKeepSegments
  , hgsKeyFormat
  , hgsKeyFormatVersions
  , hgsKeyProviderSettings
  , hgsManifestCompression
  , hgsManifestDurationFormat
  , hgsMinSegmentLength
  , hgsMode
  , hgsOutputSelection
  , hgsProgramDateTime
  , hgsProgramDateTimePeriod
  , hgsRedundantManifest
  , hgsSegmentLength
  , hgsSegmentationMode
  , hgsSegmentsPerSubdirectory
  , hgsStreamInfResolution
  , hgsTimedMetadataId3Frame
  , hgsTimedMetadataId3Period
  , hgsTimestampDeltaMilliseconds
  , hgsTsFileMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.CaptionLanguageMapping as Types
import qualified Network.AWS.MediaLive.Types.HlsAdMarkers as Types
import qualified Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting as Types
import qualified Network.AWS.MediaLive.Types.HlsCdnSettings as Types
import qualified Network.AWS.MediaLive.Types.HlsClientCache as Types
import qualified Network.AWS.MediaLive.Types.HlsCodecSpecification as Types
import qualified Network.AWS.MediaLive.Types.HlsDirectoryStructure as Types
import qualified Network.AWS.MediaLive.Types.HlsDiscontinuityTags as Types
import qualified Network.AWS.MediaLive.Types.HlsEncryptionType as Types
import qualified Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState as Types
import qualified Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior as Types
import qualified Network.AWS.MediaLive.Types.HlsIvInManifest as Types
import qualified Network.AWS.MediaLive.Types.HlsIvSource as Types
import qualified Network.AWS.MediaLive.Types.HlsManifestCompression as Types
import qualified Network.AWS.MediaLive.Types.HlsManifestDurationFormat as Types
import qualified Network.AWS.MediaLive.Types.HlsMode as Types
import qualified Network.AWS.MediaLive.Types.HlsOutputSelection as Types
import qualified Network.AWS.MediaLive.Types.HlsProgramDateTime as Types
import qualified Network.AWS.MediaLive.Types.HlsRedundantManifest as Types
import qualified Network.AWS.MediaLive.Types.HlsSegmentationMode as Types
import qualified Network.AWS.MediaLive.Types.HlsStreamInfResolution as Types
import qualified Network.AWS.MediaLive.Types.HlsTimedMetadataId3Frame as Types
import qualified Network.AWS.MediaLive.Types.HlsTsFileMode as Types
import qualified Network.AWS.MediaLive.Types.IFrameOnlyPlaylistType as Types
import qualified Network.AWS.MediaLive.Types.InputLossActionForHlsOut as Types
import qualified Network.AWS.MediaLive.Types.KeyProviderSettings as Types
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Group Settings
--
-- /See:/ 'mkHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { destination :: Types.OutputLocationRef
    -- ^ A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
  , adMarkers :: Core.Maybe [Types.HlsAdMarkers]
    -- ^ Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
  , baseUrlContent :: Core.Maybe Core.Text
    -- ^ A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
  , baseUrlContent1 :: Core.Maybe Core.Text
    -- ^ Optional. One value per output group.
--
--
-- This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
  , baseUrlManifest :: Core.Maybe Core.Text
    -- ^ A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
  , baseUrlManifest1 :: Core.Maybe Core.Text
    -- ^ Optional. One value per output group.
--
--
-- Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
  , captionLanguageMappings :: Core.Maybe [Types.CaptionLanguageMapping]
    -- ^ Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
  , captionLanguageSetting :: Core.Maybe Types.HlsCaptionLanguageSetting
    -- ^ Applies only to 608 Embedded output captions.
--
-- insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions.
-- none: Include CLOSED-CAPTIONS=NONE line in the manifest.
-- omit: Omit any CLOSED-CAPTIONS line from the manifest.
  , clientCache :: Core.Maybe Types.HlsClientCache
    -- ^ When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
  , codecSpecification :: Core.Maybe Types.HlsCodecSpecification
    -- ^ Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
  , constantIv :: Core.Maybe Core.Text
    -- ^ For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
  , directoryStructure :: Core.Maybe Types.HlsDirectoryStructure
    -- ^ Place segments in subdirectories.
  , discontinuityTags :: Core.Maybe Types.HlsDiscontinuityTags
    -- ^ Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group.
--
-- Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose.
-- Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
  , encryptionType :: Core.Maybe Types.HlsEncryptionType
    -- ^ Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
  , hlsCdnSettings :: Core.Maybe Types.HlsCdnSettings
    -- ^ Parameters that control interactions with the CDN.
  , hlsId3SegmentTagging :: Core.Maybe Types.HlsId3SegmentTaggingState
    -- ^ State of HLS ID3 Segment Tagging
  , iFrameOnlyPlaylists :: Core.Maybe Types.IFrameOnlyPlaylistType
    -- ^ DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field).
--
--
-- STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
  , incompleteSegmentBehavior :: Core.Maybe Types.HlsIncompleteSegmentBehavior
    -- ^ Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline.
--
-- Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups.
-- Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
  , indexNSegments :: Core.Maybe Core.Natural
    -- ^ Applies only if Mode field is LIVE.
--
--
-- Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
  , inputLossAction :: Core.Maybe Types.InputLossActionForHlsOut
    -- ^ Parameter that control output group behavior on input loss.
  , ivInManifest :: Core.Maybe Types.HlsIvInManifest
    -- ^ For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
  , ivSource :: Core.Maybe Types.HlsIvSource
    -- ^ For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
  , keepSegments :: Core.Maybe Core.Natural
    -- ^ Applies only if Mode field is LIVE.
--
--
-- Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1).
--
-- If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
  , keyFormat :: Core.Maybe Core.Text
    -- ^ The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
  , keyFormatVersions :: Core.Maybe Core.Text
    -- ^ Either a single positive integer version value or a slash delimited list of version values (1/2/3).
  , keyProviderSettings :: Core.Maybe Types.KeyProviderSettings
    -- ^ The key provider settings.
  , manifestCompression :: Core.Maybe Types.HlsManifestCompression
    -- ^ When set to gzip, compresses HLS playlist.
  , manifestDurationFormat :: Core.Maybe Types.HlsManifestDurationFormat
    -- ^ Indicates whether the output manifest should use floating point or integer values for segment duration.
  , minSegmentLength :: Core.Maybe Core.Natural
    -- ^ When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
  , mode :: Core.Maybe Types.HlsMode
    -- ^ If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event.
--
--
-- VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
  , outputSelection :: Core.Maybe Types.HlsOutputSelection
    -- ^ MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group.
--
--
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest.
--
-- SEGMENTS_ONLY: Does not generate any manifests for this output group.
  , programDateTime :: Core.Maybe Types.HlsProgramDateTime
    -- ^ Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
  , programDateTimePeriod :: Core.Maybe Core.Natural
    -- ^ Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
  , redundantManifest :: Core.Maybe Types.HlsRedundantManifest
    -- ^ ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines.
--
--
-- DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only.
--
-- For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
  , segmentLength :: Core.Maybe Core.Natural
    -- ^ Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
  , segmentationMode :: Core.Maybe Types.HlsSegmentationMode
    -- ^ useInputSegmentation has been deprecated. The configured segment size is always used.
  , segmentsPerSubdirectory :: Core.Maybe Core.Natural
    -- ^ Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
  , streamInfResolution :: Core.Maybe Types.HlsStreamInfResolution
    -- ^ Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
  , timedMetadataId3Frame :: Core.Maybe Types.HlsTimedMetadataId3Frame
    -- ^ Indicates ID3 frame that has the timecode.
  , timedMetadataId3Period :: Core.Maybe Core.Natural
    -- ^ Timed Metadata interval in seconds.
  , timestampDeltaMilliseconds :: Core.Maybe Core.Natural
    -- ^ Provides an extra millisecond delta offset to fine tune the timestamps.
  , tsFileMode :: Core.Maybe Types.HlsTsFileMode
    -- ^ SEGMENTED_FILES: Emit the program as segments - multiple .ts media files.
--
--
-- SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsGroupSettings' value with any optional fields omitted.
mkHlsGroupSettings
    :: Types.OutputLocationRef -- ^ 'destination'
    -> HlsGroupSettings
mkHlsGroupSettings destination
  = HlsGroupSettings'{destination, adMarkers = Core.Nothing,
                      baseUrlContent = Core.Nothing, baseUrlContent1 = Core.Nothing,
                      baseUrlManifest = Core.Nothing, baseUrlManifest1 = Core.Nothing,
                      captionLanguageMappings = Core.Nothing,
                      captionLanguageSetting = Core.Nothing, clientCache = Core.Nothing,
                      codecSpecification = Core.Nothing, constantIv = Core.Nothing,
                      directoryStructure = Core.Nothing,
                      discontinuityTags = Core.Nothing, encryptionType = Core.Nothing,
                      hlsCdnSettings = Core.Nothing, hlsId3SegmentTagging = Core.Nothing,
                      iFrameOnlyPlaylists = Core.Nothing,
                      incompleteSegmentBehavior = Core.Nothing,
                      indexNSegments = Core.Nothing, inputLossAction = Core.Nothing,
                      ivInManifest = Core.Nothing, ivSource = Core.Nothing,
                      keepSegments = Core.Nothing, keyFormat = Core.Nothing,
                      keyFormatVersions = Core.Nothing,
                      keyProviderSettings = Core.Nothing,
                      manifestCompression = Core.Nothing,
                      manifestDurationFormat = Core.Nothing,
                      minSegmentLength = Core.Nothing, mode = Core.Nothing,
                      outputSelection = Core.Nothing, programDateTime = Core.Nothing,
                      programDateTimePeriod = Core.Nothing,
                      redundantManifest = Core.Nothing, segmentLength = Core.Nothing,
                      segmentationMode = Core.Nothing,
                      segmentsPerSubdirectory = Core.Nothing,
                      streamInfResolution = Core.Nothing,
                      timedMetadataId3Frame = Core.Nothing,
                      timedMetadataId3Period = Core.Nothing,
                      timestampDeltaMilliseconds = Core.Nothing,
                      tsFileMode = Core.Nothing}

-- | A directory or HTTP destination for the HLS segments, manifest files, and encryption keys (if enabled).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestination :: Lens.Lens' HlsGroupSettings Types.OutputLocationRef
hgsDestination = Lens.field @"destination"
{-# INLINEABLE hgsDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdMarkers :: Lens.Lens' HlsGroupSettings (Core.Maybe [Types.HlsAdMarkers])
hgsAdMarkers = Lens.field @"adMarkers"
{-# INLINEABLE hgsAdMarkers #-}
{-# DEPRECATED adMarkers "Use generic-lens or generic-optics with 'adMarkers' instead"  #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseUrlContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseUrlContent :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsBaseUrlContent = Lens.field @"baseUrlContent"
{-# INLINEABLE hgsBaseUrlContent #-}
{-# DEPRECATED baseUrlContent "Use generic-lens or generic-optics with 'baseUrlContent' instead"  #-}

-- | Optional. One value per output group.
--
--
-- This field is required only if you are completing Base URL content A, and the downstream system has notified you that the media files for pipeline 1 of all outputs are in a location different from the media files for pipeline 0.
--
-- /Note:/ Consider using 'baseUrlContent1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseUrlContent1 :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsBaseUrlContent1 = Lens.field @"baseUrlContent1"
{-# INLINEABLE hgsBaseUrlContent1 #-}
{-# DEPRECATED baseUrlContent1 "Use generic-lens or generic-optics with 'baseUrlContent1' instead"  #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseUrlManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseUrlManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsBaseUrlManifest = Lens.field @"baseUrlManifest"
{-# INLINEABLE hgsBaseUrlManifest #-}
{-# DEPRECATED baseUrlManifest "Use generic-lens or generic-optics with 'baseUrlManifest' instead"  #-}

-- | Optional. One value per output group.
--
--
-- Complete this field only if you are completing Base URL manifest A, and the downstream system has notified you that the child manifest files for pipeline 1 of all outputs are in a location different from the child manifest files for pipeline 0.
--
-- /Note:/ Consider using 'baseUrlManifest1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseUrlManifest1 :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsBaseUrlManifest1 = Lens.field @"baseUrlManifest1"
{-# INLINEABLE hgsBaseUrlManifest1 #-}
{-# DEPRECATED baseUrlManifest1 "Use generic-lens or generic-optics with 'baseUrlManifest1' instead"  #-}

-- | Mapping of up to 4 caption channels to caption languages.  Is only meaningful if captionLanguageSetting is set to "insert".
--
-- /Note:/ Consider using 'captionLanguageMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageMappings :: Lens.Lens' HlsGroupSettings (Core.Maybe [Types.CaptionLanguageMapping])
hgsCaptionLanguageMappings = Lens.field @"captionLanguageMappings"
{-# INLINEABLE hgsCaptionLanguageMappings #-}
{-# DEPRECATED captionLanguageMappings "Use generic-lens or generic-optics with 'captionLanguageMappings' instead"  #-}

-- | Applies only to 608 Embedded output captions.
--
-- insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions.
-- none: Include CLOSED-CAPTIONS=NONE line in the manifest.
-- omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- /Note:/ Consider using 'captionLanguageSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageSetting :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = Lens.field @"captionLanguageSetting"
{-# INLINEABLE hgsCaptionLanguageSetting #-}
{-# DEPRECATED captionLanguageSetting "Use generic-lens or generic-optics with 'captionLanguageSetting' instead"  #-}

-- | When set to "disabled", sets the #EXT-X-ALLOW-CACHE:no tag in the manifest, which prevents clients from saving media segments for later replay.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsClientCache :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsClientCache)
hgsClientCache = Lens.field @"clientCache"
{-# INLINEABLE hgsClientCache #-}
{-# DEPRECATED clientCache "Use generic-lens or generic-optics with 'clientCache' instead"  #-}

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- /Note:/ Consider using 'codecSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCodecSpecification :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsCodecSpecification)
hgsCodecSpecification = Lens.field @"codecSpecification"
{-# INLINEABLE hgsCodecSpecification #-}
{-# DEPRECATED codecSpecification "Use generic-lens or generic-optics with 'codecSpecification' instead"  #-}

-- | For use with encryptionType. This is a 128-bit, 16-byte hex value represented by a 32-character text string. If ivSource is set to "explicit" then this parameter is required and is used as the IV for encryption.
--
-- /Note:/ Consider using 'constantIv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsConstantIv :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsConstantIv = Lens.field @"constantIv"
{-# INLINEABLE hgsConstantIv #-}
{-# DEPRECATED constantIv "Use generic-lens or generic-optics with 'constantIv' instead"  #-}

-- | Place segments in subdirectories.
--
-- /Note:/ Consider using 'directoryStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDirectoryStructure :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsDirectoryStructure)
hgsDirectoryStructure = Lens.field @"directoryStructure"
{-# INLINEABLE hgsDirectoryStructure #-}
{-# DEPRECATED directoryStructure "Use generic-lens or generic-optics with 'directoryStructure' instead"  #-}

-- | Specifies whether to insert EXT-X-DISCONTINUITY tags in the HLS child manifests for this output group.
--
-- Typically, choose Insert because these tags are required in the manifest (according to the HLS specification) and serve an important purpose.
-- Choose Never Insert only if the downstream system is doing real-time failover (without using the MediaLive automatic failover feature) and only if that downstream system has advised you to exclude the tags.
--
-- /Note:/ Consider using 'discontinuityTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDiscontinuityTags :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsDiscontinuityTags)
hgsDiscontinuityTags = Lens.field @"discontinuityTags"
{-# INLINEABLE hgsDiscontinuityTags #-}
{-# DEPRECATED discontinuityTags "Use generic-lens or generic-optics with 'discontinuityTags' instead"  #-}

-- | Encrypts the segments with the given encryption scheme.  Exclude this parameter if no encryption is desired.
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsEncryptionType :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsEncryptionType)
hgsEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE hgsEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | Parameters that control interactions with the CDN.
--
-- /Note:/ Consider using 'hlsCdnSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsHlsCdnSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsCdnSettings)
hgsHlsCdnSettings = Lens.field @"hlsCdnSettings"
{-# INLINEABLE hgsHlsCdnSettings #-}
{-# DEPRECATED hlsCdnSettings "Use generic-lens or generic-optics with 'hlsCdnSettings' instead"  #-}

-- | State of HLS ID3 Segment Tagging
--
-- /Note:/ Consider using 'hlsId3SegmentTagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsHlsId3SegmentTagging :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsId3SegmentTaggingState)
hgsHlsId3SegmentTagging = Lens.field @"hlsId3SegmentTagging"
{-# INLINEABLE hgsHlsId3SegmentTagging #-}
{-# DEPRECATED hlsId3SegmentTagging "Use generic-lens or generic-optics with 'hlsId3SegmentTagging' instead"  #-}

-- | DISABLED: Do not create an I-frame-only manifest, but do create the master and media manifests (according to the Output Selection field).
--
--
-- STANDARD: Create an I-frame-only manifest for each output that contains video, as well as the other manifests (according to the Output Selection field). The I-frame manifest contains a #EXT-X-I-FRAMES-ONLY tag to indicate it is I-frame only, and one or more #EXT-X-BYTERANGE entries identifying the I-frame position. For example, #EXT-X-BYTERANGE:160364@1461888"
--
-- /Note:/ Consider using 'iFrameOnlyPlaylists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIFrameOnlyPlaylists :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.IFrameOnlyPlaylistType)
hgsIFrameOnlyPlaylists = Lens.field @"iFrameOnlyPlaylists"
{-# INLINEABLE hgsIFrameOnlyPlaylists #-}
{-# DEPRECATED iFrameOnlyPlaylists "Use generic-lens or generic-optics with 'iFrameOnlyPlaylists' instead"  #-}

-- | Specifies whether to include the final (incomplete) segment in the media output when the pipeline stops producing output because of a channel stop, a channel pause or a loss of input to the pipeline.
--
-- Auto means that MediaLive decides whether to include the final segment, depending on the channel class and the types of output groups.
-- Suppress means to never include the incomplete segment. We recommend you choose Auto and let MediaLive control the behavior.
--
-- /Note:/ Consider using 'incompleteSegmentBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIncompleteSegmentBehavior :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsIncompleteSegmentBehavior)
hgsIncompleteSegmentBehavior = Lens.field @"incompleteSegmentBehavior"
{-# INLINEABLE hgsIncompleteSegmentBehavior #-}
{-# DEPRECATED incompleteSegmentBehavior "Use generic-lens or generic-optics with 'incompleteSegmentBehavior' instead"  #-}

-- | Applies only if Mode field is LIVE.
--
--
-- Specifies the maximum number of segments in the media manifest file. After this maximum, older segments are removed from the media manifest. This number must be smaller than the number in the Keep Segments field.
--
-- /Note:/ Consider using 'indexNSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIndexNSegments :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsIndexNSegments = Lens.field @"indexNSegments"
{-# INLINEABLE hgsIndexNSegments #-}
{-# DEPRECATED indexNSegments "Use generic-lens or generic-optics with 'indexNSegments' instead"  #-}

-- | Parameter that control output group behavior on input loss.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsInputLossAction :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.InputLossActionForHlsOut)
hgsInputLossAction = Lens.field @"inputLossAction"
{-# INLINEABLE hgsInputLossAction #-}
{-# DEPRECATED inputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead"  #-}

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If set to "include", IV is listed in the manifest, otherwise the IV is not in the manifest.
--
-- /Note:/ Consider using 'ivInManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIvInManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsIvInManifest)
hgsIvInManifest = Lens.field @"ivInManifest"
{-# INLINEABLE hgsIvInManifest #-}
{-# DEPRECATED ivInManifest "Use generic-lens or generic-optics with 'ivInManifest' instead"  #-}

-- | For use with encryptionType. The IV (Initialization Vector) is a 128-bit number used in conjunction with the key for encrypting blocks. If this setting is "followsSegmentNumber", it will cause the IV to change every segment (to match the segment number). If this is set to "explicit", you must enter a constantIv value.
--
-- /Note:/ Consider using 'ivSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsIvSource :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsIvSource)
hgsIvSource = Lens.field @"ivSource"
{-# INLINEABLE hgsIvSource #-}
{-# DEPRECATED ivSource "Use generic-lens or generic-optics with 'ivSource' instead"  #-}

-- | Applies only if Mode field is LIVE.
--
--
-- Specifies the number of media segments to retain in the destination directory. This number should be bigger than indexNSegments (Num segments). We recommend (value = (2 x indexNsegments) + 1).
--
-- If this "keep segments" number is too low, the following might happen: the player is still reading a media manifest file that lists this segment, but that segment has been removed from the destination directory (as directed by indexNSegments). This situation would result in a 404 HTTP error on the player.
--
-- /Note:/ Consider using 'keepSegments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeepSegments :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsKeepSegments = Lens.field @"keepSegments"
{-# INLINEABLE hgsKeepSegments #-}
{-# DEPRECATED keepSegments "Use generic-lens or generic-optics with 'keepSegments' instead"  #-}

-- | The value specifies how the key is represented in the resource identified by the URI.  If parameter is absent, an implicit value of "identity" is used.  A reverse DNS string can also be given.
--
-- /Note:/ Consider using 'keyFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsKeyFormat = Lens.field @"keyFormat"
{-# INLINEABLE hgsKeyFormat #-}
{-# DEPRECATED keyFormat "Use generic-lens or generic-optics with 'keyFormat' instead"  #-}

-- | Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- /Note:/ Consider using 'keyFormatVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyFormatVersions :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsKeyFormatVersions = Lens.field @"keyFormatVersions"
{-# INLINEABLE hgsKeyFormatVersions #-}
{-# DEPRECATED keyFormatVersions "Use generic-lens or generic-optics with 'keyFormatVersions' instead"  #-}

-- | The key provider settings.
--
-- /Note:/ Consider using 'keyProviderSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsKeyProviderSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.KeyProviderSettings)
hgsKeyProviderSettings = Lens.field @"keyProviderSettings"
{-# INLINEABLE hgsKeyProviderSettings #-}
{-# DEPRECATED keyProviderSettings "Use generic-lens or generic-optics with 'keyProviderSettings' instead"  #-}

-- | When set to gzip, compresses HLS playlist.
--
-- /Note:/ Consider using 'manifestCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestCompression :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsManifestCompression)
hgsManifestCompression = Lens.field @"manifestCompression"
{-# INLINEABLE hgsManifestCompression #-}
{-# DEPRECATED manifestCompression "Use generic-lens or generic-optics with 'manifestCompression' instead"  #-}

-- | Indicates whether the output manifest should use floating point or integer values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestDurationFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsManifestDurationFormat)
hgsManifestDurationFormat = Lens.field @"manifestDurationFormat"
{-# INLINEABLE hgsManifestDurationFormat #-}
{-# DEPRECATED manifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead"  #-}

-- | When set, minimumSegmentLength is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- /Note:/ Consider using 'minSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsMinSegmentLength = Lens.field @"minSegmentLength"
{-# INLINEABLE hgsMinSegmentLength #-}
{-# DEPRECATED minSegmentLength "Use generic-lens or generic-optics with 'minSegmentLength' instead"  #-}

-- | If "vod", all segments are indexed and kept permanently in the destination and manifest. If "live", only the number segments specified in keepSegments and indexNSegments are kept; newer segments replace older segments, which may prevent players from rewinding all the way to the beginning of the event.
--
--
-- VOD mode uses HLS EXT-X-PLAYLIST-TYPE of EVENT while the channel is running, converting it to a "VOD" type manifest on completion of the stream.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMode :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsMode)
hgsMode = Lens.field @"mode"
{-# INLINEABLE hgsMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | MANIFESTS_AND_SEGMENTS: Generates manifests (master manifest, if applicable, and media manifests) for this output group.
--
--
-- VARIANT_MANIFESTS_AND_SEGMENTS: Generates media manifests for this output group, but not a master manifest.
--
-- SEGMENTS_ONLY: Does not generate any manifests for this output group.
--
-- /Note:/ Consider using 'outputSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsOutputSelection :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsOutputSelection)
hgsOutputSelection = Lens.field @"outputSelection"
{-# INLINEABLE hgsOutputSelection #-}
{-# DEPRECATED outputSelection "Use generic-lens or generic-optics with 'outputSelection' instead"  #-}

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestampOffset.
--
-- /Note:/ Consider using 'programDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTime :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsProgramDateTime)
hgsProgramDateTime = Lens.field @"programDateTime"
{-# INLINEABLE hgsProgramDateTime #-}
{-# DEPRECATED programDateTime "Use generic-lens or generic-optics with 'programDateTime' instead"  #-}

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- /Note:/ Consider using 'programDateTimePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTimePeriod :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsProgramDateTimePeriod = Lens.field @"programDateTimePeriod"
{-# INLINEABLE hgsProgramDateTimePeriod #-}
{-# DEPRECATED programDateTimePeriod "Use generic-lens or generic-optics with 'programDateTimePeriod' instead"  #-}

-- | ENABLED: The master manifest (.m3u8 file) for each pipeline includes information about both pipelines: first its own media files, then the media files of the other pipeline. This feature allows playout device that support stale manifest detection to switch from one manifest to the other, when the current manifest seems to be stale. There are still two destinations and two master manifests, but both master manifests reference the media files from both pipelines.
--
--
-- DISABLED: The master manifest (.m3u8 file) for each pipeline includes information about its own pipeline only.
--
-- For an HLS output group with MediaPackage as the destination, the DISABLED behavior is always followed. MediaPackage regenerates the manifests it serves to players so a redundant manifest from MediaLive is irrelevant.
--
-- /Note:/ Consider using 'redundantManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsRedundantManifest :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsRedundantManifest)
hgsRedundantManifest = Lens.field @"redundantManifest"
{-# INLINEABLE hgsRedundantManifest #-}
{-# DEPRECATED redundantManifest "Use generic-lens or generic-optics with 'redundantManifest' instead"  #-}

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsSegmentLength = Lens.field @"segmentLength"
{-# INLINEABLE hgsSegmentLength #-}
{-# DEPRECATED segmentLength "Use generic-lens or generic-optics with 'segmentLength' instead"  #-}

-- | useInputSegmentation has been deprecated. The configured segment size is always used.
--
-- /Note:/ Consider using 'segmentationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentationMode :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsSegmentationMode)
hgsSegmentationMode = Lens.field @"segmentationMode"
{-# INLINEABLE hgsSegmentationMode #-}
{-# DEPRECATED segmentationMode "Use generic-lens or generic-optics with 'segmentationMode' instead"  #-}

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be subdirectoryPerStream for this setting to have an effect.
--
-- /Note:/ Consider using 'segmentsPerSubdirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsSegmentsPerSubdirectory = Lens.field @"segmentsPerSubdirectory"
{-# INLINEABLE hgsSegmentsPerSubdirectory #-}
{-# DEPRECATED segmentsPerSubdirectory "Use generic-lens or generic-optics with 'segmentsPerSubdirectory' instead"  #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsStreamInfResolution :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsStreamInfResolution)
hgsStreamInfResolution = Lens.field @"streamInfResolution"
{-# INLINEABLE hgsStreamInfResolution #-}
{-# DEPRECATED streamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead"  #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = Lens.field @"timedMetadataId3Frame"
{-# INLINEABLE hgsTimedMetadataId3Frame #-}
{-# DEPRECATED timedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead"  #-}

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsTimedMetadataId3Period = Lens.field @"timedMetadataId3Period"
{-# INLINEABLE hgsTimedMetadataId3Period #-}
{-# DEPRECATED timedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead"  #-}

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- /Note:/ Consider using 'timestampDeltaMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsTimestampDeltaMilliseconds = Lens.field @"timestampDeltaMilliseconds"
{-# INLINEABLE hgsTimestampDeltaMilliseconds #-}
{-# DEPRECATED timestampDeltaMilliseconds "Use generic-lens or generic-optics with 'timestampDeltaMilliseconds' instead"  #-}

-- | SEGMENTED_FILES: Emit the program as segments - multiple .ts media files.
--
--
-- SINGLE_FILE: Applies only if Mode field is VOD. Emit the program as a single .ts media file. The media manifest includes #EXT-X-BYTERANGE tags to index segments for playback. A typical use for this value is when sending the output to AWS Elemental MediaConvert, which can accept only a single media file. Playback while the channel is running is not guaranteed due to HTTP server caching.
--
-- /Note:/ Consider using 'tsFileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTsFileMode :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsTsFileMode)
hgsTsFileMode = Lens.field @"tsFileMode"
{-# INLINEABLE hgsTsFileMode #-}
{-# DEPRECATED tsFileMode "Use generic-lens or generic-optics with 'tsFileMode' instead"  #-}

instance Core.FromJSON HlsGroupSettings where
        toJSON HlsGroupSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("destination" Core..= destination),
                  ("adMarkers" Core..=) Core.<$> adMarkers,
                  ("baseUrlContent" Core..=) Core.<$> baseUrlContent,
                  ("baseUrlContent1" Core..=) Core.<$> baseUrlContent1,
                  ("baseUrlManifest" Core..=) Core.<$> baseUrlManifest,
                  ("baseUrlManifest1" Core..=) Core.<$> baseUrlManifest1,
                  ("captionLanguageMappings" Core..=) Core.<$>
                    captionLanguageMappings,
                  ("captionLanguageSetting" Core..=) Core.<$> captionLanguageSetting,
                  ("clientCache" Core..=) Core.<$> clientCache,
                  ("codecSpecification" Core..=) Core.<$> codecSpecification,
                  ("constantIv" Core..=) Core.<$> constantIv,
                  ("directoryStructure" Core..=) Core.<$> directoryStructure,
                  ("discontinuityTags" Core..=) Core.<$> discontinuityTags,
                  ("encryptionType" Core..=) Core.<$> encryptionType,
                  ("hlsCdnSettings" Core..=) Core.<$> hlsCdnSettings,
                  ("hlsId3SegmentTagging" Core..=) Core.<$> hlsId3SegmentTagging,
                  ("iFrameOnlyPlaylists" Core..=) Core.<$> iFrameOnlyPlaylists,
                  ("incompleteSegmentBehavior" Core..=) Core.<$>
                    incompleteSegmentBehavior,
                  ("indexNSegments" Core..=) Core.<$> indexNSegments,
                  ("inputLossAction" Core..=) Core.<$> inputLossAction,
                  ("ivInManifest" Core..=) Core.<$> ivInManifest,
                  ("ivSource" Core..=) Core.<$> ivSource,
                  ("keepSegments" Core..=) Core.<$> keepSegments,
                  ("keyFormat" Core..=) Core.<$> keyFormat,
                  ("keyFormatVersions" Core..=) Core.<$> keyFormatVersions,
                  ("keyProviderSettings" Core..=) Core.<$> keyProviderSettings,
                  ("manifestCompression" Core..=) Core.<$> manifestCompression,
                  ("manifestDurationFormat" Core..=) Core.<$> manifestDurationFormat,
                  ("minSegmentLength" Core..=) Core.<$> minSegmentLength,
                  ("mode" Core..=) Core.<$> mode,
                  ("outputSelection" Core..=) Core.<$> outputSelection,
                  ("programDateTime" Core..=) Core.<$> programDateTime,
                  ("programDateTimePeriod" Core..=) Core.<$> programDateTimePeriod,
                  ("redundantManifest" Core..=) Core.<$> redundantManifest,
                  ("segmentLength" Core..=) Core.<$> segmentLength,
                  ("segmentationMode" Core..=) Core.<$> segmentationMode,
                  ("segmentsPerSubdirectory" Core..=) Core.<$>
                    segmentsPerSubdirectory,
                  ("streamInfResolution" Core..=) Core.<$> streamInfResolution,
                  ("timedMetadataId3Frame" Core..=) Core.<$> timedMetadataId3Frame,
                  ("timedMetadataId3Period" Core..=) Core.<$> timedMetadataId3Period,
                  ("timestampDeltaMilliseconds" Core..=) Core.<$>
                    timestampDeltaMilliseconds,
                  ("tsFileMode" Core..=) Core.<$> tsFileMode])

instance Core.FromJSON HlsGroupSettings where
        parseJSON
          = Core.withObject "HlsGroupSettings" Core.$
              \ x ->
                HlsGroupSettings' Core.<$>
                  (x Core..: "destination") Core.<*> x Core..:? "adMarkers" Core.<*>
                    x Core..:? "baseUrlContent"
                    Core.<*> x Core..:? "baseUrlContent1"
                    Core.<*> x Core..:? "baseUrlManifest"
                    Core.<*> x Core..:? "baseUrlManifest1"
                    Core.<*> x Core..:? "captionLanguageMappings"
                    Core.<*> x Core..:? "captionLanguageSetting"
                    Core.<*> x Core..:? "clientCache"
                    Core.<*> x Core..:? "codecSpecification"
                    Core.<*> x Core..:? "constantIv"
                    Core.<*> x Core..:? "directoryStructure"
                    Core.<*> x Core..:? "discontinuityTags"
                    Core.<*> x Core..:? "encryptionType"
                    Core.<*> x Core..:? "hlsCdnSettings"
                    Core.<*> x Core..:? "hlsId3SegmentTagging"
                    Core.<*> x Core..:? "iFrameOnlyPlaylists"
                    Core.<*> x Core..:? "incompleteSegmentBehavior"
                    Core.<*> x Core..:? "indexNSegments"
                    Core.<*> x Core..:? "inputLossAction"
                    Core.<*> x Core..:? "ivInManifest"
                    Core.<*> x Core..:? "ivSource"
                    Core.<*> x Core..:? "keepSegments"
                    Core.<*> x Core..:? "keyFormat"
                    Core.<*> x Core..:? "keyFormatVersions"
                    Core.<*> x Core..:? "keyProviderSettings"
                    Core.<*> x Core..:? "manifestCompression"
                    Core.<*> x Core..:? "manifestDurationFormat"
                    Core.<*> x Core..:? "minSegmentLength"
                    Core.<*> x Core..:? "mode"
                    Core.<*> x Core..:? "outputSelection"
                    Core.<*> x Core..:? "programDateTime"
                    Core.<*> x Core..:? "programDateTimePeriod"
                    Core.<*> x Core..:? "redundantManifest"
                    Core.<*> x Core..:? "segmentLength"
                    Core.<*> x Core..:? "segmentationMode"
                    Core.<*> x Core..:? "segmentsPerSubdirectory"
                    Core.<*> x Core..:? "streamInfResolution"
                    Core.<*> x Core..:? "timedMetadataId3Frame"
                    Core.<*> x Core..:? "timedMetadataId3Period"
                    Core.<*> x Core..:? "timestampDeltaMilliseconds"
                    Core.<*> x Core..:? "tsFileMode"

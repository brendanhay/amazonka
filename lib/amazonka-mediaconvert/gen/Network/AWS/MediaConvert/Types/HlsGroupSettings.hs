{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsGroupSettings
  ( HlsGroupSettings (..),

    -- * Smart constructor
    mkHlsGroupSettings,

    -- * Lenses
    hgsAdMarkers,
    hgsAdditionalManifests,
    hgsAudioOnlyHeader,
    hgsBaseUrl,
    hgsCaptionLanguageMappings,
    hgsCaptionLanguageSetting,
    hgsClientCache,
    hgsCodecSpecification,
    hgsDestination,
    hgsDestinationSettings,
    hgsDirectoryStructure,
    hgsEncryption,
    hgsManifestCompression,
    hgsManifestDurationFormat,
    hgsMinFinalSegmentLength,
    hgsMinSegmentLength,
    hgsOutputSelection,
    hgsProgramDateTime,
    hgsProgramDateTimePeriod,
    hgsSegmentControl,
    hgsSegmentLength,
    hgsSegmentsPerSubdirectory,
    hgsStreamInfResolution,
    hgsTimedMetadataId3Frame,
    hgsTimedMetadataId3Period,
    hgsTimestampDeltaMilliseconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DestinationSettings as Types
import qualified Network.AWS.MediaConvert.Types.HlsAdMarkers as Types
import qualified Network.AWS.MediaConvert.Types.HlsAdditionalManifest as Types
import qualified Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader as Types
import qualified Network.AWS.MediaConvert.Types.HlsCaptionLanguageMapping as Types
import qualified Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting as Types
import qualified Network.AWS.MediaConvert.Types.HlsClientCache as Types
import qualified Network.AWS.MediaConvert.Types.HlsCodecSpecification as Types
import qualified Network.AWS.MediaConvert.Types.HlsDirectoryStructure as Types
import qualified Network.AWS.MediaConvert.Types.HlsEncryptionSettings as Types
import qualified Network.AWS.MediaConvert.Types.HlsManifestCompression as Types
import qualified Network.AWS.MediaConvert.Types.HlsManifestDurationFormat as Types
import qualified Network.AWS.MediaConvert.Types.HlsOutputSelection as Types
import qualified Network.AWS.MediaConvert.Types.HlsProgramDateTime as Types
import qualified Network.AWS.MediaConvert.Types.HlsSegmentControl as Types
import qualified Network.AWS.MediaConvert.Types.HlsStreamInfResolution as Types
import qualified Network.AWS.MediaConvert.Types.HlsTimedMetadataId3Frame as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /See:/ 'mkHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
    adMarkers :: Core.Maybe [Types.HlsAdMarkers],
    -- | By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
    additionalManifests :: Core.Maybe [Types.HlsAdditionalManifest],
    -- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
    audioOnlyHeader :: Core.Maybe Types.HlsAudioOnlyHeader,
    -- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
    baseUrl :: Core.Maybe Core.Text,
    -- | Language to be used on Caption outputs
    captionLanguageMappings :: Core.Maybe [Types.HlsCaptionLanguageMapping],
    -- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
    captionLanguageSetting :: Core.Maybe Types.HlsCaptionLanguageSetting,
    -- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
    clientCache :: Core.Maybe Types.HlsClientCache,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
    codecSpecification :: Core.Maybe Types.HlsCodecSpecification,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Core.Maybe Types.DestinationSettings,
    -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Core.Maybe Types.HlsDirectoryStructure,
    -- | DRM settings.
    encryption :: Core.Maybe Types.HlsEncryptionSettings,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Core.Maybe Types.HlsManifestCompression,
    -- | Indicates whether the output manifest should use floating point values for segment duration.
    manifestDurationFormat :: Core.Maybe Types.HlsManifestDurationFormat,
    -- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Core.Maybe Core.Double,
    -- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
    minSegmentLength :: Core.Maybe Core.Natural,
    -- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
    outputSelection :: Core.Maybe Types.HlsOutputSelection,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
    programDateTime :: Core.Maybe Types.HlsProgramDateTime,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Core.Maybe Core.Natural,
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Core.Maybe Types.HlsSegmentControl,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
    segmentLength :: Core.Maybe Core.Natural,
    -- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
    segmentsPerSubdirectory :: Core.Maybe Core.Natural,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
    streamInfResolution :: Core.Maybe Types.HlsStreamInfResolution,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Core.Maybe Types.HlsTimedMetadataId3Frame,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Core.Maybe Core.Int,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsGroupSettings' value with any optional fields omitted.
mkHlsGroupSettings ::
  HlsGroupSettings
mkHlsGroupSettings =
  HlsGroupSettings'
    { adMarkers = Core.Nothing,
      additionalManifests = Core.Nothing,
      audioOnlyHeader = Core.Nothing,
      baseUrl = Core.Nothing,
      captionLanguageMappings = Core.Nothing,
      captionLanguageSetting = Core.Nothing,
      clientCache = Core.Nothing,
      codecSpecification = Core.Nothing,
      destination = Core.Nothing,
      destinationSettings = Core.Nothing,
      directoryStructure = Core.Nothing,
      encryption = Core.Nothing,
      manifestCompression = Core.Nothing,
      manifestDurationFormat = Core.Nothing,
      minFinalSegmentLength = Core.Nothing,
      minSegmentLength = Core.Nothing,
      outputSelection = Core.Nothing,
      programDateTime = Core.Nothing,
      programDateTimePeriod = Core.Nothing,
      segmentControl = Core.Nothing,
      segmentLength = Core.Nothing,
      segmentsPerSubdirectory = Core.Nothing,
      streamInfResolution = Core.Nothing,
      timedMetadataId3Frame = Core.Nothing,
      timedMetadataId3Period = Core.Nothing,
      timestampDeltaMilliseconds = Core.Nothing
    }

-- | Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdMarkers :: Lens.Lens' HlsGroupSettings (Core.Maybe [Types.HlsAdMarkers])
hgsAdMarkers = Lens.field @"adMarkers"
{-# DEPRECATED hgsAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdditionalManifests :: Lens.Lens' HlsGroupSettings (Core.Maybe [Types.HlsAdditionalManifest])
hgsAdditionalManifests = Lens.field @"additionalManifests"
{-# DEPRECATED hgsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
--
-- /Note:/ Consider using 'audioOnlyHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAudioOnlyHeader :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsAudioOnlyHeader)
hgsAudioOnlyHeader = Lens.field @"audioOnlyHeader"
{-# DEPRECATED hgsAudioOnlyHeader "Use generic-lens or generic-optics with 'audioOnlyHeader' instead." #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseUrl :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsBaseUrl = Lens.field @"baseUrl"
{-# DEPRECATED hgsBaseUrl "Use generic-lens or generic-optics with 'baseUrl' instead." #-}

-- | Language to be used on Caption outputs
--
-- /Note:/ Consider using 'captionLanguageMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageMappings :: Lens.Lens' HlsGroupSettings (Core.Maybe [Types.HlsCaptionLanguageMapping])
hgsCaptionLanguageMappings = Lens.field @"captionLanguageMappings"
{-# DEPRECATED hgsCaptionLanguageMappings "Use generic-lens or generic-optics with 'captionLanguageMappings' instead." #-}

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- /Note:/ Consider using 'captionLanguageSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageSetting :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = Lens.field @"captionLanguageSetting"
{-# DEPRECATED hgsCaptionLanguageSetting "Use generic-lens or generic-optics with 'captionLanguageSetting' instead." #-}

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsClientCache :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsClientCache)
hgsClientCache = Lens.field @"clientCache"
{-# DEPRECATED hgsClientCache "Use generic-lens or generic-optics with 'clientCache' instead." #-}

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- /Note:/ Consider using 'codecSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCodecSpecification :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsCodecSpecification)
hgsCodecSpecification = Lens.field @"codecSpecification"
{-# DEPRECATED hgsCodecSpecification "Use generic-lens or generic-optics with 'codecSpecification' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestination :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Text)
hgsDestination = Lens.field @"destination"
{-# DEPRECATED hgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestinationSettings :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.DestinationSettings)
hgsDestinationSettings = Lens.field @"destinationSettings"
{-# DEPRECATED hgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Indicates whether segments should be placed in subdirectories.
--
-- /Note:/ Consider using 'directoryStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDirectoryStructure :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsDirectoryStructure)
hgsDirectoryStructure = Lens.field @"directoryStructure"
{-# DEPRECATED hgsDirectoryStructure "Use generic-lens or generic-optics with 'directoryStructure' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsEncryption :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsEncryptionSettings)
hgsEncryption = Lens.field @"encryption"
{-# DEPRECATED hgsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | When set to GZIP, compresses HLS playlist.
--
-- /Note:/ Consider using 'manifestCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestCompression :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsManifestCompression)
hgsManifestCompression = Lens.field @"manifestCompression"
{-# DEPRECATED hgsManifestCompression "Use generic-lens or generic-optics with 'manifestCompression' instead." #-}

-- | Indicates whether the output manifest should use floating point values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestDurationFormat :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsManifestDurationFormat)
hgsManifestDurationFormat = Lens.field @"manifestDurationFormat"
{-# DEPRECATED hgsManifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinFinalSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Double)
hgsMinFinalSegmentLength = Lens.field @"minFinalSegmentLength"
{-# DEPRECATED hgsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- /Note:/ Consider using 'minSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsMinSegmentLength = Lens.field @"minSegmentLength"
{-# DEPRECATED hgsMinSegmentLength "Use generic-lens or generic-optics with 'minSegmentLength' instead." #-}

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
--
-- /Note:/ Consider using 'outputSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsOutputSelection :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsOutputSelection)
hgsOutputSelection = Lens.field @"outputSelection"
{-# DEPRECATED hgsOutputSelection "Use generic-lens or generic-optics with 'outputSelection' instead." #-}

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
--
-- /Note:/ Consider using 'programDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTime :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsProgramDateTime)
hgsProgramDateTime = Lens.field @"programDateTime"
{-# DEPRECATED hgsProgramDateTime "Use generic-lens or generic-optics with 'programDateTime' instead." #-}

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- /Note:/ Consider using 'programDateTimePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTimePeriod :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsProgramDateTimePeriod = Lens.field @"programDateTimePeriod"
{-# DEPRECATED hgsProgramDateTimePeriod "Use generic-lens or generic-optics with 'programDateTimePeriod' instead." #-}

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentControl :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsSegmentControl)
hgsSegmentControl = Lens.field @"segmentControl"
{-# DEPRECATED hgsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentLength :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsSegmentLength = Lens.field @"segmentLength"
{-# DEPRECATED hgsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
--
-- /Note:/ Consider using 'segmentsPerSubdirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Natural)
hgsSegmentsPerSubdirectory = Lens.field @"segmentsPerSubdirectory"
{-# DEPRECATED hgsSegmentsPerSubdirectory "Use generic-lens or generic-optics with 'segmentsPerSubdirectory' instead." #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsStreamInfResolution :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsStreamInfResolution)
hgsStreamInfResolution = Lens.field @"streamInfResolution"
{-# DEPRECATED hgsStreamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead." #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Core.Maybe Types.HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = Lens.field @"timedMetadataId3Frame"
{-# DEPRECATED hgsTimedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead." #-}

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Int)
hgsTimedMetadataId3Period = Lens.field @"timedMetadataId3Period"
{-# DEPRECATED hgsTimedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead." #-}

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- /Note:/ Consider using 'timestampDeltaMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Core.Maybe Core.Int)
hgsTimestampDeltaMilliseconds = Lens.field @"timestampDeltaMilliseconds"
{-# DEPRECATED hgsTimestampDeltaMilliseconds "Use generic-lens or generic-optics with 'timestampDeltaMilliseconds' instead." #-}

instance Core.FromJSON HlsGroupSettings where
  toJSON HlsGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("adMarkers" Core..=) Core.<$> adMarkers,
            ("additionalManifests" Core..=) Core.<$> additionalManifests,
            ("audioOnlyHeader" Core..=) Core.<$> audioOnlyHeader,
            ("baseUrl" Core..=) Core.<$> baseUrl,
            ("captionLanguageMappings" Core..=)
              Core.<$> captionLanguageMappings,
            ("captionLanguageSetting" Core..=) Core.<$> captionLanguageSetting,
            ("clientCache" Core..=) Core.<$> clientCache,
            ("codecSpecification" Core..=) Core.<$> codecSpecification,
            ("destination" Core..=) Core.<$> destination,
            ("destinationSettings" Core..=) Core.<$> destinationSettings,
            ("directoryStructure" Core..=) Core.<$> directoryStructure,
            ("encryption" Core..=) Core.<$> encryption,
            ("manifestCompression" Core..=) Core.<$> manifestCompression,
            ("manifestDurationFormat" Core..=) Core.<$> manifestDurationFormat,
            ("minFinalSegmentLength" Core..=) Core.<$> minFinalSegmentLength,
            ("minSegmentLength" Core..=) Core.<$> minSegmentLength,
            ("outputSelection" Core..=) Core.<$> outputSelection,
            ("programDateTime" Core..=) Core.<$> programDateTime,
            ("programDateTimePeriod" Core..=) Core.<$> programDateTimePeriod,
            ("segmentControl" Core..=) Core.<$> segmentControl,
            ("segmentLength" Core..=) Core.<$> segmentLength,
            ("segmentsPerSubdirectory" Core..=)
              Core.<$> segmentsPerSubdirectory,
            ("streamInfResolution" Core..=) Core.<$> streamInfResolution,
            ("timedMetadataId3Frame" Core..=) Core.<$> timedMetadataId3Frame,
            ("timedMetadataId3Period" Core..=) Core.<$> timedMetadataId3Period,
            ("timestampDeltaMilliseconds" Core..=)
              Core.<$> timestampDeltaMilliseconds
          ]
      )

instance Core.FromJSON HlsGroupSettings where
  parseJSON =
    Core.withObject "HlsGroupSettings" Core.$
      \x ->
        HlsGroupSettings'
          Core.<$> (x Core..:? "adMarkers")
          Core.<*> (x Core..:? "additionalManifests")
          Core.<*> (x Core..:? "audioOnlyHeader")
          Core.<*> (x Core..:? "baseUrl")
          Core.<*> (x Core..:? "captionLanguageMappings")
          Core.<*> (x Core..:? "captionLanguageSetting")
          Core.<*> (x Core..:? "clientCache")
          Core.<*> (x Core..:? "codecSpecification")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "destinationSettings")
          Core.<*> (x Core..:? "directoryStructure")
          Core.<*> (x Core..:? "encryption")
          Core.<*> (x Core..:? "manifestCompression")
          Core.<*> (x Core..:? "manifestDurationFormat")
          Core.<*> (x Core..:? "minFinalSegmentLength")
          Core.<*> (x Core..:? "minSegmentLength")
          Core.<*> (x Core..:? "outputSelection")
          Core.<*> (x Core..:? "programDateTime")
          Core.<*> (x Core..:? "programDateTimePeriod")
          Core.<*> (x Core..:? "segmentControl")
          Core.<*> (x Core..:? "segmentLength")
          Core.<*> (x Core..:? "segmentsPerSubdirectory")
          Core.<*> (x Core..:? "streamInfResolution")
          Core.<*> (x Core..:? "timedMetadataId3Frame")
          Core.<*> (x Core..:? "timedMetadataId3Period")
          Core.<*> (x Core..:? "timestampDeltaMilliseconds")

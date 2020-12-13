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
    hgsDirectoryStructure,
    hgsSegmentControl,
    hgsDestination,
    hgsTimedMetadataId3Period,
    hgsAdditionalManifests,
    hgsMinSegmentLength,
    hgsProgramDateTime,
    hgsProgramDateTimePeriod,
    hgsCodecSpecification,
    hgsCaptionLanguageMappings,
    hgsBaseURL,
    hgsDestinationSettings,
    hgsMinFinalSegmentLength,
    hgsAdMarkers,
    hgsEncryption,
    hgsSegmentLength,
    hgsTimedMetadataId3Frame,
    hgsOutputSelection,
    hgsCaptionLanguageSetting,
    hgsSegmentsPerSubdirectory,
    hgsManifestDurationFormat,
    hgsAudioOnlyHeader,
    hgsClientCache,
    hgsTimestampDeltaMilliseconds,
    hgsStreamInfResolution,
    hgsManifestCompression,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /See:/ 'mkHlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { -- | Indicates whether segments should be placed in subdirectories.
    directoryStructure :: Lude.Maybe HlsDirectoryStructure,
    -- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
    segmentControl :: Lude.Maybe HlsSegmentControl,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Lude.Maybe Lude.Text,
    -- | Timed Metadata interval in seconds.
    timedMetadataId3Period :: Lude.Maybe Lude.Int,
    -- | By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
    additionalManifests :: Lude.Maybe [HlsAdditionalManifest],
    -- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
    minSegmentLength :: Lude.Maybe Lude.Natural,
    -- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
    programDateTime :: Lude.Maybe HlsProgramDateTime,
    -- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
    programDateTimePeriod :: Lude.Maybe Lude.Natural,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
    codecSpecification :: Lude.Maybe HlsCodecSpecification,
    -- | Language to be used on Caption outputs
    captionLanguageMappings :: Lude.Maybe [HlsCaptionLanguageMapping],
    -- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
    baseURL :: Lude.Maybe Lude.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Lude.Maybe DestinationSettings,
    -- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Lude.Maybe Lude.Double,
    -- | Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
    adMarkers :: Lude.Maybe [HlsAdMarkers],
    -- | DRM settings.
    encryption :: Lude.Maybe HlsEncryptionSettings,
    -- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
    segmentLength :: Lude.Maybe Lude.Natural,
    -- | Indicates ID3 frame that has the timecode.
    timedMetadataId3Frame :: Lude.Maybe HlsTimedMetadataId3Frame,
    -- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
    outputSelection :: Lude.Maybe HlsOutputSelection,
    -- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
    captionLanguageSetting :: Lude.Maybe HlsCaptionLanguageSetting,
    -- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
    segmentsPerSubdirectory :: Lude.Maybe Lude.Natural,
    -- | Indicates whether the output manifest should use floating point values for segment duration.
    manifestDurationFormat :: Lude.Maybe HlsManifestDurationFormat,
    -- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
    audioOnlyHeader :: Lude.Maybe HlsAudioOnlyHeader,
    -- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
    clientCache :: Lude.Maybe HlsClientCache,
    -- | Provides an extra millisecond delta offset to fine tune the timestamps.
    timestampDeltaMilliseconds :: Lude.Maybe Lude.Int,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
    streamInfResolution :: Lude.Maybe HlsStreamInfResolution,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Lude.Maybe HlsManifestCompression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- * 'directoryStructure' - Indicates whether segments should be placed in subdirectories.
-- * 'segmentControl' - When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
-- * 'destination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
-- * 'timedMetadataId3Period' - Timed Metadata interval in seconds.
-- * 'additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
-- * 'minSegmentLength' - When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
-- * 'programDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
-- * 'programDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
-- * 'codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
-- * 'captionLanguageMappings' - Language to be used on Caption outputs
-- * 'baseURL' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
-- * 'destinationSettings' - Settings associated with the destination. Will vary based on the type of destination
-- * 'minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
-- * 'adMarkers' - Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
-- * 'encryption' - DRM settings.
-- * 'segmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
-- * 'timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
-- * 'outputSelection' - Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
-- * 'captionLanguageSetting' - Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
-- * 'segmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
-- * 'manifestDurationFormat' - Indicates whether the output manifest should use floating point values for segment duration.
-- * 'audioOnlyHeader' - Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
-- * 'clientCache' - Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
-- * 'timestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
-- * 'streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
-- * 'manifestCompression' - When set to GZIP, compresses HLS playlist.
mkHlsGroupSettings ::
  HlsGroupSettings
mkHlsGroupSettings =
  HlsGroupSettings'
    { directoryStructure = Lude.Nothing,
      segmentControl = Lude.Nothing,
      destination = Lude.Nothing,
      timedMetadataId3Period = Lude.Nothing,
      additionalManifests = Lude.Nothing,
      minSegmentLength = Lude.Nothing,
      programDateTime = Lude.Nothing,
      programDateTimePeriod = Lude.Nothing,
      codecSpecification = Lude.Nothing,
      captionLanguageMappings = Lude.Nothing,
      baseURL = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      minFinalSegmentLength = Lude.Nothing,
      adMarkers = Lude.Nothing,
      encryption = Lude.Nothing,
      segmentLength = Lude.Nothing,
      timedMetadataId3Frame = Lude.Nothing,
      outputSelection = Lude.Nothing,
      captionLanguageSetting = Lude.Nothing,
      segmentsPerSubdirectory = Lude.Nothing,
      manifestDurationFormat = Lude.Nothing,
      audioOnlyHeader = Lude.Nothing,
      clientCache = Lude.Nothing,
      timestampDeltaMilliseconds = Lude.Nothing,
      streamInfResolution = Lude.Nothing,
      manifestCompression = Lude.Nothing
    }

-- | Indicates whether segments should be placed in subdirectories.
--
-- /Note:/ Consider using 'directoryStructure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDirectoryStructure :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsDirectoryStructure)
hgsDirectoryStructure = Lens.lens (directoryStructure :: HlsGroupSettings -> Lude.Maybe HlsDirectoryStructure) (\s a -> s {directoryStructure = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDirectoryStructure "Use generic-lens or generic-optics with 'directoryStructure' instead." #-}

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentControl :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsSegmentControl)
hgsSegmentControl = Lens.lens (segmentControl :: HlsGroupSettings -> Lude.Maybe HlsSegmentControl) (\s a -> s {segmentControl = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestination :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsDestination = Lens.lens (destination :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Period :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Int)
hgsTimedMetadataId3Period = Lens.lens (timedMetadataId3Period :: HlsGroupSettings -> Lude.Maybe Lude.Int) (\s a -> s {timedMetadataId3Period = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead." #-}

-- | By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdditionalManifests :: Lens.Lens' HlsGroupSettings (Lude.Maybe [HlsAdditionalManifest])
hgsAdditionalManifests = Lens.lens (additionalManifests :: HlsGroupSettings -> Lude.Maybe [HlsAdditionalManifest]) (\s a -> s {additionalManifests = a} :: HlsGroupSettings)
{-# DEPRECATED hgsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- /Note:/ Consider using 'minSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinSegmentLength :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsMinSegmentLength = Lens.lens (minSegmentLength :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minSegmentLength = a} :: HlsGroupSettings)
{-# DEPRECATED hgsMinSegmentLength "Use generic-lens or generic-optics with 'minSegmentLength' instead." #-}

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
--
-- /Note:/ Consider using 'programDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsProgramDateTime :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsProgramDateTime)
hgsProgramDateTime = Lens.lens (programDateTime :: HlsGroupSettings -> Lude.Maybe HlsProgramDateTime) (\s a -> s {programDateTime = a} :: HlsGroupSettings)
{-# DEPRECATED hgsProgramDateTime "Use generic-lens or generic-optics with 'programDateTime' instead." #-}

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

-- | Language to be used on Caption outputs
--
-- /Note:/ Consider using 'captionLanguageMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageMappings :: Lens.Lens' HlsGroupSettings (Lude.Maybe [HlsCaptionLanguageMapping])
hgsCaptionLanguageMappings = Lens.lens (captionLanguageMappings :: HlsGroupSettings -> Lude.Maybe [HlsCaptionLanguageMapping]) (\s a -> s {captionLanguageMappings = a} :: HlsGroupSettings)
{-# DEPRECATED hgsCaptionLanguageMappings "Use generic-lens or generic-optics with 'captionLanguageMappings' instead." #-}

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- /Note:/ Consider using 'baseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsBaseURL :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Text)
hgsBaseURL = Lens.lens (baseURL :: HlsGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURL = a} :: HlsGroupSettings)
{-# DEPRECATED hgsBaseURL "Use generic-lens or generic-optics with 'baseURL' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsDestinationSettings :: Lens.Lens' HlsGroupSettings (Lude.Maybe DestinationSettings)
hgsDestinationSettings = Lens.lens (destinationSettings :: HlsGroupSettings -> Lude.Maybe DestinationSettings) (\s a -> s {destinationSettings = a} :: HlsGroupSettings)
{-# DEPRECATED hgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsMinFinalSegmentLength :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Double)
hgsMinFinalSegmentLength = Lens.lens (minFinalSegmentLength :: HlsGroupSettings -> Lude.Maybe Lude.Double) (\s a -> s {minFinalSegmentLength = a} :: HlsGroupSettings)
{-# DEPRECATED hgsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
--
-- /Note:/ Consider using 'adMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAdMarkers :: Lens.Lens' HlsGroupSettings (Lude.Maybe [HlsAdMarkers])
hgsAdMarkers = Lens.lens (adMarkers :: HlsGroupSettings -> Lude.Maybe [HlsAdMarkers]) (\s a -> s {adMarkers = a} :: HlsGroupSettings)
{-# DEPRECATED hgsAdMarkers "Use generic-lens or generic-optics with 'adMarkers' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsEncryption :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsEncryptionSettings)
hgsEncryption = Lens.lens (encryption :: HlsGroupSettings -> Lude.Maybe HlsEncryptionSettings) (\s a -> s {encryption = a} :: HlsGroupSettings)
{-# DEPRECATED hgsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentLength :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsSegmentLength = Lens.lens (segmentLength :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentLength = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimedMetadataId3Frame :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = Lens.lens (timedMetadataId3Frame :: HlsGroupSettings -> Lude.Maybe HlsTimedMetadataId3Frame) (\s a -> s {timedMetadataId3Frame = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead." #-}

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
--
-- /Note:/ Consider using 'outputSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsOutputSelection :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsOutputSelection)
hgsOutputSelection = Lens.lens (outputSelection :: HlsGroupSettings -> Lude.Maybe HlsOutputSelection) (\s a -> s {outputSelection = a} :: HlsGroupSettings)
{-# DEPRECATED hgsOutputSelection "Use generic-lens or generic-optics with 'outputSelection' instead." #-}

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- /Note:/ Consider using 'captionLanguageSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsCaptionLanguageSetting :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = Lens.lens (captionLanguageSetting :: HlsGroupSettings -> Lude.Maybe HlsCaptionLanguageSetting) (\s a -> s {captionLanguageSetting = a} :: HlsGroupSettings)
{-# DEPRECATED hgsCaptionLanguageSetting "Use generic-lens or generic-optics with 'captionLanguageSetting' instead." #-}

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
--
-- /Note:/ Consider using 'segmentsPerSubdirectory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsSegmentsPerSubdirectory :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Natural)
hgsSegmentsPerSubdirectory = Lens.lens (segmentsPerSubdirectory :: HlsGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentsPerSubdirectory = a} :: HlsGroupSettings)
{-# DEPRECATED hgsSegmentsPerSubdirectory "Use generic-lens or generic-optics with 'segmentsPerSubdirectory' instead." #-}

-- | Indicates whether the output manifest should use floating point values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsManifestDurationFormat :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = Lens.lens (manifestDurationFormat :: HlsGroupSettings -> Lude.Maybe HlsManifestDurationFormat) (\s a -> s {manifestDurationFormat = a} :: HlsGroupSettings)
{-# DEPRECATED hgsManifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead." #-}

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
--
-- /Note:/ Consider using 'audioOnlyHeader' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsAudioOnlyHeader :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsAudioOnlyHeader)
hgsAudioOnlyHeader = Lens.lens (audioOnlyHeader :: HlsGroupSettings -> Lude.Maybe HlsAudioOnlyHeader) (\s a -> s {audioOnlyHeader = a} :: HlsGroupSettings)
{-# DEPRECATED hgsAudioOnlyHeader "Use generic-lens or generic-optics with 'audioOnlyHeader' instead." #-}

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsClientCache :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsClientCache)
hgsClientCache = Lens.lens (clientCache :: HlsGroupSettings -> Lude.Maybe HlsClientCache) (\s a -> s {clientCache = a} :: HlsGroupSettings)
{-# DEPRECATED hgsClientCache "Use generic-lens or generic-optics with 'clientCache' instead." #-}

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- /Note:/ Consider using 'timestampDeltaMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsTimestampDeltaMilliseconds :: Lens.Lens' HlsGroupSettings (Lude.Maybe Lude.Int)
hgsTimestampDeltaMilliseconds = Lens.lens (timestampDeltaMilliseconds :: HlsGroupSettings -> Lude.Maybe Lude.Int) (\s a -> s {timestampDeltaMilliseconds = a} :: HlsGroupSettings)
{-# DEPRECATED hgsTimestampDeltaMilliseconds "Use generic-lens or generic-optics with 'timestampDeltaMilliseconds' instead." #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsStreamInfResolution :: Lens.Lens' HlsGroupSettings (Lude.Maybe HlsStreamInfResolution)
hgsStreamInfResolution = Lens.lens (streamInfResolution :: HlsGroupSettings -> Lude.Maybe HlsStreamInfResolution) (\s a -> s {streamInfResolution = a} :: HlsGroupSettings)
{-# DEPRECATED hgsStreamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead." #-}

-- | When set to GZIP, compresses HLS playlist.
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
            Lude.<*> (x Lude..:? "segmentControl")
            Lude.<*> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "timedMetadataId3Period")
            Lude.<*> (x Lude..:? "additionalManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "minSegmentLength")
            Lude.<*> (x Lude..:? "programDateTime")
            Lude.<*> (x Lude..:? "programDateTimePeriod")
            Lude.<*> (x Lude..:? "codecSpecification")
            Lude.<*> (x Lude..:? "captionLanguageMappings" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "baseUrl")
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "minFinalSegmentLength")
            Lude.<*> (x Lude..:? "adMarkers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "segmentLength")
            Lude.<*> (x Lude..:? "timedMetadataId3Frame")
            Lude.<*> (x Lude..:? "outputSelection")
            Lude.<*> (x Lude..:? "captionLanguageSetting")
            Lude.<*> (x Lude..:? "segmentsPerSubdirectory")
            Lude.<*> (x Lude..:? "manifestDurationFormat")
            Lude.<*> (x Lude..:? "audioOnlyHeader")
            Lude.<*> (x Lude..:? "clientCache")
            Lude.<*> (x Lude..:? "timestampDeltaMilliseconds")
            Lude.<*> (x Lude..:? "streamInfResolution")
            Lude.<*> (x Lude..:? "manifestCompression")
      )

instance Lude.ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("directoryStructure" Lude..=) Lude.<$> directoryStructure,
            ("segmentControl" Lude..=) Lude.<$> segmentControl,
            ("destination" Lude..=) Lude.<$> destination,
            ("timedMetadataId3Period" Lude..=) Lude.<$> timedMetadataId3Period,
            ("additionalManifests" Lude..=) Lude.<$> additionalManifests,
            ("minSegmentLength" Lude..=) Lude.<$> minSegmentLength,
            ("programDateTime" Lude..=) Lude.<$> programDateTime,
            ("programDateTimePeriod" Lude..=) Lude.<$> programDateTimePeriod,
            ("codecSpecification" Lude..=) Lude.<$> codecSpecification,
            ("captionLanguageMappings" Lude..=)
              Lude.<$> captionLanguageMappings,
            ("baseUrl" Lude..=) Lude.<$> baseURL,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("minFinalSegmentLength" Lude..=) Lude.<$> minFinalSegmentLength,
            ("adMarkers" Lude..=) Lude.<$> adMarkers,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("segmentLength" Lude..=) Lude.<$> segmentLength,
            ("timedMetadataId3Frame" Lude..=) Lude.<$> timedMetadataId3Frame,
            ("outputSelection" Lude..=) Lude.<$> outputSelection,
            ("captionLanguageSetting" Lude..=) Lude.<$> captionLanguageSetting,
            ("segmentsPerSubdirectory" Lude..=)
              Lude.<$> segmentsPerSubdirectory,
            ("manifestDurationFormat" Lude..=) Lude.<$> manifestDurationFormat,
            ("audioOnlyHeader" Lude..=) Lude.<$> audioOnlyHeader,
            ("clientCache" Lude..=) Lude.<$> clientCache,
            ("timestampDeltaMilliseconds" Lude..=)
              Lude.<$> timestampDeltaMilliseconds,
            ("streamInfResolution" Lude..=) Lude.<$> streamInfResolution,
            ("manifestCompression" Lude..=) Lude.<$> manifestCompression
          ]
      )

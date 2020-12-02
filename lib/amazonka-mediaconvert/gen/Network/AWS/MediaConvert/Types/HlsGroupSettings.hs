{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsGroupSettings where

import Network.AWS.Lens
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
import Network.AWS.Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /See:/ 'hlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { _hgsDirectoryStructure ::
      !(Maybe HlsDirectoryStructure),
    _hgsSegmentControl :: !(Maybe HlsSegmentControl),
    _hgsDestination :: !(Maybe Text),
    _hgsTimedMetadataId3Period :: !(Maybe Int),
    _hgsAdditionalManifests ::
      !(Maybe [HlsAdditionalManifest]),
    _hgsMinSegmentLength :: !(Maybe Nat),
    _hgsProgramDateTime :: !(Maybe HlsProgramDateTime),
    _hgsProgramDateTimePeriod :: !(Maybe Nat),
    _hgsCodecSpecification :: !(Maybe HlsCodecSpecification),
    _hgsCaptionLanguageMappings ::
      !(Maybe [HlsCaptionLanguageMapping]),
    _hgsBaseURL :: !(Maybe Text),
    _hgsDestinationSettings :: !(Maybe DestinationSettings),
    _hgsMinFinalSegmentLength :: !(Maybe Double),
    _hgsAdMarkers :: !(Maybe [HlsAdMarkers]),
    _hgsEncryption :: !(Maybe HlsEncryptionSettings),
    _hgsSegmentLength :: !(Maybe Nat),
    _hgsTimedMetadataId3Frame ::
      !(Maybe HlsTimedMetadataId3Frame),
    _hgsOutputSelection :: !(Maybe HlsOutputSelection),
    _hgsCaptionLanguageSetting ::
      !(Maybe HlsCaptionLanguageSetting),
    _hgsSegmentsPerSubdirectory :: !(Maybe Nat),
    _hgsManifestDurationFormat ::
      !(Maybe HlsManifestDurationFormat),
    _hgsAudioOnlyHeader :: !(Maybe HlsAudioOnlyHeader),
    _hgsClientCache :: !(Maybe HlsClientCache),
    _hgsTimestampDeltaMilliseconds :: !(Maybe Int),
    _hgsStreamInfResolution ::
      !(Maybe HlsStreamInfResolution),
    _hgsManifestCompression ::
      !(Maybe HlsManifestCompression)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsDirectoryStructure' - Indicates whether segments should be placed in subdirectories.
--
-- * 'hgsSegmentControl' - When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
--
-- * 'hgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'hgsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'hgsAdditionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- * 'hgsMinSegmentLength' - When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- * 'hgsProgramDateTime' - Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
--
-- * 'hgsProgramDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- * 'hgsCodecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- * 'hgsCaptionLanguageMappings' - Language to be used on Caption outputs
--
-- * 'hgsBaseURL' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsDestinationSettings' - Settings associated with the destination. Will vary based on the type of destination
--
-- * 'hgsMinFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- * 'hgsAdMarkers' - Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
--
-- * 'hgsEncryption' - DRM settings.
--
-- * 'hgsSegmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- * 'hgsTimedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
--
-- * 'hgsOutputSelection' - Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
--
-- * 'hgsCaptionLanguageSetting' - Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
--
-- * 'hgsSegmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
--
-- * 'hgsManifestDurationFormat' - Indicates whether the output manifest should use floating point values for segment duration.
--
-- * 'hgsAudioOnlyHeader' - Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
--
-- * 'hgsClientCache' - Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- * 'hgsTimestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- * 'hgsStreamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- * 'hgsManifestCompression' - When set to GZIP, compresses HLS playlist.
hlsGroupSettings ::
  HlsGroupSettings
hlsGroupSettings =
  HlsGroupSettings'
    { _hgsDirectoryStructure = Nothing,
      _hgsSegmentControl = Nothing,
      _hgsDestination = Nothing,
      _hgsTimedMetadataId3Period = Nothing,
      _hgsAdditionalManifests = Nothing,
      _hgsMinSegmentLength = Nothing,
      _hgsProgramDateTime = Nothing,
      _hgsProgramDateTimePeriod = Nothing,
      _hgsCodecSpecification = Nothing,
      _hgsCaptionLanguageMappings = Nothing,
      _hgsBaseURL = Nothing,
      _hgsDestinationSettings = Nothing,
      _hgsMinFinalSegmentLength = Nothing,
      _hgsAdMarkers = Nothing,
      _hgsEncryption = Nothing,
      _hgsSegmentLength = Nothing,
      _hgsTimedMetadataId3Frame = Nothing,
      _hgsOutputSelection = Nothing,
      _hgsCaptionLanguageSetting = Nothing,
      _hgsSegmentsPerSubdirectory = Nothing,
      _hgsManifestDurationFormat = Nothing,
      _hgsAudioOnlyHeader = Nothing,
      _hgsClientCache = Nothing,
      _hgsTimestampDeltaMilliseconds = Nothing,
      _hgsStreamInfResolution = Nothing,
      _hgsManifestCompression = Nothing
    }

-- | Indicates whether segments should be placed in subdirectories.
hgsDirectoryStructure :: Lens' HlsGroupSettings (Maybe HlsDirectoryStructure)
hgsDirectoryStructure = lens _hgsDirectoryStructure (\s a -> s {_hgsDirectoryStructure = a})

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
hgsSegmentControl :: Lens' HlsGroupSettings (Maybe HlsSegmentControl)
hgsSegmentControl = lens _hgsSegmentControl (\s a -> s {_hgsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
hgsDestination :: Lens' HlsGroupSettings (Maybe Text)
hgsDestination = lens _hgsDestination (\s a -> s {_hgsDestination = a})

-- | Timed Metadata interval in seconds.
hgsTimedMetadataId3Period :: Lens' HlsGroupSettings (Maybe Int)
hgsTimedMetadataId3Period = lens _hgsTimedMetadataId3Period (\s a -> s {_hgsTimedMetadataId3Period = a})

-- | By default, the service creates one top-level .m3u8 HLS manifest for each HLS output group in your job. This default manifest references every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here.
hgsAdditionalManifests :: Lens' HlsGroupSettings [HlsAdditionalManifest]
hgsAdditionalManifests = lens _hgsAdditionalManifests (\s a -> s {_hgsAdditionalManifests = a}) . _Default . _Coerce

-- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
hgsMinSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsMinSegmentLength = lens _hgsMinSegmentLength (\s a -> s {_hgsMinSegmentLength = a}) . mapping _Nat

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
hgsProgramDateTime :: Lens' HlsGroupSettings (Maybe HlsProgramDateTime)
hgsProgramDateTime = lens _hgsProgramDateTime (\s a -> s {_hgsProgramDateTime = a})

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hgsProgramDateTimePeriod :: Lens' HlsGroupSettings (Maybe Natural)
hgsProgramDateTimePeriod = lens _hgsProgramDateTimePeriod (\s a -> s {_hgsProgramDateTimePeriod = a}) . mapping _Nat

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
hgsCodecSpecification :: Lens' HlsGroupSettings (Maybe HlsCodecSpecification)
hgsCodecSpecification = lens _hgsCodecSpecification (\s a -> s {_hgsCodecSpecification = a})

-- | Language to be used on Caption outputs
hgsCaptionLanguageMappings :: Lens' HlsGroupSettings [HlsCaptionLanguageMapping]
hgsCaptionLanguageMappings = lens _hgsCaptionLanguageMappings (\s a -> s {_hgsCaptionLanguageMappings = a}) . _Default . _Coerce

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURL :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURL = lens _hgsBaseURL (\s a -> s {_hgsBaseURL = a})

-- | Settings associated with the destination. Will vary based on the type of destination
hgsDestinationSettings :: Lens' HlsGroupSettings (Maybe DestinationSettings)
hgsDestinationSettings = lens _hgsDestinationSettings (\s a -> s {_hgsDestinationSettings = a})

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
hgsMinFinalSegmentLength :: Lens' HlsGroupSettings (Maybe Double)
hgsMinFinalSegmentLength = lens _hgsMinFinalSegmentLength (\s a -> s {_hgsMinFinalSegmentLength = a})

-- | Choose one or more ad marker types to decorate your Apple HLS manifest. This setting does not determine whether SCTE-35 markers appear in the outputs themselves.
hgsAdMarkers :: Lens' HlsGroupSettings [HlsAdMarkers]
hgsAdMarkers = lens _hgsAdMarkers (\s a -> s {_hgsAdMarkers = a}) . _Default . _Coerce

-- | DRM settings.
hgsEncryption :: Lens' HlsGroupSettings (Maybe HlsEncryptionSettings)
hgsEncryption = lens _hgsEncryption (\s a -> s {_hgsEncryption = a})

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
hgsSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentLength = lens _hgsSegmentLength (\s a -> s {_hgsSegmentLength = a}) . mapping _Nat

-- | Indicates ID3 frame that has the timecode.
hgsTimedMetadataId3Frame :: Lens' HlsGroupSettings (Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = lens _hgsTimedMetadataId3Frame (\s a -> s {_hgsTimedMetadataId3Frame = a})

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
hgsOutputSelection :: Lens' HlsGroupSettings (Maybe HlsOutputSelection)
hgsOutputSelection = lens _hgsOutputSelection (\s a -> s {_hgsOutputSelection = a})

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
hgsCaptionLanguageSetting :: Lens' HlsGroupSettings (Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = lens _hgsCaptionLanguageSetting (\s a -> s {_hgsCaptionLanguageSetting = a})

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
hgsSegmentsPerSubdirectory :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentsPerSubdirectory = lens _hgsSegmentsPerSubdirectory (\s a -> s {_hgsSegmentsPerSubdirectory = a}) . mapping _Nat

-- | Indicates whether the output manifest should use floating point values for segment duration.
hgsManifestDurationFormat :: Lens' HlsGroupSettings (Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = lens _hgsManifestDurationFormat (\s a -> s {_hgsManifestDurationFormat = a})

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
hgsAudioOnlyHeader :: Lens' HlsGroupSettings (Maybe HlsAudioOnlyHeader)
hgsAudioOnlyHeader = lens _hgsAudioOnlyHeader (\s a -> s {_hgsAudioOnlyHeader = a})

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
hgsClientCache :: Lens' HlsGroupSettings (Maybe HlsClientCache)
hgsClientCache = lens _hgsClientCache (\s a -> s {_hgsClientCache = a})

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hgsTimestampDeltaMilliseconds :: Lens' HlsGroupSettings (Maybe Int)
hgsTimestampDeltaMilliseconds = lens _hgsTimestampDeltaMilliseconds (\s a -> s {_hgsTimestampDeltaMilliseconds = a})

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
hgsStreamInfResolution :: Lens' HlsGroupSettings (Maybe HlsStreamInfResolution)
hgsStreamInfResolution = lens _hgsStreamInfResolution (\s a -> s {_hgsStreamInfResolution = a})

-- | When set to GZIP, compresses HLS playlist.
hgsManifestCompression :: Lens' HlsGroupSettings (Maybe HlsManifestCompression)
hgsManifestCompression = lens _hgsManifestCompression (\s a -> s {_hgsManifestCompression = a})

instance FromJSON HlsGroupSettings where
  parseJSON =
    withObject
      "HlsGroupSettings"
      ( \x ->
          HlsGroupSettings'
            <$> (x .:? "directoryStructure")
            <*> (x .:? "segmentControl")
            <*> (x .:? "destination")
            <*> (x .:? "timedMetadataId3Period")
            <*> (x .:? "additionalManifests" .!= mempty)
            <*> (x .:? "minSegmentLength")
            <*> (x .:? "programDateTime")
            <*> (x .:? "programDateTimePeriod")
            <*> (x .:? "codecSpecification")
            <*> (x .:? "captionLanguageMappings" .!= mempty)
            <*> (x .:? "baseUrl")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "minFinalSegmentLength")
            <*> (x .:? "adMarkers" .!= mempty)
            <*> (x .:? "encryption")
            <*> (x .:? "segmentLength")
            <*> (x .:? "timedMetadataId3Frame")
            <*> (x .:? "outputSelection")
            <*> (x .:? "captionLanguageSetting")
            <*> (x .:? "segmentsPerSubdirectory")
            <*> (x .:? "manifestDurationFormat")
            <*> (x .:? "audioOnlyHeader")
            <*> (x .:? "clientCache")
            <*> (x .:? "timestampDeltaMilliseconds")
            <*> (x .:? "streamInfResolution")
            <*> (x .:? "manifestCompression")
      )

instance Hashable HlsGroupSettings

instance NFData HlsGroupSettings

instance ToJSON HlsGroupSettings where
  toJSON HlsGroupSettings' {..} =
    object
      ( catMaybes
          [ ("directoryStructure" .=) <$> _hgsDirectoryStructure,
            ("segmentControl" .=) <$> _hgsSegmentControl,
            ("destination" .=) <$> _hgsDestination,
            ("timedMetadataId3Period" .=) <$> _hgsTimedMetadataId3Period,
            ("additionalManifests" .=) <$> _hgsAdditionalManifests,
            ("minSegmentLength" .=) <$> _hgsMinSegmentLength,
            ("programDateTime" .=) <$> _hgsProgramDateTime,
            ("programDateTimePeriod" .=) <$> _hgsProgramDateTimePeriod,
            ("codecSpecification" .=) <$> _hgsCodecSpecification,
            ("captionLanguageMappings" .=) <$> _hgsCaptionLanguageMappings,
            ("baseUrl" .=) <$> _hgsBaseURL,
            ("destinationSettings" .=) <$> _hgsDestinationSettings,
            ("minFinalSegmentLength" .=) <$> _hgsMinFinalSegmentLength,
            ("adMarkers" .=) <$> _hgsAdMarkers,
            ("encryption" .=) <$> _hgsEncryption,
            ("segmentLength" .=) <$> _hgsSegmentLength,
            ("timedMetadataId3Frame" .=) <$> _hgsTimedMetadataId3Frame,
            ("outputSelection" .=) <$> _hgsOutputSelection,
            ("captionLanguageSetting" .=) <$> _hgsCaptionLanguageSetting,
            ("segmentsPerSubdirectory" .=) <$> _hgsSegmentsPerSubdirectory,
            ("manifestDurationFormat" .=) <$> _hgsManifestDurationFormat,
            ("audioOnlyHeader" .=) <$> _hgsAudioOnlyHeader,
            ("clientCache" .=) <$> _hgsClientCache,
            ("timestampDeltaMilliseconds" .=)
              <$> _hgsTimestampDeltaMilliseconds,
            ("streamInfResolution" .=) <$> _hgsStreamInfResolution,
            ("manifestCompression" .=) <$> _hgsManifestCompression
          ]
      )

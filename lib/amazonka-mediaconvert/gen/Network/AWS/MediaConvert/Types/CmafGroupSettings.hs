{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CmafAdditionalManifest
import Network.AWS.MediaConvert.Types.CmafClientCache
import Network.AWS.MediaConvert.Types.CmafCodecSpecification
import Network.AWS.MediaConvert.Types.CmafEncryptionSettings
import Network.AWS.MediaConvert.Types.CmafManifestCompression
import Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
import Network.AWS.MediaConvert.Types.CmafMpdProfile
import Network.AWS.MediaConvert.Types.CmafSegmentControl
import Network.AWS.MediaConvert.Types.CmafStreamInfResolution
import Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
import Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
import Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /See:/ 'cmafGroupSettings' smart constructor.
data CmafGroupSettings = CmafGroupSettings'
  { _cgsFragmentLength ::
      !(Maybe Nat),
    _cgsSegmentControl :: !(Maybe CmafSegmentControl),
    _cgsDestination :: !(Maybe Text),
    _cgsMinBufferTime :: !(Maybe Nat),
    _cgsMpdProfile :: !(Maybe CmafMpdProfile),
    _cgsWriteHlsManifest :: !(Maybe CmafWriteHLSManifest),
    _cgsAdditionalManifests ::
      !(Maybe [CmafAdditionalManifest]),
    _cgsCodecSpecification ::
      !(Maybe CmafCodecSpecification),
    _cgsBaseURL :: !(Maybe Text),
    _cgsDestinationSettings :: !(Maybe DestinationSettings),
    _cgsMinFinalSegmentLength :: !(Maybe Double),
    _cgsWriteDashManifest :: !(Maybe CmafWriteDASHManifest),
    _cgsEncryption :: !(Maybe CmafEncryptionSettings),
    _cgsSegmentLength :: !(Maybe Nat),
    _cgsManifestDurationFormat ::
      !(Maybe CmafManifestDurationFormat),
    _cgsClientCache :: !(Maybe CmafClientCache),
    _cgsWriteSegmentTimelineInRepresentation ::
      !(Maybe CmafWriteSegmentTimelineInRepresentation),
    _cgsStreamInfResolution ::
      !(Maybe CmafStreamInfResolution),
    _cgsManifestCompression ::
      !(Maybe CmafManifestCompression)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CmafGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsFragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- * 'cgsSegmentControl' - When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- * 'cgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'cgsMinBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- * 'cgsMpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- * 'cgsWriteHlsManifest' - When set to ENABLED, an Apple HLS manifest will be generated for this output.
--
-- * 'cgsAdditionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
--
-- * 'cgsCodecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- * 'cgsBaseURL' - A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- * 'cgsDestinationSettings' - Settings associated with the destination. Will vary based on the type of destination
--
-- * 'cgsMinFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- * 'cgsWriteDashManifest' - When set to ENABLED, a DASH MPD manifest will be generated for this output.
--
-- * 'cgsEncryption' - DRM settings.
--
-- * 'cgsSegmentLength' - Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
--
-- * 'cgsManifestDurationFormat' - Indicates whether the output manifest should use floating point values for segment duration.
--
-- * 'cgsClientCache' - Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- * 'cgsWriteSegmentTimelineInRepresentation' - When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
--
-- * 'cgsStreamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- * 'cgsManifestCompression' - When set to GZIP, compresses HLS playlist.
cmafGroupSettings ::
  CmafGroupSettings
cmafGroupSettings =
  CmafGroupSettings'
    { _cgsFragmentLength = Nothing,
      _cgsSegmentControl = Nothing,
      _cgsDestination = Nothing,
      _cgsMinBufferTime = Nothing,
      _cgsMpdProfile = Nothing,
      _cgsWriteHlsManifest = Nothing,
      _cgsAdditionalManifests = Nothing,
      _cgsCodecSpecification = Nothing,
      _cgsBaseURL = Nothing,
      _cgsDestinationSettings = Nothing,
      _cgsMinFinalSegmentLength = Nothing,
      _cgsWriteDashManifest = Nothing,
      _cgsEncryption = Nothing,
      _cgsSegmentLength = Nothing,
      _cgsManifestDurationFormat = Nothing,
      _cgsClientCache = Nothing,
      _cgsWriteSegmentTimelineInRepresentation = Nothing,
      _cgsStreamInfResolution = Nothing,
      _cgsManifestCompression = Nothing
    }

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
cgsFragmentLength :: Lens' CmafGroupSettings (Maybe Natural)
cgsFragmentLength = lens _cgsFragmentLength (\s a -> s {_cgsFragmentLength = a}) . mapping _Nat

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
cgsSegmentControl :: Lens' CmafGroupSettings (Maybe CmafSegmentControl)
cgsSegmentControl = lens _cgsSegmentControl (\s a -> s {_cgsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
cgsDestination :: Lens' CmafGroupSettings (Maybe Text)
cgsDestination = lens _cgsDestination (\s a -> s {_cgsDestination = a})

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
cgsMinBufferTime :: Lens' CmafGroupSettings (Maybe Natural)
cgsMinBufferTime = lens _cgsMinBufferTime (\s a -> s {_cgsMinBufferTime = a}) . mapping _Nat

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
cgsMpdProfile :: Lens' CmafGroupSettings (Maybe CmafMpdProfile)
cgsMpdProfile = lens _cgsMpdProfile (\s a -> s {_cgsMpdProfile = a})

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
cgsWriteHlsManifest :: Lens' CmafGroupSettings (Maybe CmafWriteHLSManifest)
cgsWriteHlsManifest = lens _cgsWriteHlsManifest (\s a -> s {_cgsWriteHlsManifest = a})

-- | By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
cgsAdditionalManifests :: Lens' CmafGroupSettings [CmafAdditionalManifest]
cgsAdditionalManifests = lens _cgsAdditionalManifests (\s a -> s {_cgsAdditionalManifests = a}) . _Default . _Coerce

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
cgsCodecSpecification :: Lens' CmafGroupSettings (Maybe CmafCodecSpecification)
cgsCodecSpecification = lens _cgsCodecSpecification (\s a -> s {_cgsCodecSpecification = a})

-- | A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
cgsBaseURL :: Lens' CmafGroupSettings (Maybe Text)
cgsBaseURL = lens _cgsBaseURL (\s a -> s {_cgsBaseURL = a})

-- | Settings associated with the destination. Will vary based on the type of destination
cgsDestinationSettings :: Lens' CmafGroupSettings (Maybe DestinationSettings)
cgsDestinationSettings = lens _cgsDestinationSettings (\s a -> s {_cgsDestinationSettings = a})

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
cgsMinFinalSegmentLength :: Lens' CmafGroupSettings (Maybe Double)
cgsMinFinalSegmentLength = lens _cgsMinFinalSegmentLength (\s a -> s {_cgsMinFinalSegmentLength = a})

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
cgsWriteDashManifest :: Lens' CmafGroupSettings (Maybe CmafWriteDASHManifest)
cgsWriteDashManifest = lens _cgsWriteDashManifest (\s a -> s {_cgsWriteDashManifest = a})

-- | DRM settings.
cgsEncryption :: Lens' CmafGroupSettings (Maybe CmafEncryptionSettings)
cgsEncryption = lens _cgsEncryption (\s a -> s {_cgsEncryption = a})

-- | Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
cgsSegmentLength :: Lens' CmafGroupSettings (Maybe Natural)
cgsSegmentLength = lens _cgsSegmentLength (\s a -> s {_cgsSegmentLength = a}) . mapping _Nat

-- | Indicates whether the output manifest should use floating point values for segment duration.
cgsManifestDurationFormat :: Lens' CmafGroupSettings (Maybe CmafManifestDurationFormat)
cgsManifestDurationFormat = lens _cgsManifestDurationFormat (\s a -> s {_cgsManifestDurationFormat = a})

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
cgsClientCache :: Lens' CmafGroupSettings (Maybe CmafClientCache)
cgsClientCache = lens _cgsClientCache (\s a -> s {_cgsClientCache = a})

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
cgsWriteSegmentTimelineInRepresentation :: Lens' CmafGroupSettings (Maybe CmafWriteSegmentTimelineInRepresentation)
cgsWriteSegmentTimelineInRepresentation = lens _cgsWriteSegmentTimelineInRepresentation (\s a -> s {_cgsWriteSegmentTimelineInRepresentation = a})

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
cgsStreamInfResolution :: Lens' CmafGroupSettings (Maybe CmafStreamInfResolution)
cgsStreamInfResolution = lens _cgsStreamInfResolution (\s a -> s {_cgsStreamInfResolution = a})

-- | When set to GZIP, compresses HLS playlist.
cgsManifestCompression :: Lens' CmafGroupSettings (Maybe CmafManifestCompression)
cgsManifestCompression = lens _cgsManifestCompression (\s a -> s {_cgsManifestCompression = a})

instance FromJSON CmafGroupSettings where
  parseJSON =
    withObject
      "CmafGroupSettings"
      ( \x ->
          CmafGroupSettings'
            <$> (x .:? "fragmentLength")
            <*> (x .:? "segmentControl")
            <*> (x .:? "destination")
            <*> (x .:? "minBufferTime")
            <*> (x .:? "mpdProfile")
            <*> (x .:? "writeHlsManifest")
            <*> (x .:? "additionalManifests" .!= mempty)
            <*> (x .:? "codecSpecification")
            <*> (x .:? "baseUrl")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "minFinalSegmentLength")
            <*> (x .:? "writeDashManifest")
            <*> (x .:? "encryption")
            <*> (x .:? "segmentLength")
            <*> (x .:? "manifestDurationFormat")
            <*> (x .:? "clientCache")
            <*> (x .:? "writeSegmentTimelineInRepresentation")
            <*> (x .:? "streamInfResolution")
            <*> (x .:? "manifestCompression")
      )

instance Hashable CmafGroupSettings

instance NFData CmafGroupSettings

instance ToJSON CmafGroupSettings where
  toJSON CmafGroupSettings' {..} =
    object
      ( catMaybes
          [ ("fragmentLength" .=) <$> _cgsFragmentLength,
            ("segmentControl" .=) <$> _cgsSegmentControl,
            ("destination" .=) <$> _cgsDestination,
            ("minBufferTime" .=) <$> _cgsMinBufferTime,
            ("mpdProfile" .=) <$> _cgsMpdProfile,
            ("writeHlsManifest" .=) <$> _cgsWriteHlsManifest,
            ("additionalManifests" .=) <$> _cgsAdditionalManifests,
            ("codecSpecification" .=) <$> _cgsCodecSpecification,
            ("baseUrl" .=) <$> _cgsBaseURL,
            ("destinationSettings" .=) <$> _cgsDestinationSettings,
            ("minFinalSegmentLength" .=) <$> _cgsMinFinalSegmentLength,
            ("writeDashManifest" .=) <$> _cgsWriteDashManifest,
            ("encryption" .=) <$> _cgsEncryption,
            ("segmentLength" .=) <$> _cgsSegmentLength,
            ("manifestDurationFormat" .=) <$> _cgsManifestDurationFormat,
            ("clientCache" .=) <$> _cgsClientCache,
            ("writeSegmentTimelineInRepresentation" .=)
              <$> _cgsWriteSegmentTimelineInRepresentation,
            ("streamInfResolution" .=) <$> _cgsStreamInfResolution,
            ("manifestCompression" .=) <$> _cgsManifestCompression
          ]
      )

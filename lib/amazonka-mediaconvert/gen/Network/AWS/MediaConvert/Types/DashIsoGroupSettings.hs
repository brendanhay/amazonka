{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'dashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { _digsFragmentLength ::
      !(Maybe Nat),
    _digsSegmentControl ::
      !(Maybe DashIsoSegmentControl),
    _digsDestination :: !(Maybe Text),
    _digsHbbtvCompliance ::
      !(Maybe DashIsoHbbtvCompliance),
    _digsMinBufferTime :: !(Maybe Nat),
    _digsMpdProfile :: !(Maybe DashIsoMpdProfile),
    _digsAdditionalManifests ::
      !(Maybe [DashAdditionalManifest]),
    _digsBaseURL :: !(Maybe Text),
    _digsDestinationSettings ::
      !(Maybe DestinationSettings),
    _digsMinFinalSegmentLength :: !(Maybe Double),
    _digsEncryption ::
      !(Maybe DashIsoEncryptionSettings),
    _digsSegmentLength :: !(Maybe Nat),
    _digsWriteSegmentTimelineInRepresentation ::
      !( Maybe
           DashIsoWriteSegmentTimelineInRepresentation
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashIsoGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digsFragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- * 'digsSegmentControl' - When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- * 'digsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'digsHbbtvCompliance' - Supports HbbTV specification as indicated
--
-- * 'digsMinBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- * 'digsMpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- * 'digsAdditionalManifests' - By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- * 'digsBaseURL' - A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- * 'digsDestinationSettings' - Settings associated with the destination. Will vary based on the type of destination
--
-- * 'digsMinFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- * 'digsEncryption' - DRM settings.
--
-- * 'digsSegmentLength' - Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- * 'digsWriteSegmentTimelineInRepresentation' - If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
dashIsoGroupSettings ::
  DashIsoGroupSettings
dashIsoGroupSettings =
  DashIsoGroupSettings'
    { _digsFragmentLength = Nothing,
      _digsSegmentControl = Nothing,
      _digsDestination = Nothing,
      _digsHbbtvCompliance = Nothing,
      _digsMinBufferTime = Nothing,
      _digsMpdProfile = Nothing,
      _digsAdditionalManifests = Nothing,
      _digsBaseURL = Nothing,
      _digsDestinationSettings = Nothing,
      _digsMinFinalSegmentLength = Nothing,
      _digsEncryption = Nothing,
      _digsSegmentLength = Nothing,
      _digsWriteSegmentTimelineInRepresentation = Nothing
    }

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsFragmentLength :: Lens' DashIsoGroupSettings (Maybe Natural)
digsFragmentLength = lens _digsFragmentLength (\s a -> s {_digsFragmentLength = a}) . mapping _Nat

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
digsSegmentControl :: Lens' DashIsoGroupSettings (Maybe DashIsoSegmentControl)
digsSegmentControl = lens _digsSegmentControl (\s a -> s {_digsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
digsDestination :: Lens' DashIsoGroupSettings (Maybe Text)
digsDestination = lens _digsDestination (\s a -> s {_digsDestination = a})

-- | Supports HbbTV specification as indicated
digsHbbtvCompliance :: Lens' DashIsoGroupSettings (Maybe DashIsoHbbtvCompliance)
digsHbbtvCompliance = lens _digsHbbtvCompliance (\s a -> s {_digsHbbtvCompliance = a})

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
digsMinBufferTime :: Lens' DashIsoGroupSettings (Maybe Natural)
digsMinBufferTime = lens _digsMinBufferTime (\s a -> s {_digsMinBufferTime = a}) . mapping _Nat

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
digsMpdProfile :: Lens' DashIsoGroupSettings (Maybe DashIsoMpdProfile)
digsMpdProfile = lens _digsMpdProfile (\s a -> s {_digsMpdProfile = a})

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
digsAdditionalManifests :: Lens' DashIsoGroupSettings [DashAdditionalManifest]
digsAdditionalManifests = lens _digsAdditionalManifests (\s a -> s {_digsAdditionalManifests = a}) . _Default . _Coerce

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
digsBaseURL :: Lens' DashIsoGroupSettings (Maybe Text)
digsBaseURL = lens _digsBaseURL (\s a -> s {_digsBaseURL = a})

-- | Settings associated with the destination. Will vary based on the type of destination
digsDestinationSettings :: Lens' DashIsoGroupSettings (Maybe DestinationSettings)
digsDestinationSettings = lens _digsDestinationSettings (\s a -> s {_digsDestinationSettings = a})

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
digsMinFinalSegmentLength :: Lens' DashIsoGroupSettings (Maybe Double)
digsMinFinalSegmentLength = lens _digsMinFinalSegmentLength (\s a -> s {_digsMinFinalSegmentLength = a})

-- | DRM settings.
digsEncryption :: Lens' DashIsoGroupSettings (Maybe DashIsoEncryptionSettings)
digsEncryption = lens _digsEncryption (\s a -> s {_digsEncryption = a})

-- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsSegmentLength :: Lens' DashIsoGroupSettings (Maybe Natural)
digsSegmentLength = lens _digsSegmentLength (\s a -> s {_digsSegmentLength = a}) . mapping _Nat

-- | If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
digsWriteSegmentTimelineInRepresentation :: Lens' DashIsoGroupSettings (Maybe DashIsoWriteSegmentTimelineInRepresentation)
digsWriteSegmentTimelineInRepresentation = lens _digsWriteSegmentTimelineInRepresentation (\s a -> s {_digsWriteSegmentTimelineInRepresentation = a})

instance FromJSON DashIsoGroupSettings where
  parseJSON =
    withObject
      "DashIsoGroupSettings"
      ( \x ->
          DashIsoGroupSettings'
            <$> (x .:? "fragmentLength")
            <*> (x .:? "segmentControl")
            <*> (x .:? "destination")
            <*> (x .:? "hbbtvCompliance")
            <*> (x .:? "minBufferTime")
            <*> (x .:? "mpdProfile")
            <*> (x .:? "additionalManifests" .!= mempty)
            <*> (x .:? "baseUrl")
            <*> (x .:? "destinationSettings")
            <*> (x .:? "minFinalSegmentLength")
            <*> (x .:? "encryption")
            <*> (x .:? "segmentLength")
            <*> (x .:? "writeSegmentTimelineInRepresentation")
      )

instance Hashable DashIsoGroupSettings

instance NFData DashIsoGroupSettings

instance ToJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings' {..} =
    object
      ( catMaybes
          [ ("fragmentLength" .=) <$> _digsFragmentLength,
            ("segmentControl" .=) <$> _digsSegmentControl,
            ("destination" .=) <$> _digsDestination,
            ("hbbtvCompliance" .=) <$> _digsHbbtvCompliance,
            ("minBufferTime" .=) <$> _digsMinBufferTime,
            ("mpdProfile" .=) <$> _digsMpdProfile,
            ("additionalManifests" .=) <$> _digsAdditionalManifests,
            ("baseUrl" .=) <$> _digsBaseURL,
            ("destinationSettings" .=) <$> _digsDestinationSettings,
            ("minFinalSegmentLength" .=) <$> _digsMinFinalSegmentLength,
            ("encryption" .=) <$> _digsEncryption,
            ("segmentLength" .=) <$> _digsSegmentLength,
            ("writeSegmentTimelineInRepresentation" .=)
              <$> _digsWriteSegmentTimelineInRepresentation
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
import Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
import Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
import Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
import Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35ReturnToNetworkScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
import Network.AWS.MediaLive.Types.Scte35TimeSignalScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageActivateScheduleActionSettings
import Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
import Network.AWS.Prelude

-- | Holds the settings for a single schedule action.
--
-- /See:/ 'scheduleActionSettings' smart constructor.
data ScheduleActionSettings = ScheduleActionSettings'
  { _sasStaticImageDeactivateSettings ::
      !( Maybe
           StaticImageDeactivateScheduleActionSettings
       ),
    _sasScte35SpliceInsertSettings ::
      !( Maybe
           Scte35SpliceInsertScheduleActionSettings
       ),
    _sasStaticImageActivateSettings ::
      !( Maybe
           StaticImageActivateScheduleActionSettings
       ),
    _sasScte35TimeSignalSettings ::
      !( Maybe
           Scte35TimeSignalScheduleActionSettings
       ),
    _sasInputPrepareSettings ::
      !(Maybe InputPrepareScheduleActionSettings),
    _sasHlsId3SegmentTaggingSettings ::
      !( Maybe
           HlsId3SegmentTaggingScheduleActionSettings
       ),
    _sasScte35ReturnToNetworkSettings ::
      !( Maybe
           Scte35ReturnToNetworkScheduleActionSettings
       ),
    _sasPauseStateSettings ::
      !(Maybe PauseStateScheduleActionSettings),
    _sasHlsTimedMetadataSettings ::
      !( Maybe
           HlsTimedMetadataScheduleActionSettings
       ),
    _sasInputSwitchSettings ::
      !(Maybe InputSwitchScheduleActionSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasStaticImageDeactivateSettings' - Action to deactivate a static image overlay
--
-- * 'sasScte35SpliceInsertSettings' - Action to insert SCTE-35 splice_insert message
--
-- * 'sasStaticImageActivateSettings' - Action to activate a static image overlay
--
-- * 'sasScte35TimeSignalSettings' - Action to insert SCTE-35 time_signal message
--
-- * 'sasInputPrepareSettings' - Action to prepare an input for a future immediate input switch
--
-- * 'sasHlsId3SegmentTaggingSettings' - Action to insert HLS ID3 segment tagging
--
-- * 'sasScte35ReturnToNetworkSettings' - Action to insert SCTE-35 return_to_network message
--
-- * 'sasPauseStateSettings' - Action to pause or unpause one or both channel pipelines
--
-- * 'sasHlsTimedMetadataSettings' - Action to insert HLS metadata
--
-- * 'sasInputSwitchSettings' - Action to switch the input
scheduleActionSettings ::
  ScheduleActionSettings
scheduleActionSettings =
  ScheduleActionSettings'
    { _sasStaticImageDeactivateSettings =
        Nothing,
      _sasScte35SpliceInsertSettings = Nothing,
      _sasStaticImageActivateSettings = Nothing,
      _sasScte35TimeSignalSettings = Nothing,
      _sasInputPrepareSettings = Nothing,
      _sasHlsId3SegmentTaggingSettings = Nothing,
      _sasScte35ReturnToNetworkSettings = Nothing,
      _sasPauseStateSettings = Nothing,
      _sasHlsTimedMetadataSettings = Nothing,
      _sasInputSwitchSettings = Nothing
    }

-- | Action to deactivate a static image overlay
sasStaticImageDeactivateSettings :: Lens' ScheduleActionSettings (Maybe StaticImageDeactivateScheduleActionSettings)
sasStaticImageDeactivateSettings = lens _sasStaticImageDeactivateSettings (\s a -> s {_sasStaticImageDeactivateSettings = a})

-- | Action to insert SCTE-35 splice_insert message
sasScte35SpliceInsertSettings :: Lens' ScheduleActionSettings (Maybe Scte35SpliceInsertScheduleActionSettings)
sasScte35SpliceInsertSettings = lens _sasScte35SpliceInsertSettings (\s a -> s {_sasScte35SpliceInsertSettings = a})

-- | Action to activate a static image overlay
sasStaticImageActivateSettings :: Lens' ScheduleActionSettings (Maybe StaticImageActivateScheduleActionSettings)
sasStaticImageActivateSettings = lens _sasStaticImageActivateSettings (\s a -> s {_sasStaticImageActivateSettings = a})

-- | Action to insert SCTE-35 time_signal message
sasScte35TimeSignalSettings :: Lens' ScheduleActionSettings (Maybe Scte35TimeSignalScheduleActionSettings)
sasScte35TimeSignalSettings = lens _sasScte35TimeSignalSettings (\s a -> s {_sasScte35TimeSignalSettings = a})

-- | Action to prepare an input for a future immediate input switch
sasInputPrepareSettings :: Lens' ScheduleActionSettings (Maybe InputPrepareScheduleActionSettings)
sasInputPrepareSettings = lens _sasInputPrepareSettings (\s a -> s {_sasInputPrepareSettings = a})

-- | Action to insert HLS ID3 segment tagging
sasHlsId3SegmentTaggingSettings :: Lens' ScheduleActionSettings (Maybe HlsId3SegmentTaggingScheduleActionSettings)
sasHlsId3SegmentTaggingSettings = lens _sasHlsId3SegmentTaggingSettings (\s a -> s {_sasHlsId3SegmentTaggingSettings = a})

-- | Action to insert SCTE-35 return_to_network message
sasScte35ReturnToNetworkSettings :: Lens' ScheduleActionSettings (Maybe Scte35ReturnToNetworkScheduleActionSettings)
sasScte35ReturnToNetworkSettings = lens _sasScte35ReturnToNetworkSettings (\s a -> s {_sasScte35ReturnToNetworkSettings = a})

-- | Action to pause or unpause one or both channel pipelines
sasPauseStateSettings :: Lens' ScheduleActionSettings (Maybe PauseStateScheduleActionSettings)
sasPauseStateSettings = lens _sasPauseStateSettings (\s a -> s {_sasPauseStateSettings = a})

-- | Action to insert HLS metadata
sasHlsTimedMetadataSettings :: Lens' ScheduleActionSettings (Maybe HlsTimedMetadataScheduleActionSettings)
sasHlsTimedMetadataSettings = lens _sasHlsTimedMetadataSettings (\s a -> s {_sasHlsTimedMetadataSettings = a})

-- | Action to switch the input
sasInputSwitchSettings :: Lens' ScheduleActionSettings (Maybe InputSwitchScheduleActionSettings)
sasInputSwitchSettings = lens _sasInputSwitchSettings (\s a -> s {_sasInputSwitchSettings = a})

instance FromJSON ScheduleActionSettings where
  parseJSON =
    withObject
      "ScheduleActionSettings"
      ( \x ->
          ScheduleActionSettings'
            <$> (x .:? "staticImageDeactivateSettings")
            <*> (x .:? "scte35SpliceInsertSettings")
            <*> (x .:? "staticImageActivateSettings")
            <*> (x .:? "scte35TimeSignalSettings")
            <*> (x .:? "inputPrepareSettings")
            <*> (x .:? "hlsId3SegmentTaggingSettings")
            <*> (x .:? "scte35ReturnToNetworkSettings")
            <*> (x .:? "pauseStateSettings")
            <*> (x .:? "hlsTimedMetadataSettings")
            <*> (x .:? "inputSwitchSettings")
      )

instance Hashable ScheduleActionSettings

instance NFData ScheduleActionSettings

instance ToJSON ScheduleActionSettings where
  toJSON ScheduleActionSettings' {..} =
    object
      ( catMaybes
          [ ("staticImageDeactivateSettings" .=)
              <$> _sasStaticImageDeactivateSettings,
            ("scte35SpliceInsertSettings" .=)
              <$> _sasScte35SpliceInsertSettings,
            ("staticImageActivateSettings" .=)
              <$> _sasStaticImageActivateSettings,
            ("scte35TimeSignalSettings" .=) <$> _sasScte35TimeSignalSettings,
            ("inputPrepareSettings" .=) <$> _sasInputPrepareSettings,
            ("hlsId3SegmentTaggingSettings" .=)
              <$> _sasHlsId3SegmentTaggingSettings,
            ("scte35ReturnToNetworkSettings" .=)
              <$> _sasScte35ReturnToNetworkSettings,
            ("pauseStateSettings" .=) <$> _sasPauseStateSettings,
            ("hlsTimedMetadataSettings" .=) <$> _sasHlsTimedMetadataSettings,
            ("inputSwitchSettings" .=) <$> _sasInputSwitchSettings
          ]
      )

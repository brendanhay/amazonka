{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M3u8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.M3u8NielsenId3Behavior
import Network.AWS.MediaLive.Types.M3u8PcrControl
import Network.AWS.MediaLive.Types.M3u8Scte35Behavior
import Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
import Network.AWS.Prelude

-- | Settings information for the .m3u8 container
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { _mssPmtPid :: !(Maybe Text),
    _mssVideoPid :: !(Maybe Text),
    _mssNielsenId3Behavior :: !(Maybe M3u8NielsenId3Behavior),
    _mssScte35Pid :: !(Maybe Text),
    _mssTransportStreamId :: !(Maybe Nat),
    _mssProgramNum :: !(Maybe Nat),
    _mssTimedMetadataBehavior :: !(Maybe M3u8TimedMetadataBehavior),
    _mssPmtInterval :: !(Maybe Nat),
    _mssEcmPid :: !(Maybe Text),
    _mssTimedMetadataPid :: !(Maybe Text),
    _mssAudioFramesPerPes :: !(Maybe Nat),
    _mssPcrPeriod :: !(Maybe Nat),
    _mssPcrPid :: !(Maybe Text),
    _mssPatInterval :: !(Maybe Nat),
    _mssAudioPids :: !(Maybe Text),
    _mssScte35Behavior :: !(Maybe M3u8Scte35Behavior),
    _mssPcrControl :: !(Maybe M3u8PcrControl)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mssVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mssNielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'mssScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
--
-- * 'mssTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'mssProgramNum' - The value of the program number field in the Program Map Table.
--
-- * 'mssTimedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
--
-- * 'mssPmtInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mssEcmPid' - This parameter is unused and deprecated.
--
-- * 'mssTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
--
-- * 'mssAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mssPcrPeriod' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mssPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
--
-- * 'mssPatInterval' - The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
--
-- * 'mssAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
--
-- * 'mssScte35Behavior' - If set to passthrough, passes any SCTE-35 signals from the input source to this output.
--
-- * 'mssPcrControl' - When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
m3u8Settings ::
  M3u8Settings
m3u8Settings =
  M3u8Settings'
    { _mssPmtPid = Nothing,
      _mssVideoPid = Nothing,
      _mssNielsenId3Behavior = Nothing,
      _mssScte35Pid = Nothing,
      _mssTransportStreamId = Nothing,
      _mssProgramNum = Nothing,
      _mssTimedMetadataBehavior = Nothing,
      _mssPmtInterval = Nothing,
      _mssEcmPid = Nothing,
      _mssTimedMetadataPid = Nothing,
      _mssAudioFramesPerPes = Nothing,
      _mssPcrPeriod = Nothing,
      _mssPcrPid = Nothing,
      _mssPatInterval = Nothing,
      _mssAudioPids = Nothing,
      _mssScte35Behavior = Nothing,
      _mssPcrControl = Nothing
    }

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream. Can be entered as a decimal or hexadecimal value.
mssPmtPid :: Lens' M3u8Settings (Maybe Text)
mssPmtPid = lens _mssPmtPid (\s a -> s {_mssPmtPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mssVideoPid :: Lens' M3u8Settings (Maybe Text)
mssVideoPid = lens _mssVideoPid (\s a -> s {_mssVideoPid = a})

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
mssNielsenId3Behavior :: Lens' M3u8Settings (Maybe M3u8NielsenId3Behavior)
mssNielsenId3Behavior = lens _mssNielsenId3Behavior (\s a -> s {_mssNielsenId3Behavior = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream. Can be entered as a decimal or hexadecimal value.
mssScte35Pid :: Lens' M3u8Settings (Maybe Text)
mssScte35Pid = lens _mssScte35Pid (\s a -> s {_mssScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
mssTransportStreamId :: Lens' M3u8Settings (Maybe Natural)
mssTransportStreamId = lens _mssTransportStreamId (\s a -> s {_mssTransportStreamId = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
mssProgramNum :: Lens' M3u8Settings (Maybe Natural)
mssProgramNum = lens _mssProgramNum (\s a -> s {_mssProgramNum = a}) . mapping _Nat

-- | When set to passthrough, timed metadata is passed through from input to output.
mssTimedMetadataBehavior :: Lens' M3u8Settings (Maybe M3u8TimedMetadataBehavior)
mssTimedMetadataBehavior = lens _mssTimedMetadataBehavior (\s a -> s {_mssTimedMetadataBehavior = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mssPmtInterval :: Lens' M3u8Settings (Maybe Natural)
mssPmtInterval = lens _mssPmtInterval (\s a -> s {_mssPmtInterval = a}) . mapping _Nat

-- | This parameter is unused and deprecated.
mssEcmPid :: Lens' M3u8Settings (Maybe Text)
mssEcmPid = lens _mssEcmPid (\s a -> s {_mssEcmPid = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream. Can be entered as a decimal or hexadecimal value.  Valid values are 32 (or 0x20)..8182 (or 0x1ff6).
mssTimedMetadataPid :: Lens' M3u8Settings (Maybe Text)
mssTimedMetadataPid = lens _mssTimedMetadataPid (\s a -> s {_mssTimedMetadataPid = a})

-- | The number of audio frames to insert for each PES packet.
mssAudioFramesPerPes :: Lens' M3u8Settings (Maybe Natural)
mssAudioFramesPerPes = lens _mssAudioFramesPerPes (\s a -> s {_mssAudioFramesPerPes = a}) . mapping _Nat

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
mssPcrPeriod :: Lens' M3u8Settings (Maybe Natural)
mssPcrPeriod = lens _mssPcrPeriod (\s a -> s {_mssPcrPeriod = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID. Can be entered as a decimal or hexadecimal value.
mssPcrPid :: Lens' M3u8Settings (Maybe Text)
mssPcrPid = lens _mssPcrPid (\s a -> s {_mssPcrPid = a})

-- | The number of milliseconds between instances of this table in the output transport stream. A value of \"0\" writes out the PMT once per segment file.
mssPatInterval :: Lens' M3u8Settings (Maybe Natural)
mssPatInterval = lens _mssPatInterval (\s a -> s {_mssPatInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation. Can be entered as decimal or hexadecimal values.
mssAudioPids :: Lens' M3u8Settings (Maybe Text)
mssAudioPids = lens _mssAudioPids (\s a -> s {_mssAudioPids = a})

-- | If set to passthrough, passes any SCTE-35 signals from the input source to this output.
mssScte35Behavior :: Lens' M3u8Settings (Maybe M3u8Scte35Behavior)
mssScte35Behavior = lens _mssScte35Behavior (\s a -> s {_mssScte35Behavior = a})

-- | When set to pcrEveryPesPacket, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
mssPcrControl :: Lens' M3u8Settings (Maybe M3u8PcrControl)
mssPcrControl = lens _mssPcrControl (\s a -> s {_mssPcrControl = a})

instance FromJSON M3u8Settings where
  parseJSON =
    withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            <$> (x .:? "pmtPid")
            <*> (x .:? "videoPid")
            <*> (x .:? "nielsenId3Behavior")
            <*> (x .:? "scte35Pid")
            <*> (x .:? "transportStreamId")
            <*> (x .:? "programNum")
            <*> (x .:? "timedMetadataBehavior")
            <*> (x .:? "pmtInterval")
            <*> (x .:? "ecmPid")
            <*> (x .:? "timedMetadataPid")
            <*> (x .:? "audioFramesPerPes")
            <*> (x .:? "pcrPeriod")
            <*> (x .:? "pcrPid")
            <*> (x .:? "patInterval")
            <*> (x .:? "audioPids")
            <*> (x .:? "scte35Behavior")
            <*> (x .:? "pcrControl")
      )

instance Hashable M3u8Settings

instance NFData M3u8Settings

instance ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    object
      ( catMaybes
          [ ("pmtPid" .=) <$> _mssPmtPid,
            ("videoPid" .=) <$> _mssVideoPid,
            ("nielsenId3Behavior" .=) <$> _mssNielsenId3Behavior,
            ("scte35Pid" .=) <$> _mssScte35Pid,
            ("transportStreamId" .=) <$> _mssTransportStreamId,
            ("programNum" .=) <$> _mssProgramNum,
            ("timedMetadataBehavior" .=) <$> _mssTimedMetadataBehavior,
            ("pmtInterval" .=) <$> _mssPmtInterval,
            ("ecmPid" .=) <$> _mssEcmPid,
            ("timedMetadataPid" .=) <$> _mssTimedMetadataPid,
            ("audioFramesPerPes" .=) <$> _mssAudioFramesPerPes,
            ("pcrPeriod" .=) <$> _mssPcrPeriod,
            ("pcrPid" .=) <$> _mssPcrPid,
            ("patInterval" .=) <$> _mssPatInterval,
            ("audioPids" .=) <$> _mssAudioPids,
            ("scte35Behavior" .=) <$> _mssScte35Behavior,
            ("pcrControl" .=) <$> _mssPcrControl
          ]
      )

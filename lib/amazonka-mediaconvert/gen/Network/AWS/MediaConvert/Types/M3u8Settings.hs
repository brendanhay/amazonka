{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.M3u8AudioDuration
import Network.AWS.MediaConvert.Types.M3u8NielsenId3
import Network.AWS.MediaConvert.Types.M3u8PcrControl
import Network.AWS.MediaConvert.Types.M3u8Scte35Source
import Network.AWS.MediaConvert.Types.TimedMetadata
import Network.AWS.Prelude

-- | Settings for TS segments in HLS
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { _msPmtPid :: !(Maybe Nat),
    _msVideoPid :: !(Maybe Nat),
    _msProgramNumber :: !(Maybe Nat),
    _msScte35Pid :: !(Maybe Nat),
    _msTransportStreamId :: !(Maybe Nat),
    _msPrivateMetadataPid :: !(Maybe Nat),
    _msAudioDuration :: !(Maybe M3u8AudioDuration),
    _msPmtInterval :: !(Maybe Nat),
    _msTimedMetadataPid :: !(Maybe Nat),
    _msAudioFramesPerPes :: !(Maybe Nat),
    _msPcrPid :: !(Maybe Nat),
    _msTimedMetadata :: !(Maybe TimedMetadata),
    _msScte35Source :: !(Maybe M3u8Scte35Source),
    _msPatInterval :: !(Maybe Nat),
    _msAudioPids :: !(Maybe [Nat]),
    _msNielsenId3 :: !(Maybe M3u8NielsenId3),
    _msPcrControl :: !(Maybe M3u8PcrControl)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
--
-- * 'msVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream.
--
-- * 'msProgramNumber' - The value of the program number field in the Program Map Table.
--
-- * 'msScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- * 'msTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'msPrivateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport stream.
--
-- * 'msAudioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- * 'msPmtInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'msTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream.
--
-- * 'msAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'msPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
--
-- * 'msTimedMetadata' - Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
--
-- * 'msScte35Source' - For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
--
-- * 'msPatInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'msAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- * 'msNielsenId3' - If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- * 'msPcrControl' - When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
m3u8Settings ::
  M3u8Settings
m3u8Settings =
  M3u8Settings'
    { _msPmtPid = Nothing,
      _msVideoPid = Nothing,
      _msProgramNumber = Nothing,
      _msScte35Pid = Nothing,
      _msTransportStreamId = Nothing,
      _msPrivateMetadataPid = Nothing,
      _msAudioDuration = Nothing,
      _msPmtInterval = Nothing,
      _msTimedMetadataPid = Nothing,
      _msAudioFramesPerPes = Nothing,
      _msPcrPid = Nothing,
      _msTimedMetadata = Nothing,
      _msScte35Source = Nothing,
      _msPatInterval = Nothing,
      _msAudioPids = Nothing,
      _msNielsenId3 = Nothing,
      _msPcrControl = Nothing
    }

-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
msPmtPid :: Lens' M3u8Settings (Maybe Natural)
msPmtPid = lens _msPmtPid (\s a -> s {_msPmtPid = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
msVideoPid :: Lens' M3u8Settings (Maybe Natural)
msVideoPid = lens _msVideoPid (\s a -> s {_msVideoPid = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
msProgramNumber :: Lens' M3u8Settings (Maybe Natural)
msProgramNumber = lens _msProgramNumber (\s a -> s {_msProgramNumber = a}) . mapping _Nat

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
msScte35Pid :: Lens' M3u8Settings (Maybe Natural)
msScte35Pid = lens _msScte35Pid (\s a -> s {_msScte35Pid = a}) . mapping _Nat

-- | The value of the transport stream ID field in the Program Map Table.
msTransportStreamId :: Lens' M3u8Settings (Maybe Natural)
msTransportStreamId = lens _msTransportStreamId (\s a -> s {_msTransportStreamId = a}) . mapping _Nat

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
msPrivateMetadataPid :: Lens' M3u8Settings (Maybe Natural)
msPrivateMetadataPid = lens _msPrivateMetadataPid (\s a -> s {_msPrivateMetadataPid = a}) . mapping _Nat

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
msAudioDuration :: Lens' M3u8Settings (Maybe M3u8AudioDuration)
msAudioDuration = lens _msAudioDuration (\s a -> s {_msAudioDuration = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
msPmtInterval :: Lens' M3u8Settings (Maybe Natural)
msPmtInterval = lens _msPmtInterval (\s a -> s {_msPmtInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
msTimedMetadataPid :: Lens' M3u8Settings (Maybe Natural)
msTimedMetadataPid = lens _msTimedMetadataPid (\s a -> s {_msTimedMetadataPid = a}) . mapping _Nat

-- | The number of audio frames to insert for each PES packet.
msAudioFramesPerPes :: Lens' M3u8Settings (Maybe Natural)
msAudioFramesPerPes = lens _msAudioFramesPerPes (\s a -> s {_msAudioFramesPerPes = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
msPcrPid :: Lens' M3u8Settings (Maybe Natural)
msPcrPid = lens _msPcrPid (\s a -> s {_msPcrPid = a}) . mapping _Nat

-- | Applies only to HLS outputs. Use this setting to specify whether the service inserts the ID3 timed metadata from the input in this output.
msTimedMetadata :: Lens' M3u8Settings (Maybe TimedMetadata)
msTimedMetadata = lens _msTimedMetadata (\s a -> s {_msTimedMetadata = a})

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE) if you don't want manifest conditioning. Choose Passthrough (PASSTHROUGH) and choose Ad markers (adMarkers) if you do want manifest conditioning. In both cases, also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml).
msScte35Source :: Lens' M3u8Settings (Maybe M3u8Scte35Source)
msScte35Source = lens _msScte35Source (\s a -> s {_msScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
msPatInterval :: Lens' M3u8Settings (Maybe Natural)
msPatInterval = lens _msPatInterval (\s a -> s {_msPatInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
msAudioPids :: Lens' M3u8Settings [Natural]
msAudioPids = lens _msAudioPids (\s a -> s {_msAudioPids = a}) . _Default . _Coerce

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
msNielsenId3 :: Lens' M3u8Settings (Maybe M3u8NielsenId3)
msNielsenId3 = lens _msNielsenId3 (\s a -> s {_msNielsenId3 = a})

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
msPcrControl :: Lens' M3u8Settings (Maybe M3u8PcrControl)
msPcrControl = lens _msPcrControl (\s a -> s {_msPcrControl = a})

instance FromJSON M3u8Settings where
  parseJSON =
    withObject
      "M3u8Settings"
      ( \x ->
          M3u8Settings'
            <$> (x .:? "pmtPid")
            <*> (x .:? "videoPid")
            <*> (x .:? "programNumber")
            <*> (x .:? "scte35Pid")
            <*> (x .:? "transportStreamId")
            <*> (x .:? "privateMetadataPid")
            <*> (x .:? "audioDuration")
            <*> (x .:? "pmtInterval")
            <*> (x .:? "timedMetadataPid")
            <*> (x .:? "audioFramesPerPes")
            <*> (x .:? "pcrPid")
            <*> (x .:? "timedMetadata")
            <*> (x .:? "scte35Source")
            <*> (x .:? "patInterval")
            <*> (x .:? "audioPids" .!= mempty)
            <*> (x .:? "nielsenId3")
            <*> (x .:? "pcrControl")
      )

instance Hashable M3u8Settings

instance NFData M3u8Settings

instance ToJSON M3u8Settings where
  toJSON M3u8Settings' {..} =
    object
      ( catMaybes
          [ ("pmtPid" .=) <$> _msPmtPid,
            ("videoPid" .=) <$> _msVideoPid,
            ("programNumber" .=) <$> _msProgramNumber,
            ("scte35Pid" .=) <$> _msScte35Pid,
            ("transportStreamId" .=) <$> _msTransportStreamId,
            ("privateMetadataPid" .=) <$> _msPrivateMetadataPid,
            ("audioDuration" .=) <$> _msAudioDuration,
            ("pmtInterval" .=) <$> _msPmtInterval,
            ("timedMetadataPid" .=) <$> _msTimedMetadataPid,
            ("audioFramesPerPes" .=) <$> _msAudioFramesPerPes,
            ("pcrPid" .=) <$> _msPcrPid,
            ("timedMetadata" .=) <$> _msTimedMetadata,
            ("scte35Source" .=) <$> _msScte35Source,
            ("patInterval" .=) <$> _msPatInterval,
            ("audioPids" .=) <$> _msAudioPids,
            ("nielsenId3" .=) <$> _msNielsenId3,
            ("pcrControl" .=) <$> _msPcrControl
          ]
      )

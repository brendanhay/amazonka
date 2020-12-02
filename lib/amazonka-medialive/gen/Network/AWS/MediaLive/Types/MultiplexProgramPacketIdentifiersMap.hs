{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Packet identifiers map for a given Multiplex program.
--
-- /See:/ 'multiplexProgramPacketIdentifiersMap' smart constructor.
data MultiplexProgramPacketIdentifiersMap = MultiplexProgramPacketIdentifiersMap'
  { _mppimPmtPid ::
      !(Maybe Int),
    _mppimEtvSignalPid ::
      !(Maybe Int),
    _mppimVideoPid ::
      !(Maybe Int),
    _mppimScte35Pid ::
      !(Maybe Int),
    _mppimPrivateMetadataPid ::
      !(Maybe Int),
    _mppimTimedMetadataPid ::
      !(Maybe Int),
    _mppimPcrPid ::
      !(Maybe Int),
    _mppimKlvDataPids ::
      !(Maybe [Int]),
    _mppimDvbSubPids ::
      !(Maybe [Int]),
    _mppimScte27Pids ::
      !(Maybe [Int]),
    _mppimEtvPlatformPid ::
      !(Maybe Int),
    _mppimAudioPids ::
      !(Maybe [Int]),
    _mppimDvbTeletextPid ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgramPacketIdentifiersMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mppimPmtPid' - Undocumented member.
--
-- * 'mppimEtvSignalPid' - Undocumented member.
--
-- * 'mppimVideoPid' - Undocumented member.
--
-- * 'mppimScte35Pid' - Undocumented member.
--
-- * 'mppimPrivateMetadataPid' - Undocumented member.
--
-- * 'mppimTimedMetadataPid' - Undocumented member.
--
-- * 'mppimPcrPid' - Undocumented member.
--
-- * 'mppimKlvDataPids' - Undocumented member.
--
-- * 'mppimDvbSubPids' - Undocumented member.
--
-- * 'mppimScte27Pids' - Undocumented member.
--
-- * 'mppimEtvPlatformPid' - Undocumented member.
--
-- * 'mppimAudioPids' - Undocumented member.
--
-- * 'mppimDvbTeletextPid' - Undocumented member.
multiplexProgramPacketIdentifiersMap ::
  MultiplexProgramPacketIdentifiersMap
multiplexProgramPacketIdentifiersMap =
  MultiplexProgramPacketIdentifiersMap'
    { _mppimPmtPid = Nothing,
      _mppimEtvSignalPid = Nothing,
      _mppimVideoPid = Nothing,
      _mppimScte35Pid = Nothing,
      _mppimPrivateMetadataPid = Nothing,
      _mppimTimedMetadataPid = Nothing,
      _mppimPcrPid = Nothing,
      _mppimKlvDataPids = Nothing,
      _mppimDvbSubPids = Nothing,
      _mppimScte27Pids = Nothing,
      _mppimEtvPlatformPid = Nothing,
      _mppimAudioPids = Nothing,
      _mppimDvbTeletextPid = Nothing
    }

-- | Undocumented member.
mppimPmtPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPmtPid = lens _mppimPmtPid (\s a -> s {_mppimPmtPid = a})

-- | Undocumented member.
mppimEtvSignalPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimEtvSignalPid = lens _mppimEtvSignalPid (\s a -> s {_mppimEtvSignalPid = a})

-- | Undocumented member.
mppimVideoPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimVideoPid = lens _mppimVideoPid (\s a -> s {_mppimVideoPid = a})

-- | Undocumented member.
mppimScte35Pid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimScte35Pid = lens _mppimScte35Pid (\s a -> s {_mppimScte35Pid = a})

-- | Undocumented member.
mppimPrivateMetadataPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPrivateMetadataPid = lens _mppimPrivateMetadataPid (\s a -> s {_mppimPrivateMetadataPid = a})

-- | Undocumented member.
mppimTimedMetadataPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimTimedMetadataPid = lens _mppimTimedMetadataPid (\s a -> s {_mppimTimedMetadataPid = a})

-- | Undocumented member.
mppimPcrPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimPcrPid = lens _mppimPcrPid (\s a -> s {_mppimPcrPid = a})

-- | Undocumented member.
mppimKlvDataPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimKlvDataPids = lens _mppimKlvDataPids (\s a -> s {_mppimKlvDataPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimDvbSubPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimDvbSubPids = lens _mppimDvbSubPids (\s a -> s {_mppimDvbSubPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimScte27Pids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimScte27Pids = lens _mppimScte27Pids (\s a -> s {_mppimScte27Pids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimEtvPlatformPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimEtvPlatformPid = lens _mppimEtvPlatformPid (\s a -> s {_mppimEtvPlatformPid = a})

-- | Undocumented member.
mppimAudioPids :: Lens' MultiplexProgramPacketIdentifiersMap [Int]
mppimAudioPids = lens _mppimAudioPids (\s a -> s {_mppimAudioPids = a}) . _Default . _Coerce

-- | Undocumented member.
mppimDvbTeletextPid :: Lens' MultiplexProgramPacketIdentifiersMap (Maybe Int)
mppimDvbTeletextPid = lens _mppimDvbTeletextPid (\s a -> s {_mppimDvbTeletextPid = a})

instance FromJSON MultiplexProgramPacketIdentifiersMap where
  parseJSON =
    withObject
      "MultiplexProgramPacketIdentifiersMap"
      ( \x ->
          MultiplexProgramPacketIdentifiersMap'
            <$> (x .:? "pmtPid")
            <*> (x .:? "etvSignalPid")
            <*> (x .:? "videoPid")
            <*> (x .:? "scte35Pid")
            <*> (x .:? "privateMetadataPid")
            <*> (x .:? "timedMetadataPid")
            <*> (x .:? "pcrPid")
            <*> (x .:? "klvDataPids" .!= mempty)
            <*> (x .:? "dvbSubPids" .!= mempty)
            <*> (x .:? "scte27Pids" .!= mempty)
            <*> (x .:? "etvPlatformPid")
            <*> (x .:? "audioPids" .!= mempty)
            <*> (x .:? "dvbTeletextPid")
      )

instance Hashable MultiplexProgramPacketIdentifiersMap

instance NFData MultiplexProgramPacketIdentifiersMap

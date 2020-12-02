{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgram where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
import Network.AWS.Prelude

-- | The multiplex program object.
--
-- /See:/ 'multiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'
  { _mpPacketIdentifiersMap ::
      !(Maybe MultiplexProgramPacketIdentifiersMap),
    _mpPipelineDetails ::
      !(Maybe [MultiplexProgramPipelineDetail]),
    _mpProgramName :: !(Maybe Text),
    _mpChannelId :: !(Maybe Text),
    _mpMultiplexProgramSettings ::
      !(Maybe MultiplexProgramSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'mpPipelineDetails' - Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- * 'mpProgramName' - The name of the multiplex program.
--
-- * 'mpChannelId' - The MediaLive channel associated with the program.
--
-- * 'mpMultiplexProgramSettings' - The settings for this multiplex program.
multiplexProgram ::
  MultiplexProgram
multiplexProgram =
  MultiplexProgram'
    { _mpPacketIdentifiersMap = Nothing,
      _mpPipelineDetails = Nothing,
      _mpProgramName = Nothing,
      _mpChannelId = Nothing,
      _mpMultiplexProgramSettings = Nothing
    }

-- | The packet identifier map for this multiplex program.
mpPacketIdentifiersMap :: Lens' MultiplexProgram (Maybe MultiplexProgramPacketIdentifiersMap)
mpPacketIdentifiersMap = lens _mpPacketIdentifiersMap (\s a -> s {_mpPacketIdentifiersMap = a})

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
mpPipelineDetails :: Lens' MultiplexProgram [MultiplexProgramPipelineDetail]
mpPipelineDetails = lens _mpPipelineDetails (\s a -> s {_mpPipelineDetails = a}) . _Default . _Coerce

-- | The name of the multiplex program.
mpProgramName :: Lens' MultiplexProgram (Maybe Text)
mpProgramName = lens _mpProgramName (\s a -> s {_mpProgramName = a})

-- | The MediaLive channel associated with the program.
mpChannelId :: Lens' MultiplexProgram (Maybe Text)
mpChannelId = lens _mpChannelId (\s a -> s {_mpChannelId = a})

-- | The settings for this multiplex program.
mpMultiplexProgramSettings :: Lens' MultiplexProgram (Maybe MultiplexProgramSettings)
mpMultiplexProgramSettings = lens _mpMultiplexProgramSettings (\s a -> s {_mpMultiplexProgramSettings = a})

instance FromJSON MultiplexProgram where
  parseJSON =
    withObject
      "MultiplexProgram"
      ( \x ->
          MultiplexProgram'
            <$> (x .:? "packetIdentifiersMap")
            <*> (x .:? "pipelineDetails" .!= mempty)
            <*> (x .:? "programName")
            <*> (x .:? "channelId")
            <*> (x .:? "multiplexProgramSettings")
      )

instance Hashable MultiplexProgram

instance NFData MultiplexProgram

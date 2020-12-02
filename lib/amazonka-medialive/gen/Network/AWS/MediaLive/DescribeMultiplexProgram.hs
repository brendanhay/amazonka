{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeMultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a program in a multiplex.
module Network.AWS.MediaLive.DescribeMultiplexProgram
  ( -- * Creating a Request
    describeMultiplexProgram,
    DescribeMultiplexProgram,

    -- * Request Lenses
    desMultiplexId,
    desProgramName,

    -- * Destructuring the Response
    describeMultiplexProgramResponse,
    DescribeMultiplexProgramResponse,

    -- * Response Lenses
    dmpmrsPacketIdentifiersMap,
    dmpmrsPipelineDetails,
    dmpmrsProgramName,
    dmpmrsChannelId,
    dmpmrsMultiplexProgramSettings,
    dmpmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'describeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'
  { _desMultiplexId ::
      !Text,
    _desProgramName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desMultiplexId' - The ID of the multiplex that the program belongs to.
--
-- * 'desProgramName' - The name of the program.
describeMultiplexProgram ::
  -- | 'desMultiplexId'
  Text ->
  -- | 'desProgramName'
  Text ->
  DescribeMultiplexProgram
describeMultiplexProgram pMultiplexId_ pProgramName_ =
  DescribeMultiplexProgram'
    { _desMultiplexId = pMultiplexId_,
      _desProgramName = pProgramName_
    }

-- | The ID of the multiplex that the program belongs to.
desMultiplexId :: Lens' DescribeMultiplexProgram Text
desMultiplexId = lens _desMultiplexId (\s a -> s {_desMultiplexId = a})

-- | The name of the program.
desProgramName :: Lens' DescribeMultiplexProgram Text
desProgramName = lens _desProgramName (\s a -> s {_desProgramName = a})

instance AWSRequest DescribeMultiplexProgram where
  type Rs DescribeMultiplexProgram = DescribeMultiplexProgramResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeMultiplexProgramResponse'
            <$> (x .?> "packetIdentifiersMap")
            <*> (x .?> "pipelineDetails" .!@ mempty)
            <*> (x .?> "programName")
            <*> (x .?> "channelId")
            <*> (x .?> "multiplexProgramSettings")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeMultiplexProgram

instance NFData DescribeMultiplexProgram

instance ToHeaders DescribeMultiplexProgram where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeMultiplexProgram where
  toPath DescribeMultiplexProgram' {..} =
    mconcat
      [ "/prod/multiplexes/",
        toBS _desMultiplexId,
        "/programs/",
        toBS _desProgramName
      ]

instance ToQuery DescribeMultiplexProgram where
  toQuery = const mempty

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'describeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'
  { _dmpmrsPacketIdentifiersMap ::
      !( Maybe
           MultiplexProgramPacketIdentifiersMap
       ),
    _dmpmrsPipelineDetails ::
      !( Maybe
           [MultiplexProgramPipelineDetail]
       ),
    _dmpmrsProgramName ::
      !(Maybe Text),
    _dmpmrsChannelId ::
      !(Maybe Text),
    _dmpmrsMultiplexProgramSettings ::
      !( Maybe
           MultiplexProgramSettings
       ),
    _dmpmrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmpmrsPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'dmpmrsPipelineDetails' - Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- * 'dmpmrsProgramName' - The name of the multiplex program.
--
-- * 'dmpmrsChannelId' - The MediaLive channel associated with the program.
--
-- * 'dmpmrsMultiplexProgramSettings' - The settings for this multiplex program.
--
-- * 'dmpmrsResponseStatus' - -- | The response status code.
describeMultiplexProgramResponse ::
  -- | 'dmpmrsResponseStatus'
  Int ->
  DescribeMultiplexProgramResponse
describeMultiplexProgramResponse pResponseStatus_ =
  DescribeMultiplexProgramResponse'
    { _dmpmrsPacketIdentifiersMap =
        Nothing,
      _dmpmrsPipelineDetails = Nothing,
      _dmpmrsProgramName = Nothing,
      _dmpmrsChannelId = Nothing,
      _dmpmrsMultiplexProgramSettings = Nothing,
      _dmpmrsResponseStatus = pResponseStatus_
    }

-- | The packet identifier map for this multiplex program.
dmpmrsPacketIdentifiersMap :: Lens' DescribeMultiplexProgramResponse (Maybe MultiplexProgramPacketIdentifiersMap)
dmpmrsPacketIdentifiersMap = lens _dmpmrsPacketIdentifiersMap (\s a -> s {_dmpmrsPacketIdentifiersMap = a})

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
dmpmrsPipelineDetails :: Lens' DescribeMultiplexProgramResponse [MultiplexProgramPipelineDetail]
dmpmrsPipelineDetails = lens _dmpmrsPipelineDetails (\s a -> s {_dmpmrsPipelineDetails = a}) . _Default . _Coerce

-- | The name of the multiplex program.
dmpmrsProgramName :: Lens' DescribeMultiplexProgramResponse (Maybe Text)
dmpmrsProgramName = lens _dmpmrsProgramName (\s a -> s {_dmpmrsProgramName = a})

-- | The MediaLive channel associated with the program.
dmpmrsChannelId :: Lens' DescribeMultiplexProgramResponse (Maybe Text)
dmpmrsChannelId = lens _dmpmrsChannelId (\s a -> s {_dmpmrsChannelId = a})

-- | The settings for this multiplex program.
dmpmrsMultiplexProgramSettings :: Lens' DescribeMultiplexProgramResponse (Maybe MultiplexProgramSettings)
dmpmrsMultiplexProgramSettings = lens _dmpmrsMultiplexProgramSettings (\s a -> s {_dmpmrsMultiplexProgramSettings = a})

-- | -- | The response status code.
dmpmrsResponseStatus :: Lens' DescribeMultiplexProgramResponse Int
dmpmrsResponseStatus = lens _dmpmrsResponseStatus (\s a -> s {_dmpmrsResponseStatus = a})

instance NFData DescribeMultiplexProgramResponse

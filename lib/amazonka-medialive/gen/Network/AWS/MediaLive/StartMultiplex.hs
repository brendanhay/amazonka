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
-- Module      : Network.AWS.MediaLive.StartMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start (run) the multiplex. Starting the multiplex does not start the channels. You must explicitly start each channel.
module Network.AWS.MediaLive.StartMultiplex
  ( -- * Creating a Request
    startMultiplex,
    StartMultiplex,

    -- * Request Lenses
    sMultiplexId,

    -- * Destructuring the Response
    startMultiplexResponse,
    StartMultiplexResponse,

    -- * Response Lenses
    starsState,
    starsARN,
    starsPipelinesRunningCount,
    starsAvailabilityZones,
    starsProgramCount,
    starsDestinations,
    starsName,
    starsId,
    starsMultiplexSettings,
    starsTags,
    starsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'startMultiplex' smart constructor.
newtype StartMultiplex = StartMultiplex' {_sMultiplexId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMultiplexId' - The ID of the multiplex.
startMultiplex ::
  -- | 'sMultiplexId'
  Text ->
  StartMultiplex
startMultiplex pMultiplexId_ =
  StartMultiplex' {_sMultiplexId = pMultiplexId_}

-- | The ID of the multiplex.
sMultiplexId :: Lens' StartMultiplex Text
sMultiplexId = lens _sMultiplexId (\s a -> s {_sMultiplexId = a})

instance AWSRequest StartMultiplex where
  type Rs StartMultiplex = StartMultiplexResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          StartMultiplexResponse'
            <$> (x .?> "state")
            <*> (x .?> "arn")
            <*> (x .?> "pipelinesRunningCount")
            <*> (x .?> "availabilityZones" .!@ mempty)
            <*> (x .?> "programCount")
            <*> (x .?> "destinations" .!@ mempty)
            <*> (x .?> "name")
            <*> (x .?> "id")
            <*> (x .?> "multiplexSettings")
            <*> (x .?> "tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable StartMultiplex

instance NFData StartMultiplex

instance ToHeaders StartMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StartMultiplex where
  toJSON = const (Object mempty)

instance ToPath StartMultiplex where
  toPath StartMultiplex' {..} =
    mconcat ["/prod/multiplexes/", toBS _sMultiplexId, "/start"]

instance ToQuery StartMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'startMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
  { _starsState ::
      !(Maybe MultiplexState),
    _starsARN :: !(Maybe Text),
    _starsPipelinesRunningCount :: !(Maybe Int),
    _starsAvailabilityZones :: !(Maybe [Text]),
    _starsProgramCount :: !(Maybe Int),
    _starsDestinations ::
      !(Maybe [MultiplexOutputDestination]),
    _starsName :: !(Maybe Text),
    _starsId :: !(Maybe Text),
    _starsMultiplexSettings ::
      !(Maybe MultiplexSettings),
    _starsTags :: !(Maybe (Map Text (Text))),
    _starsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'starsState' - The current state of the multiplex.
--
-- * 'starsARN' - The unique arn of the multiplex.
--
-- * 'starsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'starsAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'starsProgramCount' - The number of programs in the multiplex.
--
-- * 'starsDestinations' - A list of the multiplex output destinations.
--
-- * 'starsName' - The name of the multiplex.
--
-- * 'starsId' - The unique id of the multiplex.
--
-- * 'starsMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'starsTags' - A collection of key-value pairs.
--
-- * 'starsResponseStatus' - -- | The response status code.
startMultiplexResponse ::
  -- | 'starsResponseStatus'
  Int ->
  StartMultiplexResponse
startMultiplexResponse pResponseStatus_ =
  StartMultiplexResponse'
    { _starsState = Nothing,
      _starsARN = Nothing,
      _starsPipelinesRunningCount = Nothing,
      _starsAvailabilityZones = Nothing,
      _starsProgramCount = Nothing,
      _starsDestinations = Nothing,
      _starsName = Nothing,
      _starsId = Nothing,
      _starsMultiplexSettings = Nothing,
      _starsTags = Nothing,
      _starsResponseStatus = pResponseStatus_
    }

-- | The current state of the multiplex.
starsState :: Lens' StartMultiplexResponse (Maybe MultiplexState)
starsState = lens _starsState (\s a -> s {_starsState = a})

-- | The unique arn of the multiplex.
starsARN :: Lens' StartMultiplexResponse (Maybe Text)
starsARN = lens _starsARN (\s a -> s {_starsARN = a})

-- | The number of currently healthy pipelines.
starsPipelinesRunningCount :: Lens' StartMultiplexResponse (Maybe Int)
starsPipelinesRunningCount = lens _starsPipelinesRunningCount (\s a -> s {_starsPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
starsAvailabilityZones :: Lens' StartMultiplexResponse [Text]
starsAvailabilityZones = lens _starsAvailabilityZones (\s a -> s {_starsAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
starsProgramCount :: Lens' StartMultiplexResponse (Maybe Int)
starsProgramCount = lens _starsProgramCount (\s a -> s {_starsProgramCount = a})

-- | A list of the multiplex output destinations.
starsDestinations :: Lens' StartMultiplexResponse [MultiplexOutputDestination]
starsDestinations = lens _starsDestinations (\s a -> s {_starsDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
starsName :: Lens' StartMultiplexResponse (Maybe Text)
starsName = lens _starsName (\s a -> s {_starsName = a})

-- | The unique id of the multiplex.
starsId :: Lens' StartMultiplexResponse (Maybe Text)
starsId = lens _starsId (\s a -> s {_starsId = a})

-- | Configuration for a multiplex event.
starsMultiplexSettings :: Lens' StartMultiplexResponse (Maybe MultiplexSettings)
starsMultiplexSettings = lens _starsMultiplexSettings (\s a -> s {_starsMultiplexSettings = a})

-- | A collection of key-value pairs.
starsTags :: Lens' StartMultiplexResponse (HashMap Text (Text))
starsTags = lens _starsTags (\s a -> s {_starsTags = a}) . _Default . _Map

-- | -- | The response status code.
starsResponseStatus :: Lens' StartMultiplexResponse Int
starsResponseStatus = lens _starsResponseStatus (\s a -> s {_starsResponseStatus = a})

instance NFData StartMultiplexResponse

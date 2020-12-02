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
-- Module      : Network.AWS.MediaLive.StopMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running multiplex. If the multiplex isn't running, this action has no effect.
module Network.AWS.MediaLive.StopMultiplex
  ( -- * Creating a Request
    stopMultiplex,
    StopMultiplex,

    -- * Request Lenses
    smMultiplexId,

    -- * Destructuring the Response
    stopMultiplexResponse,
    StopMultiplexResponse,

    -- * Response Lenses
    smrsState,
    smrsARN,
    smrsPipelinesRunningCount,
    smrsAvailabilityZones,
    smrsProgramCount,
    smrsDestinations,
    smrsName,
    smrsId,
    smrsMultiplexSettings,
    smrsTags,
    smrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for StopMultiplexRequest
--
-- /See:/ 'stopMultiplex' smart constructor.
newtype StopMultiplex = StopMultiplex' {_smMultiplexId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smMultiplexId' - The ID of the multiplex.
stopMultiplex ::
  -- | 'smMultiplexId'
  Text ->
  StopMultiplex
stopMultiplex pMultiplexId_ =
  StopMultiplex' {_smMultiplexId = pMultiplexId_}

-- | The ID of the multiplex.
smMultiplexId :: Lens' StopMultiplex Text
smMultiplexId = lens _smMultiplexId (\s a -> s {_smMultiplexId = a})

instance AWSRequest StopMultiplex where
  type Rs StopMultiplex = StopMultiplexResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          StopMultiplexResponse'
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

instance Hashable StopMultiplex

instance NFData StopMultiplex

instance ToHeaders StopMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StopMultiplex where
  toJSON = const (Object mempty)

instance ToPath StopMultiplex where
  toPath StopMultiplex' {..} =
    mconcat ["/prod/multiplexes/", toBS _smMultiplexId, "/stop"]

instance ToQuery StopMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for StopMultiplexResponse
--
-- /See:/ 'stopMultiplexResponse' smart constructor.
data StopMultiplexResponse = StopMultiplexResponse'
  { _smrsState ::
      !(Maybe MultiplexState),
    _smrsARN :: !(Maybe Text),
    _smrsPipelinesRunningCount :: !(Maybe Int),
    _smrsAvailabilityZones :: !(Maybe [Text]),
    _smrsProgramCount :: !(Maybe Int),
    _smrsDestinations ::
      !(Maybe [MultiplexOutputDestination]),
    _smrsName :: !(Maybe Text),
    _smrsId :: !(Maybe Text),
    _smrsMultiplexSettings ::
      !(Maybe MultiplexSettings),
    _smrsTags :: !(Maybe (Map Text (Text))),
    _smrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsState' - The current state of the multiplex.
--
-- * 'smrsARN' - The unique arn of the multiplex.
--
-- * 'smrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'smrsAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'smrsProgramCount' - The number of programs in the multiplex.
--
-- * 'smrsDestinations' - A list of the multiplex output destinations.
--
-- * 'smrsName' - The name of the multiplex.
--
-- * 'smrsId' - The unique id of the multiplex.
--
-- * 'smrsMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'smrsTags' - A collection of key-value pairs.
--
-- * 'smrsResponseStatus' - -- | The response status code.
stopMultiplexResponse ::
  -- | 'smrsResponseStatus'
  Int ->
  StopMultiplexResponse
stopMultiplexResponse pResponseStatus_ =
  StopMultiplexResponse'
    { _smrsState = Nothing,
      _smrsARN = Nothing,
      _smrsPipelinesRunningCount = Nothing,
      _smrsAvailabilityZones = Nothing,
      _smrsProgramCount = Nothing,
      _smrsDestinations = Nothing,
      _smrsName = Nothing,
      _smrsId = Nothing,
      _smrsMultiplexSettings = Nothing,
      _smrsTags = Nothing,
      _smrsResponseStatus = pResponseStatus_
    }

-- | The current state of the multiplex.
smrsState :: Lens' StopMultiplexResponse (Maybe MultiplexState)
smrsState = lens _smrsState (\s a -> s {_smrsState = a})

-- | The unique arn of the multiplex.
smrsARN :: Lens' StopMultiplexResponse (Maybe Text)
smrsARN = lens _smrsARN (\s a -> s {_smrsARN = a})

-- | The number of currently healthy pipelines.
smrsPipelinesRunningCount :: Lens' StopMultiplexResponse (Maybe Int)
smrsPipelinesRunningCount = lens _smrsPipelinesRunningCount (\s a -> s {_smrsPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
smrsAvailabilityZones :: Lens' StopMultiplexResponse [Text]
smrsAvailabilityZones = lens _smrsAvailabilityZones (\s a -> s {_smrsAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
smrsProgramCount :: Lens' StopMultiplexResponse (Maybe Int)
smrsProgramCount = lens _smrsProgramCount (\s a -> s {_smrsProgramCount = a})

-- | A list of the multiplex output destinations.
smrsDestinations :: Lens' StopMultiplexResponse [MultiplexOutputDestination]
smrsDestinations = lens _smrsDestinations (\s a -> s {_smrsDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
smrsName :: Lens' StopMultiplexResponse (Maybe Text)
smrsName = lens _smrsName (\s a -> s {_smrsName = a})

-- | The unique id of the multiplex.
smrsId :: Lens' StopMultiplexResponse (Maybe Text)
smrsId = lens _smrsId (\s a -> s {_smrsId = a})

-- | Configuration for a multiplex event.
smrsMultiplexSettings :: Lens' StopMultiplexResponse (Maybe MultiplexSettings)
smrsMultiplexSettings = lens _smrsMultiplexSettings (\s a -> s {_smrsMultiplexSettings = a})

-- | A collection of key-value pairs.
smrsTags :: Lens' StopMultiplexResponse (HashMap Text (Text))
smrsTags = lens _smrsTags (\s a -> s {_smrsTags = a}) . _Default . _Map

-- | -- | The response status code.
smrsResponseStatus :: Lens' StopMultiplexResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\s a -> s {_smrsResponseStatus = a})

instance NFData StopMultiplexResponse

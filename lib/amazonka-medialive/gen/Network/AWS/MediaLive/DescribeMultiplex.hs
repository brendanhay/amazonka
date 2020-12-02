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
-- Module      : Network.AWS.MediaLive.DescribeMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a multiplex.
module Network.AWS.MediaLive.DescribeMultiplex
  ( -- * Creating a Request
    describeMultiplex,
    DescribeMultiplex,

    -- * Request Lenses
    dmMultiplexId,

    -- * Destructuring the Response
    describeMultiplexResponse,
    DescribeMultiplexResponse,

    -- * Response Lenses
    dmrsState,
    dmrsARN,
    dmrsPipelinesRunningCount,
    dmrsAvailabilityZones,
    dmrsProgramCount,
    dmrsDestinations,
    dmrsName,
    dmrsId,
    dmrsMultiplexSettings,
    dmrsTags,
    dmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'describeMultiplex' smart constructor.
newtype DescribeMultiplex = DescribeMultiplex'
  { _dmMultiplexId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmMultiplexId' - The ID of the multiplex.
describeMultiplex ::
  -- | 'dmMultiplexId'
  Text ->
  DescribeMultiplex
describeMultiplex pMultiplexId_ =
  DescribeMultiplex' {_dmMultiplexId = pMultiplexId_}

-- | The ID of the multiplex.
dmMultiplexId :: Lens' DescribeMultiplex Text
dmMultiplexId = lens _dmMultiplexId (\s a -> s {_dmMultiplexId = a})

instance AWSRequest DescribeMultiplex where
  type Rs DescribeMultiplex = DescribeMultiplexResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeMultiplexResponse'
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

instance Hashable DescribeMultiplex

instance NFData DescribeMultiplex

instance ToHeaders DescribeMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeMultiplex where
  toPath DescribeMultiplex' {..} =
    mconcat ["/prod/multiplexes/", toBS _dmMultiplexId]

instance ToQuery DescribeMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'describeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
  { _dmrsState ::
      !(Maybe MultiplexState),
    _dmrsARN :: !(Maybe Text),
    _dmrsPipelinesRunningCount ::
      !(Maybe Int),
    _dmrsAvailabilityZones ::
      !(Maybe [Text]),
    _dmrsProgramCount :: !(Maybe Int),
    _dmrsDestinations ::
      !(Maybe [MultiplexOutputDestination]),
    _dmrsName :: !(Maybe Text),
    _dmrsId :: !(Maybe Text),
    _dmrsMultiplexSettings ::
      !(Maybe MultiplexSettings),
    _dmrsTags :: !(Maybe (Map Text (Text))),
    _dmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsState' - The current state of the multiplex.
--
-- * 'dmrsARN' - The unique arn of the multiplex.
--
-- * 'dmrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'dmrsAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'dmrsProgramCount' - The number of programs in the multiplex.
--
-- * 'dmrsDestinations' - A list of the multiplex output destinations.
--
-- * 'dmrsName' - The name of the multiplex.
--
-- * 'dmrsId' - The unique id of the multiplex.
--
-- * 'dmrsMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'dmrsTags' - A collection of key-value pairs.
--
-- * 'dmrsResponseStatus' - -- | The response status code.
describeMultiplexResponse ::
  -- | 'dmrsResponseStatus'
  Int ->
  DescribeMultiplexResponse
describeMultiplexResponse pResponseStatus_ =
  DescribeMultiplexResponse'
    { _dmrsState = Nothing,
      _dmrsARN = Nothing,
      _dmrsPipelinesRunningCount = Nothing,
      _dmrsAvailabilityZones = Nothing,
      _dmrsProgramCount = Nothing,
      _dmrsDestinations = Nothing,
      _dmrsName = Nothing,
      _dmrsId = Nothing,
      _dmrsMultiplexSettings = Nothing,
      _dmrsTags = Nothing,
      _dmrsResponseStatus = pResponseStatus_
    }

-- | The current state of the multiplex.
dmrsState :: Lens' DescribeMultiplexResponse (Maybe MultiplexState)
dmrsState = lens _dmrsState (\s a -> s {_dmrsState = a})

-- | The unique arn of the multiplex.
dmrsARN :: Lens' DescribeMultiplexResponse (Maybe Text)
dmrsARN = lens _dmrsARN (\s a -> s {_dmrsARN = a})

-- | The number of currently healthy pipelines.
dmrsPipelinesRunningCount :: Lens' DescribeMultiplexResponse (Maybe Int)
dmrsPipelinesRunningCount = lens _dmrsPipelinesRunningCount (\s a -> s {_dmrsPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
dmrsAvailabilityZones :: Lens' DescribeMultiplexResponse [Text]
dmrsAvailabilityZones = lens _dmrsAvailabilityZones (\s a -> s {_dmrsAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
dmrsProgramCount :: Lens' DescribeMultiplexResponse (Maybe Int)
dmrsProgramCount = lens _dmrsProgramCount (\s a -> s {_dmrsProgramCount = a})

-- | A list of the multiplex output destinations.
dmrsDestinations :: Lens' DescribeMultiplexResponse [MultiplexOutputDestination]
dmrsDestinations = lens _dmrsDestinations (\s a -> s {_dmrsDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
dmrsName :: Lens' DescribeMultiplexResponse (Maybe Text)
dmrsName = lens _dmrsName (\s a -> s {_dmrsName = a})

-- | The unique id of the multiplex.
dmrsId :: Lens' DescribeMultiplexResponse (Maybe Text)
dmrsId = lens _dmrsId (\s a -> s {_dmrsId = a})

-- | Configuration for a multiplex event.
dmrsMultiplexSettings :: Lens' DescribeMultiplexResponse (Maybe MultiplexSettings)
dmrsMultiplexSettings = lens _dmrsMultiplexSettings (\s a -> s {_dmrsMultiplexSettings = a})

-- | A collection of key-value pairs.
dmrsTags :: Lens' DescribeMultiplexResponse (HashMap Text (Text))
dmrsTags = lens _dmrsTags (\s a -> s {_dmrsTags = a}) . _Default . _Map

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DescribeMultiplexResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\s a -> s {_dmrsResponseStatus = a})

instance NFData DescribeMultiplexResponse

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
-- Module      : Network.AWS.MediaLive.DeleteMultiplex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a multiplex. The multiplex must be idle.
module Network.AWS.MediaLive.DeleteMultiplex
  ( -- * Creating a Request
    deleteMultiplex,
    DeleteMultiplex,

    -- * Request Lenses
    dMultiplexId,

    -- * Destructuring the Response
    deleteMultiplexResponse,
    DeleteMultiplexResponse,

    -- * Response Lenses
    delrsState,
    delrsARN,
    delrsPipelinesRunningCount,
    delrsAvailabilityZones,
    delrsProgramCount,
    delrsDestinations,
    delrsName,
    delrsId,
    delrsMultiplexSettings,
    delrsTags,
    delrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'deleteMultiplex' smart constructor.
newtype DeleteMultiplex = DeleteMultiplex' {_dMultiplexId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMultiplex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMultiplexId' - The ID of the multiplex.
deleteMultiplex ::
  -- | 'dMultiplexId'
  Text ->
  DeleteMultiplex
deleteMultiplex pMultiplexId_ =
  DeleteMultiplex' {_dMultiplexId = pMultiplexId_}

-- | The ID of the multiplex.
dMultiplexId :: Lens' DeleteMultiplex Text
dMultiplexId = lens _dMultiplexId (\s a -> s {_dMultiplexId = a})

instance AWSRequest DeleteMultiplex where
  type Rs DeleteMultiplex = DeleteMultiplexResponse
  request = delete mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DeleteMultiplexResponse'
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

instance Hashable DeleteMultiplex

instance NFData DeleteMultiplex

instance ToHeaders DeleteMultiplex where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteMultiplex where
  toPath DeleteMultiplex' {..} =
    mconcat ["/prod/multiplexes/", toBS _dMultiplexId]

instance ToQuery DeleteMultiplex where
  toQuery = const mempty

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'deleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
  { _delrsState ::
      !(Maybe MultiplexState),
    _delrsARN :: !(Maybe Text),
    _delrsPipelinesRunningCount :: !(Maybe Int),
    _delrsAvailabilityZones :: !(Maybe [Text]),
    _delrsProgramCount :: !(Maybe Int),
    _delrsDestinations ::
      !(Maybe [MultiplexOutputDestination]),
    _delrsName :: !(Maybe Text),
    _delrsId :: !(Maybe Text),
    _delrsMultiplexSettings ::
      !(Maybe MultiplexSettings),
    _delrsTags :: !(Maybe (Map Text (Text))),
    _delrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMultiplexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsState' - The current state of the multiplex.
--
-- * 'delrsARN' - The unique arn of the multiplex.
--
-- * 'delrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'delrsAvailabilityZones' - A list of availability zones for the multiplex.
--
-- * 'delrsProgramCount' - The number of programs in the multiplex.
--
-- * 'delrsDestinations' - A list of the multiplex output destinations.
--
-- * 'delrsName' - The name of the multiplex.
--
-- * 'delrsId' - The unique id of the multiplex.
--
-- * 'delrsMultiplexSettings' - Configuration for a multiplex event.
--
-- * 'delrsTags' - A collection of key-value pairs.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteMultiplexResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteMultiplexResponse
deleteMultiplexResponse pResponseStatus_ =
  DeleteMultiplexResponse'
    { _delrsState = Nothing,
      _delrsARN = Nothing,
      _delrsPipelinesRunningCount = Nothing,
      _delrsAvailabilityZones = Nothing,
      _delrsProgramCount = Nothing,
      _delrsDestinations = Nothing,
      _delrsName = Nothing,
      _delrsId = Nothing,
      _delrsMultiplexSettings = Nothing,
      _delrsTags = Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | The current state of the multiplex.
delrsState :: Lens' DeleteMultiplexResponse (Maybe MultiplexState)
delrsState = lens _delrsState (\s a -> s {_delrsState = a})

-- | The unique arn of the multiplex.
delrsARN :: Lens' DeleteMultiplexResponse (Maybe Text)
delrsARN = lens _delrsARN (\s a -> s {_delrsARN = a})

-- | The number of currently healthy pipelines.
delrsPipelinesRunningCount :: Lens' DeleteMultiplexResponse (Maybe Int)
delrsPipelinesRunningCount = lens _delrsPipelinesRunningCount (\s a -> s {_delrsPipelinesRunningCount = a})

-- | A list of availability zones for the multiplex.
delrsAvailabilityZones :: Lens' DeleteMultiplexResponse [Text]
delrsAvailabilityZones = lens _delrsAvailabilityZones (\s a -> s {_delrsAvailabilityZones = a}) . _Default . _Coerce

-- | The number of programs in the multiplex.
delrsProgramCount :: Lens' DeleteMultiplexResponse (Maybe Int)
delrsProgramCount = lens _delrsProgramCount (\s a -> s {_delrsProgramCount = a})

-- | A list of the multiplex output destinations.
delrsDestinations :: Lens' DeleteMultiplexResponse [MultiplexOutputDestination]
delrsDestinations = lens _delrsDestinations (\s a -> s {_delrsDestinations = a}) . _Default . _Coerce

-- | The name of the multiplex.
delrsName :: Lens' DeleteMultiplexResponse (Maybe Text)
delrsName = lens _delrsName (\s a -> s {_delrsName = a})

-- | The unique id of the multiplex.
delrsId :: Lens' DeleteMultiplexResponse (Maybe Text)
delrsId = lens _delrsId (\s a -> s {_delrsId = a})

-- | Configuration for a multiplex event.
delrsMultiplexSettings :: Lens' DeleteMultiplexResponse (Maybe MultiplexSettings)
delrsMultiplexSettings = lens _delrsMultiplexSettings (\s a -> s {_delrsMultiplexSettings = a})

-- | A collection of key-value pairs.
delrsTags :: Lens' DeleteMultiplexResponse (HashMap Text (Text))
delrsTags = lens _delrsTags (\s a -> s {_delrsTags = a}) . _Default . _Map

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteMultiplexResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteMultiplexResponse

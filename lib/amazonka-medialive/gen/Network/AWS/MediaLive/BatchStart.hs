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
-- Module      : Network.AWS.MediaLive.BatchStart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts existing resources
module Network.AWS.MediaLive.BatchStart
  ( -- * Creating a Request
    batchStart,
    BatchStart,

    -- * Request Lenses
    bsChannelIds,
    bsMultiplexIds,

    -- * Destructuring the Response
    batchStartResponse,
    BatchStartResponse,

    -- * Response Lenses
    bsrsSuccessful,
    bsrsFailed,
    bsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to start resources
--
-- /See:/ 'batchStart' smart constructor.
data BatchStart = BatchStart'
  { _bsChannelIds :: !(Maybe [Text]),
    _bsMultiplexIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsChannelIds' - List of channel IDs
--
-- * 'bsMultiplexIds' - List of multiplex IDs
batchStart ::
  BatchStart
batchStart =
  BatchStart' {_bsChannelIds = Nothing, _bsMultiplexIds = Nothing}

-- | List of channel IDs
bsChannelIds :: Lens' BatchStart [Text]
bsChannelIds = lens _bsChannelIds (\s a -> s {_bsChannelIds = a}) . _Default . _Coerce

-- | List of multiplex IDs
bsMultiplexIds :: Lens' BatchStart [Text]
bsMultiplexIds = lens _bsMultiplexIds (\s a -> s {_bsMultiplexIds = a}) . _Default . _Coerce

instance AWSRequest BatchStart where
  type Rs BatchStart = BatchStartResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          BatchStartResponse'
            <$> (x .?> "successful" .!@ mempty)
            <*> (x .?> "failed" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchStart

instance NFData BatchStart

instance ToHeaders BatchStart where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON BatchStart where
  toJSON BatchStart' {..} =
    object
      ( catMaybes
          [ ("channelIds" .=) <$> _bsChannelIds,
            ("multiplexIds" .=) <$> _bsMultiplexIds
          ]
      )

instance ToPath BatchStart where
  toPath = const "/prod/batch/start"

instance ToQuery BatchStart where
  toQuery = const mempty

-- | Placeholder documentation for BatchStartResponse
--
-- /See:/ 'batchStartResponse' smart constructor.
data BatchStartResponse = BatchStartResponse'
  { _bsrsSuccessful ::
      !(Maybe [BatchSuccessfulResultModel]),
    _bsrsFailed :: !(Maybe [BatchFailedResultModel]),
    _bsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStartResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsrsSuccessful' - List of successful operations
--
-- * 'bsrsFailed' - List of failed operations
--
-- * 'bsrsResponseStatus' - -- | The response status code.
batchStartResponse ::
  -- | 'bsrsResponseStatus'
  Int ->
  BatchStartResponse
batchStartResponse pResponseStatus_ =
  BatchStartResponse'
    { _bsrsSuccessful = Nothing,
      _bsrsFailed = Nothing,
      _bsrsResponseStatus = pResponseStatus_
    }

-- | List of successful operations
bsrsSuccessful :: Lens' BatchStartResponse [BatchSuccessfulResultModel]
bsrsSuccessful = lens _bsrsSuccessful (\s a -> s {_bsrsSuccessful = a}) . _Default . _Coerce

-- | List of failed operations
bsrsFailed :: Lens' BatchStartResponse [BatchFailedResultModel]
bsrsFailed = lens _bsrsFailed (\s a -> s {_bsrsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
bsrsResponseStatus :: Lens' BatchStartResponse Int
bsrsResponseStatus = lens _bsrsResponseStatus (\s a -> s {_bsrsResponseStatus = a})

instance NFData BatchStartResponse

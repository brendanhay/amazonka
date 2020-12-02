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
-- Module      : Network.AWS.MediaLive.BatchStop
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops running resources
module Network.AWS.MediaLive.BatchStop
  ( -- * Creating a Request
    batchStop,
    BatchStop,

    -- * Request Lenses
    bChannelIds,
    bMultiplexIds,

    -- * Destructuring the Response
    batchStopResponse,
    BatchStopResponse,

    -- * Response Lenses
    brsSuccessful,
    brsFailed,
    brsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to stop resources
--
-- /See:/ 'batchStop' smart constructor.
data BatchStop = BatchStop'
  { _bChannelIds :: !(Maybe [Text]),
    _bMultiplexIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStop' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bChannelIds' - List of channel IDs
--
-- * 'bMultiplexIds' - List of multiplex IDs
batchStop ::
  BatchStop
batchStop =
  BatchStop' {_bChannelIds = Nothing, _bMultiplexIds = Nothing}

-- | List of channel IDs
bChannelIds :: Lens' BatchStop [Text]
bChannelIds = lens _bChannelIds (\s a -> s {_bChannelIds = a}) . _Default . _Coerce

-- | List of multiplex IDs
bMultiplexIds :: Lens' BatchStop [Text]
bMultiplexIds = lens _bMultiplexIds (\s a -> s {_bMultiplexIds = a}) . _Default . _Coerce

instance AWSRequest BatchStop where
  type Rs BatchStop = BatchStopResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          BatchStopResponse'
            <$> (x .?> "successful" .!@ mempty)
            <*> (x .?> "failed" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchStop

instance NFData BatchStop

instance ToHeaders BatchStop where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON BatchStop where
  toJSON BatchStop' {..} =
    object
      ( catMaybes
          [ ("channelIds" .=) <$> _bChannelIds,
            ("multiplexIds" .=) <$> _bMultiplexIds
          ]
      )

instance ToPath BatchStop where
  toPath = const "/prod/batch/stop"

instance ToQuery BatchStop where
  toQuery = const mempty

-- | Placeholder documentation for BatchStopResponse
--
-- /See:/ 'batchStopResponse' smart constructor.
data BatchStopResponse = BatchStopResponse'
  { _brsSuccessful ::
      !(Maybe [BatchSuccessfulResultModel]),
    _brsFailed :: !(Maybe [BatchFailedResultModel]),
    _brsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchStopResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsSuccessful' - List of successful operations
--
-- * 'brsFailed' - List of failed operations
--
-- * 'brsResponseStatus' - -- | The response status code.
batchStopResponse ::
  -- | 'brsResponseStatus'
  Int ->
  BatchStopResponse
batchStopResponse pResponseStatus_ =
  BatchStopResponse'
    { _brsSuccessful = Nothing,
      _brsFailed = Nothing,
      _brsResponseStatus = pResponseStatus_
    }

-- | List of successful operations
brsSuccessful :: Lens' BatchStopResponse [BatchSuccessfulResultModel]
brsSuccessful = lens _brsSuccessful (\s a -> s {_brsSuccessful = a}) . _Default . _Coerce

-- | List of failed operations
brsFailed :: Lens' BatchStopResponse [BatchFailedResultModel]
brsFailed = lens _brsFailed (\s a -> s {_brsFailed = a}) . _Default . _Coerce

-- | -- | The response status code.
brsResponseStatus :: Lens' BatchStopResponse Int
brsResponseStatus = lens _brsResponseStatus (\s a -> s {_brsResponseStatus = a})

instance NFData BatchStopResponse

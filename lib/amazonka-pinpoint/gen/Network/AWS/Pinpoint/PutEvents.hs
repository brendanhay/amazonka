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
-- Module      : Network.AWS.Pinpoint.PutEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event to record for endpoints, or creates or updates endpoint data that existing events are associated with.
module Network.AWS.Pinpoint.PutEvents
  ( -- * Creating a Request
    putEvents,
    PutEvents,

    -- * Request Lenses
    peApplicationId,
    peEventsRequest,

    -- * Destructuring the Response
    putEventsResponse,
    PutEventsResponse,

    -- * Response Lenses
    persResponseStatus,
    persEventsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putEvents' smart constructor.
data PutEvents = PutEvents'
  { _peApplicationId :: !Text,
    _peEventsRequest :: !EventsRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'peEventsRequest' - Undocumented member.
putEvents ::
  -- | 'peApplicationId'
  Text ->
  -- | 'peEventsRequest'
  EventsRequest ->
  PutEvents
putEvents pApplicationId_ pEventsRequest_ =
  PutEvents'
    { _peApplicationId = pApplicationId_,
      _peEventsRequest = pEventsRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
peApplicationId :: Lens' PutEvents Text
peApplicationId = lens _peApplicationId (\s a -> s {_peApplicationId = a})

-- | Undocumented member.
peEventsRequest :: Lens' PutEvents EventsRequest
peEventsRequest = lens _peEventsRequest (\s a -> s {_peEventsRequest = a})

instance AWSRequest PutEvents where
  type Rs PutEvents = PutEventsResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          PutEventsResponse' <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable PutEvents

instance NFData PutEvents

instance ToHeaders PutEvents where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PutEvents where
  toJSON PutEvents' {..} =
    object (catMaybes [Just ("EventsRequest" .= _peEventsRequest)])

instance ToPath PutEvents where
  toPath PutEvents' {..} =
    mconcat ["/v1/apps/", toBS _peApplicationId, "/events"]

instance ToQuery PutEvents where
  toQuery = const mempty

-- | /See:/ 'putEventsResponse' smart constructor.
data PutEventsResponse = PutEventsResponse'
  { _persResponseStatus ::
      !Int,
    _persEventsResponse :: !EventsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'persResponseStatus' - -- | The response status code.
--
-- * 'persEventsResponse' - Undocumented member.
putEventsResponse ::
  -- | 'persResponseStatus'
  Int ->
  -- | 'persEventsResponse'
  EventsResponse ->
  PutEventsResponse
putEventsResponse pResponseStatus_ pEventsResponse_ =
  PutEventsResponse'
    { _persResponseStatus = pResponseStatus_,
      _persEventsResponse = pEventsResponse_
    }

-- | -- | The response status code.
persResponseStatus :: Lens' PutEventsResponse Int
persResponseStatus = lens _persResponseStatus (\s a -> s {_persResponseStatus = a})

-- | Undocumented member.
persEventsResponse :: Lens' PutEventsResponse EventsResponse
persEventsResponse = lens _persEventsResponse (\s a -> s {_persEventsResponse = a})

instance NFData PutEventsResponse

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
-- Module      : Network.AWS.Pinpoint.UpdateJourneyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) an active journey.
module Network.AWS.Pinpoint.UpdateJourneyState
  ( -- * Creating a Request
    updateJourneyState,
    UpdateJourneyState,

    -- * Request Lenses
    ujsJourneyId,
    ujsApplicationId,
    ujsJourneyStateRequest,

    -- * Destructuring the Response
    updateJourneyStateResponse,
    UpdateJourneyStateResponse,

    -- * Response Lenses
    ujsrsResponseStatus,
    ujsrsJourneyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJourneyState' smart constructor.
data UpdateJourneyState = UpdateJourneyState'
  { _ujsJourneyId ::
      !Text,
    _ujsApplicationId :: !Text,
    _ujsJourneyStateRequest :: !JourneyStateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJourneyState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujsJourneyId' - The unique identifier for the journey.
--
-- * 'ujsApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'ujsJourneyStateRequest' - Undocumented member.
updateJourneyState ::
  -- | 'ujsJourneyId'
  Text ->
  -- | 'ujsApplicationId'
  Text ->
  -- | 'ujsJourneyStateRequest'
  JourneyStateRequest ->
  UpdateJourneyState
updateJourneyState
  pJourneyId_
  pApplicationId_
  pJourneyStateRequest_ =
    UpdateJourneyState'
      { _ujsJourneyId = pJourneyId_,
        _ujsApplicationId = pApplicationId_,
        _ujsJourneyStateRequest = pJourneyStateRequest_
      }

-- | The unique identifier for the journey.
ujsJourneyId :: Lens' UpdateJourneyState Text
ujsJourneyId = lens _ujsJourneyId (\s a -> s {_ujsJourneyId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
ujsApplicationId :: Lens' UpdateJourneyState Text
ujsApplicationId = lens _ujsApplicationId (\s a -> s {_ujsApplicationId = a})

-- | Undocumented member.
ujsJourneyStateRequest :: Lens' UpdateJourneyState JourneyStateRequest
ujsJourneyStateRequest = lens _ujsJourneyStateRequest (\s a -> s {_ujsJourneyStateRequest = a})

instance AWSRequest UpdateJourneyState where
  type Rs UpdateJourneyState = UpdateJourneyStateResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateJourneyStateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateJourneyState

instance NFData UpdateJourneyState

instance ToHeaders UpdateJourneyState where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateJourneyState where
  toJSON UpdateJourneyState' {..} =
    object
      ( catMaybes
          [Just ("JourneyStateRequest" .= _ujsJourneyStateRequest)]
      )

instance ToPath UpdateJourneyState where
  toPath UpdateJourneyState' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _ujsApplicationId,
        "/journeys/",
        toBS _ujsJourneyId,
        "/state"
      ]

instance ToQuery UpdateJourneyState where
  toQuery = const mempty

-- | /See:/ 'updateJourneyStateResponse' smart constructor.
data UpdateJourneyStateResponse = UpdateJourneyStateResponse'
  { _ujsrsResponseStatus ::
      !Int,
    _ujsrsJourneyResponse ::
      !JourneyResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJourneyStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujsrsResponseStatus' - -- | The response status code.
--
-- * 'ujsrsJourneyResponse' - Undocumented member.
updateJourneyStateResponse ::
  -- | 'ujsrsResponseStatus'
  Int ->
  -- | 'ujsrsJourneyResponse'
  JourneyResponse ->
  UpdateJourneyStateResponse
updateJourneyStateResponse pResponseStatus_ pJourneyResponse_ =
  UpdateJourneyStateResponse'
    { _ujsrsResponseStatus =
        pResponseStatus_,
      _ujsrsJourneyResponse = pJourneyResponse_
    }

-- | -- | The response status code.
ujsrsResponseStatus :: Lens' UpdateJourneyStateResponse Int
ujsrsResponseStatus = lens _ujsrsResponseStatus (\s a -> s {_ujsrsResponseStatus = a})

-- | Undocumented member.
ujsrsJourneyResponse :: Lens' UpdateJourneyStateResponse JourneyResponse
ujsrsJourneyResponse = lens _ujsrsJourneyResponse (\s a -> s {_ujsrsJourneyResponse = a})

instance NFData UpdateJourneyStateResponse

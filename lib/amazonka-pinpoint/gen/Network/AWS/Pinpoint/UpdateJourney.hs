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
-- Module      : Network.AWS.Pinpoint.UpdateJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a journey.
module Network.AWS.Pinpoint.UpdateJourney
  ( -- * Creating a Request
    updateJourney,
    UpdateJourney,

    -- * Request Lenses
    ujJourneyId,
    ujApplicationId,
    ujWriteJourneyRequest,

    -- * Destructuring the Response
    updateJourneyResponse,
    UpdateJourneyResponse,

    -- * Response Lenses
    ujrsResponseStatus,
    ujrsJourneyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateJourney' smart constructor.
data UpdateJourney = UpdateJourney'
  { _ujJourneyId :: !Text,
    _ujApplicationId :: !Text,
    _ujWriteJourneyRequest :: !WriteJourneyRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJourney' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujJourneyId' - The unique identifier for the journey.
--
-- * 'ujApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'ujWriteJourneyRequest' - Undocumented member.
updateJourney ::
  -- | 'ujJourneyId'
  Text ->
  -- | 'ujApplicationId'
  Text ->
  -- | 'ujWriteJourneyRequest'
  WriteJourneyRequest ->
  UpdateJourney
updateJourney pJourneyId_ pApplicationId_ pWriteJourneyRequest_ =
  UpdateJourney'
    { _ujJourneyId = pJourneyId_,
      _ujApplicationId = pApplicationId_,
      _ujWriteJourneyRequest = pWriteJourneyRequest_
    }

-- | The unique identifier for the journey.
ujJourneyId :: Lens' UpdateJourney Text
ujJourneyId = lens _ujJourneyId (\s a -> s {_ujJourneyId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
ujApplicationId :: Lens' UpdateJourney Text
ujApplicationId = lens _ujApplicationId (\s a -> s {_ujApplicationId = a})

-- | Undocumented member.
ujWriteJourneyRequest :: Lens' UpdateJourney WriteJourneyRequest
ujWriteJourneyRequest = lens _ujWriteJourneyRequest (\s a -> s {_ujWriteJourneyRequest = a})

instance AWSRequest UpdateJourney where
  type Rs UpdateJourney = UpdateJourneyResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateJourneyResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateJourney

instance NFData UpdateJourney

instance ToHeaders UpdateJourney where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateJourney where
  toJSON UpdateJourney' {..} =
    object
      ( catMaybes
          [Just ("WriteJourneyRequest" .= _ujWriteJourneyRequest)]
      )

instance ToPath UpdateJourney where
  toPath UpdateJourney' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _ujApplicationId,
        "/journeys/",
        toBS _ujJourneyId
      ]

instance ToQuery UpdateJourney where
  toQuery = const mempty

-- | /See:/ 'updateJourneyResponse' smart constructor.
data UpdateJourneyResponse = UpdateJourneyResponse'
  { _ujrsResponseStatus ::
      !Int,
    _ujrsJourneyResponse :: !JourneyResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateJourneyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujrsResponseStatus' - -- | The response status code.
--
-- * 'ujrsJourneyResponse' - Undocumented member.
updateJourneyResponse ::
  -- | 'ujrsResponseStatus'
  Int ->
  -- | 'ujrsJourneyResponse'
  JourneyResponse ->
  UpdateJourneyResponse
updateJourneyResponse pResponseStatus_ pJourneyResponse_ =
  UpdateJourneyResponse'
    { _ujrsResponseStatus = pResponseStatus_,
      _ujrsJourneyResponse = pJourneyResponse_
    }

-- | -- | The response status code.
ujrsResponseStatus :: Lens' UpdateJourneyResponse Int
ujrsResponseStatus = lens _ujrsResponseStatus (\s a -> s {_ujrsResponseStatus = a})

-- | Undocumented member.
ujrsJourneyResponse :: Lens' UpdateJourneyResponse JourneyResponse
ujrsJourneyResponse = lens _ujrsJourneyResponse (\s a -> s {_ujrsJourneyResponse = a})

instance NFData UpdateJourneyResponse

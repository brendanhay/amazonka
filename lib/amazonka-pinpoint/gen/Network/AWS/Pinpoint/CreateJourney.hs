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
-- Module      : Network.AWS.Pinpoint.CreateJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a journey for an application.
module Network.AWS.Pinpoint.CreateJourney
  ( -- * Creating a Request
    createJourney,
    CreateJourney,

    -- * Request Lenses
    cjApplicationId,
    cjWriteJourneyRequest,

    -- * Destructuring the Response
    createJourneyResponse,
    CreateJourneyResponse,

    -- * Response Lenses
    cjrsResponseStatus,
    cjrsJourneyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJourney' smart constructor.
data CreateJourney = CreateJourney'
  { _cjApplicationId :: !Text,
    _cjWriteJourneyRequest :: !WriteJourneyRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJourney' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'cjWriteJourneyRequest' - Undocumented member.
createJourney ::
  -- | 'cjApplicationId'
  Text ->
  -- | 'cjWriteJourneyRequest'
  WriteJourneyRequest ->
  CreateJourney
createJourney pApplicationId_ pWriteJourneyRequest_ =
  CreateJourney'
    { _cjApplicationId = pApplicationId_,
      _cjWriteJourneyRequest = pWriteJourneyRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
cjApplicationId :: Lens' CreateJourney Text
cjApplicationId = lens _cjApplicationId (\s a -> s {_cjApplicationId = a})

-- | Undocumented member.
cjWriteJourneyRequest :: Lens' CreateJourney WriteJourneyRequest
cjWriteJourneyRequest = lens _cjWriteJourneyRequest (\s a -> s {_cjWriteJourneyRequest = a})

instance AWSRequest CreateJourney where
  type Rs CreateJourney = CreateJourneyResponse
  request = postJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          CreateJourneyResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable CreateJourney

instance NFData CreateJourney

instance ToHeaders CreateJourney where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateJourney where
  toJSON CreateJourney' {..} =
    object
      ( catMaybes
          [Just ("WriteJourneyRequest" .= _cjWriteJourneyRequest)]
      )

instance ToPath CreateJourney where
  toPath CreateJourney' {..} =
    mconcat ["/v1/apps/", toBS _cjApplicationId, "/journeys"]

instance ToQuery CreateJourney where
  toQuery = const mempty

-- | /See:/ 'createJourneyResponse' smart constructor.
data CreateJourneyResponse = CreateJourneyResponse'
  { _cjrsResponseStatus ::
      !Int,
    _cjrsJourneyResponse :: !JourneyResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateJourneyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsResponseStatus' - -- | The response status code.
--
-- * 'cjrsJourneyResponse' - Undocumented member.
createJourneyResponse ::
  -- | 'cjrsResponseStatus'
  Int ->
  -- | 'cjrsJourneyResponse'
  JourneyResponse ->
  CreateJourneyResponse
createJourneyResponse pResponseStatus_ pJourneyResponse_ =
  CreateJourneyResponse'
    { _cjrsResponseStatus = pResponseStatus_,
      _cjrsJourneyResponse = pJourneyResponse_
    }

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJourneyResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\s a -> s {_cjrsResponseStatus = a})

-- | Undocumented member.
cjrsJourneyResponse :: Lens' CreateJourneyResponse JourneyResponse
cjrsJourneyResponse = lens _cjrsJourneyResponse (\s a -> s {_cjrsJourneyResponse = a})

instance NFData CreateJourneyResponse

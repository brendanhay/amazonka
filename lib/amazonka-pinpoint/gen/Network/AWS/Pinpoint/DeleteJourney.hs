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
-- Module      : Network.AWS.Pinpoint.DeleteJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a journey from an application.
module Network.AWS.Pinpoint.DeleteJourney
  ( -- * Creating a Request
    deleteJourney,
    DeleteJourney,

    -- * Request Lenses
    djJourneyId,
    djApplicationId,

    -- * Destructuring the Response
    deleteJourneyResponse,
    DeleteJourneyResponse,

    -- * Response Lenses
    djrsResponseStatus,
    djrsJourneyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteJourney' smart constructor.
data DeleteJourney = DeleteJourney'
  { _djJourneyId :: !Text,
    _djApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteJourney' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djJourneyId' - The unique identifier for the journey.
--
-- * 'djApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
deleteJourney ::
  -- | 'djJourneyId'
  Text ->
  -- | 'djApplicationId'
  Text ->
  DeleteJourney
deleteJourney pJourneyId_ pApplicationId_ =
  DeleteJourney'
    { _djJourneyId = pJourneyId_,
      _djApplicationId = pApplicationId_
    }

-- | The unique identifier for the journey.
djJourneyId :: Lens' DeleteJourney Text
djJourneyId = lens _djJourneyId (\s a -> s {_djJourneyId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
djApplicationId :: Lens' DeleteJourney Text
djApplicationId = lens _djApplicationId (\s a -> s {_djApplicationId = a})

instance AWSRequest DeleteJourney where
  type Rs DeleteJourney = DeleteJourneyResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteJourneyResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteJourney

instance NFData DeleteJourney

instance ToHeaders DeleteJourney where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteJourney where
  toPath DeleteJourney' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _djApplicationId,
        "/journeys/",
        toBS _djJourneyId
      ]

instance ToQuery DeleteJourney where
  toQuery = const mempty

-- | /See:/ 'deleteJourneyResponse' smart constructor.
data DeleteJourneyResponse = DeleteJourneyResponse'
  { _djrsResponseStatus ::
      !Int,
    _djrsJourneyResponse :: !JourneyResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteJourneyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djrsResponseStatus' - -- | The response status code.
--
-- * 'djrsJourneyResponse' - Undocumented member.
deleteJourneyResponse ::
  -- | 'djrsResponseStatus'
  Int ->
  -- | 'djrsJourneyResponse'
  JourneyResponse ->
  DeleteJourneyResponse
deleteJourneyResponse pResponseStatus_ pJourneyResponse_ =
  DeleteJourneyResponse'
    { _djrsResponseStatus = pResponseStatus_,
      _djrsJourneyResponse = pJourneyResponse_
    }

-- | -- | The response status code.
djrsResponseStatus :: Lens' DeleteJourneyResponse Int
djrsResponseStatus = lens _djrsResponseStatus (\s a -> s {_djrsResponseStatus = a})

-- | Undocumented member.
djrsJourneyResponse :: Lens' DeleteJourneyResponse JourneyResponse
djrsJourneyResponse = lens _djrsJourneyResponse (\s a -> s {_djrsJourneyResponse = a})

instance NFData DeleteJourneyResponse

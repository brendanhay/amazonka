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
-- Module      : Network.AWS.XRay.GetInsight
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summary information of an insight. This includes impact to clients and root cause services, the top anomalous services, the category, the state of the insight, and the start and end time of the insight.
module Network.AWS.XRay.GetInsight
  ( -- * Creating a Request
    getInsight,
    GetInsight,

    -- * Request Lenses
    giInsightId,

    -- * Destructuring the Response
    getInsightResponse,
    GetInsightResponse,

    -- * Response Lenses
    girsInsight,
    girsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'getInsight' smart constructor.
newtype GetInsight = GetInsight' {_giInsightId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsight' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giInsightId' - The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
getInsight ::
  -- | 'giInsightId'
  Text ->
  GetInsight
getInsight pInsightId_ = GetInsight' {_giInsightId = pInsightId_}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
giInsightId :: Lens' GetInsight Text
giInsightId = lens _giInsightId (\s a -> s {_giInsightId = a})

instance AWSRequest GetInsight where
  type Rs GetInsight = GetInsightResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          GetInsightResponse' <$> (x .?> "Insight") <*> (pure (fromEnum s))
      )

instance Hashable GetInsight

instance NFData GetInsight

instance ToHeaders GetInsight where
  toHeaders = const mempty

instance ToJSON GetInsight where
  toJSON GetInsight' {..} =
    object (catMaybes [Just ("InsightId" .= _giInsightId)])

instance ToPath GetInsight where
  toPath = const "/Insight"

instance ToQuery GetInsight where
  toQuery = const mempty

-- | /See:/ 'getInsightResponse' smart constructor.
data GetInsightResponse = GetInsightResponse'
  { _girsInsight ::
      !(Maybe Insight),
    _girsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsInsight' - The summary information of an insight.
--
-- * 'girsResponseStatus' - -- | The response status code.
getInsightResponse ::
  -- | 'girsResponseStatus'
  Int ->
  GetInsightResponse
getInsightResponse pResponseStatus_ =
  GetInsightResponse'
    { _girsInsight = Nothing,
      _girsResponseStatus = pResponseStatus_
    }

-- | The summary information of an insight.
girsInsight :: Lens' GetInsightResponse (Maybe Insight)
girsInsight = lens _girsInsight (\s a -> s {_girsInsight = a})

-- | -- | The response status code.
girsResponseStatus :: Lens' GetInsightResponse Int
girsResponseStatus = lens _girsResponseStatus (\s a -> s {_girsResponseStatus = a})

instance NFData GetInsightResponse

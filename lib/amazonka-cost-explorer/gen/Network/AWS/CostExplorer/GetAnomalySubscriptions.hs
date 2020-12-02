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
-- Module      : Network.AWS.CostExplorer.GetAnomalySubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the cost anomaly subscription objects for your account. You can filter using a list of cost anomaly monitor Amazon Resource Names (ARNs).
module Network.AWS.CostExplorer.GetAnomalySubscriptions
  ( -- * Creating a Request
    getAnomalySubscriptions,
    GetAnomalySubscriptions,

    -- * Request Lenses
    gasSubscriptionARNList,
    gasNextPageToken,
    gasMaxResults,
    gasMonitorARN,

    -- * Destructuring the Response
    getAnomalySubscriptionsResponse,
    GetAnomalySubscriptionsResponse,

    -- * Response Lenses
    gasrsNextPageToken,
    gasrsResponseStatus,
    gasrsAnomalySubscriptions,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAnomalySubscriptions' smart constructor.
data GetAnomalySubscriptions = GetAnomalySubscriptions'
  { _gasSubscriptionARNList ::
      !(Maybe [Text]),
    _gasNextPageToken :: !(Maybe Text),
    _gasMaxResults :: !(Maybe Int),
    _gasMonitorARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomalySubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasSubscriptionARNList' - A list of cost anomaly subscription ARNs.
--
-- * 'gasNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gasMaxResults' - The number of entries a paginated response contains.
--
-- * 'gasMonitorARN' - Cost anomaly monitor ARNs.
getAnomalySubscriptions ::
  GetAnomalySubscriptions
getAnomalySubscriptions =
  GetAnomalySubscriptions'
    { _gasSubscriptionARNList = Nothing,
      _gasNextPageToken = Nothing,
      _gasMaxResults = Nothing,
      _gasMonitorARN = Nothing
    }

-- | A list of cost anomaly subscription ARNs.
gasSubscriptionARNList :: Lens' GetAnomalySubscriptions [Text]
gasSubscriptionARNList = lens _gasSubscriptionARNList (\s a -> s {_gasSubscriptionARNList = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gasNextPageToken :: Lens' GetAnomalySubscriptions (Maybe Text)
gasNextPageToken = lens _gasNextPageToken (\s a -> s {_gasNextPageToken = a})

-- | The number of entries a paginated response contains.
gasMaxResults :: Lens' GetAnomalySubscriptions (Maybe Int)
gasMaxResults = lens _gasMaxResults (\s a -> s {_gasMaxResults = a})

-- | Cost anomaly monitor ARNs.
gasMonitorARN :: Lens' GetAnomalySubscriptions (Maybe Text)
gasMonitorARN = lens _gasMonitorARN (\s a -> s {_gasMonitorARN = a})

instance AWSRequest GetAnomalySubscriptions where
  type Rs GetAnomalySubscriptions = GetAnomalySubscriptionsResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetAnomalySubscriptionsResponse'
            <$> (x .?> "NextPageToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "AnomalySubscriptions" .!@ mempty)
      )

instance Hashable GetAnomalySubscriptions

instance NFData GetAnomalySubscriptions

instance ToHeaders GetAnomalySubscriptions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.GetAnomalySubscriptions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetAnomalySubscriptions where
  toJSON GetAnomalySubscriptions' {..} =
    object
      ( catMaybes
          [ ("SubscriptionArnList" .=) <$> _gasSubscriptionARNList,
            ("NextPageToken" .=) <$> _gasNextPageToken,
            ("MaxResults" .=) <$> _gasMaxResults,
            ("MonitorArn" .=) <$> _gasMonitorARN
          ]
      )

instance ToPath GetAnomalySubscriptions where
  toPath = const "/"

instance ToQuery GetAnomalySubscriptions where
  toQuery = const mempty

-- | /See:/ 'getAnomalySubscriptionsResponse' smart constructor.
data GetAnomalySubscriptionsResponse = GetAnomalySubscriptionsResponse'
  { _gasrsNextPageToken ::
      !(Maybe Text),
    _gasrsResponseStatus ::
      !Int,
    _gasrsAnomalySubscriptions ::
      ![AnomalySubscription]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAnomalySubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsNextPageToken' - The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'gasrsResponseStatus' - -- | The response status code.
--
-- * 'gasrsAnomalySubscriptions' - A list of cost anomaly subscriptions that includes the detailed metadata for each one.
getAnomalySubscriptionsResponse ::
  -- | 'gasrsResponseStatus'
  Int ->
  GetAnomalySubscriptionsResponse
getAnomalySubscriptionsResponse pResponseStatus_ =
  GetAnomalySubscriptionsResponse'
    { _gasrsNextPageToken = Nothing,
      _gasrsResponseStatus = pResponseStatus_,
      _gasrsAnomalySubscriptions = mempty
    }

-- | The token to retrieve the next set of results. AWS provides the token when the response from a previous call has more results than the maximum page size.
gasrsNextPageToken :: Lens' GetAnomalySubscriptionsResponse (Maybe Text)
gasrsNextPageToken = lens _gasrsNextPageToken (\s a -> s {_gasrsNextPageToken = a})

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetAnomalySubscriptionsResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\s a -> s {_gasrsResponseStatus = a})

-- | A list of cost anomaly subscriptions that includes the detailed metadata for each one.
gasrsAnomalySubscriptions :: Lens' GetAnomalySubscriptionsResponse [AnomalySubscription]
gasrsAnomalySubscriptions = lens _gasrsAnomalySubscriptions (\s a -> s {_gasrsAnomalySubscriptions = a}) . _Coerce

instance NFData GetAnomalySubscriptionsResponse

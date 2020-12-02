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
-- Module      : Network.AWS.AppStream.DescribeUsageReportSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more usage report subscriptions.
module Network.AWS.AppStream.DescribeUsageReportSubscriptions
  ( -- * Creating a Request
    describeUsageReportSubscriptions,
    DescribeUsageReportSubscriptions,

    -- * Request Lenses
    durssNextToken,
    durssMaxResults,

    -- * Destructuring the Response
    describeUsageReportSubscriptionsResponse,
    DescribeUsageReportSubscriptionsResponse,

    -- * Response Lenses
    durssrsUsageReportSubscriptions,
    durssrsNextToken,
    durssrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUsageReportSubscriptions' smart constructor.
data DescribeUsageReportSubscriptions = DescribeUsageReportSubscriptions'
  { _durssNextToken ::
      !(Maybe Text),
    _durssMaxResults ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsageReportSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'durssNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'durssMaxResults' - The maximum size of each page of results.
describeUsageReportSubscriptions ::
  DescribeUsageReportSubscriptions
describeUsageReportSubscriptions =
  DescribeUsageReportSubscriptions'
    { _durssNextToken = Nothing,
      _durssMaxResults = Nothing
    }

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
durssNextToken :: Lens' DescribeUsageReportSubscriptions (Maybe Text)
durssNextToken = lens _durssNextToken (\s a -> s {_durssNextToken = a})

-- | The maximum size of each page of results.
durssMaxResults :: Lens' DescribeUsageReportSubscriptions (Maybe Int)
durssMaxResults = lens _durssMaxResults (\s a -> s {_durssMaxResults = a})

instance AWSRequest DescribeUsageReportSubscriptions where
  type
    Rs DescribeUsageReportSubscriptions =
      DescribeUsageReportSubscriptionsResponse
  request = postJSON appStream
  response =
    receiveJSON
      ( \s h x ->
          DescribeUsageReportSubscriptionsResponse'
            <$> (x .?> "UsageReportSubscriptions" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeUsageReportSubscriptions

instance NFData DescribeUsageReportSubscriptions

instance ToHeaders DescribeUsageReportSubscriptions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "PhotonAdminProxyService.DescribeUsageReportSubscriptions" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeUsageReportSubscriptions where
  toJSON DescribeUsageReportSubscriptions' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _durssNextToken,
            ("MaxResults" .=) <$> _durssMaxResults
          ]
      )

instance ToPath DescribeUsageReportSubscriptions where
  toPath = const "/"

instance ToQuery DescribeUsageReportSubscriptions where
  toQuery = const mempty

-- | /See:/ 'describeUsageReportSubscriptionsResponse' smart constructor.
data DescribeUsageReportSubscriptionsResponse = DescribeUsageReportSubscriptionsResponse'
  { _durssrsUsageReportSubscriptions ::
      !( Maybe
           [UsageReportSubscription]
       ),
    _durssrsNextToken ::
      !( Maybe
           Text
       ),
    _durssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUsageReportSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'durssrsUsageReportSubscriptions' - Information about the usage report subscription.
--
-- * 'durssrsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'durssrsResponseStatus' - -- | The response status code.
describeUsageReportSubscriptionsResponse ::
  -- | 'durssrsResponseStatus'
  Int ->
  DescribeUsageReportSubscriptionsResponse
describeUsageReportSubscriptionsResponse pResponseStatus_ =
  DescribeUsageReportSubscriptionsResponse'
    { _durssrsUsageReportSubscriptions =
        Nothing,
      _durssrsNextToken = Nothing,
      _durssrsResponseStatus = pResponseStatus_
    }

-- | Information about the usage report subscription.
durssrsUsageReportSubscriptions :: Lens' DescribeUsageReportSubscriptionsResponse [UsageReportSubscription]
durssrsUsageReportSubscriptions = lens _durssrsUsageReportSubscriptions (\s a -> s {_durssrsUsageReportSubscriptions = a}) . _Default . _Coerce

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
durssrsNextToken :: Lens' DescribeUsageReportSubscriptionsResponse (Maybe Text)
durssrsNextToken = lens _durssrsNextToken (\s a -> s {_durssrsNextToken = a})

-- | -- | The response status code.
durssrsResponseStatus :: Lens' DescribeUsageReportSubscriptionsResponse Int
durssrsResponseStatus = lens _durssrsResponseStatus (\s a -> s {_durssrsResponseStatus = a})

instance NFData DescribeUsageReportSubscriptionsResponse

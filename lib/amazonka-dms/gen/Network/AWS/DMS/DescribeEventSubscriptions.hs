{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for a customer account. The description of a subscription includes @SubscriptionName@ , @SNSTopicARN@ , @CustomerID@ , @SourceType@ , @SourceID@ , @CreationTime@ , and @Status@ .
--
--
-- If you specify @SubscriptionName@ , this action lists the description for that subscription.
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEventSubscriptions
    (
    -- * Creating a Request
      describeEventSubscriptions
    , DescribeEventSubscriptions
    -- * Request Lenses
    , dessSubscriptionName
    , dessFilters
    , dessMarker
    , dessMaxRecords

    -- * Destructuring the Response
    , describeEventSubscriptionsResponse
    , DescribeEventSubscriptionsResponse
    -- * Response Lenses
    , desrsEventSubscriptionsList
    , desrsMarker
    , desrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { _dessSubscriptionName :: !(Maybe Text)
  , _dessFilters          :: !(Maybe [Filter])
  , _dessMarker           :: !(Maybe Text)
  , _dessMaxRecords       :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dessSubscriptionName' - The name of the AWS DMS event subscription to be described.
--
-- * 'dessFilters' - Filters applied to the action.
--
-- * 'dessMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dessMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeEventSubscriptions
    :: DescribeEventSubscriptions
describeEventSubscriptions =
  DescribeEventSubscriptions'
    { _dessSubscriptionName = Nothing
    , _dessFilters = Nothing
    , _dessMarker = Nothing
    , _dessMaxRecords = Nothing
    }


-- | The name of the AWS DMS event subscription to be described.
dessSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
dessSubscriptionName = lens _dessSubscriptionName (\ s a -> s{_dessSubscriptionName = a})

-- | Filters applied to the action.
dessFilters :: Lens' DescribeEventSubscriptions [Filter]
dessFilters = lens _dessFilters (\ s a -> s{_dessFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dessMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
dessMarker = lens _dessMarker (\ s a -> s{_dessMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dessMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
dessMaxRecords = lens _dessMaxRecords (\ s a -> s{_dessMaxRecords = a})

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrsMarker) = Nothing
          | stop (rs ^. desrsEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & dessMarker .~ rs ^. desrsMarker

instance AWSRequest DescribeEventSubscriptions where
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventSubscriptionsResponse' <$>
                   (x .?> "EventSubscriptionsList" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventSubscriptions where

instance NFData DescribeEventSubscriptions where

instance ToHeaders DescribeEventSubscriptions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeEventSubscriptions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventSubscriptions where
        toJSON DescribeEventSubscriptions'{..}
          = object
              (catMaybes
                 [("SubscriptionName" .=) <$> _dessSubscriptionName,
                  ("Filters" .=) <$> _dessFilters,
                  ("Marker" .=) <$> _dessMarker,
                  ("MaxRecords" .=) <$> _dessMaxRecords])

instance ToPath DescribeEventSubscriptions where
        toPath = const "/"

instance ToQuery DescribeEventSubscriptions where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { _desrsEventSubscriptionsList :: !(Maybe [EventSubscription])
  , _desrsMarker                 :: !(Maybe Text)
  , _desrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsEventSubscriptionsList' - A list of event subscriptions.
--
-- * 'desrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeEventSubscriptionsResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse pResponseStatus_ =
  DescribeEventSubscriptionsResponse'
    { _desrsEventSubscriptionsList = Nothing
    , _desrsMarker = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | A list of event subscriptions.
desrsEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrsEventSubscriptionsList = lens _desrsEventSubscriptionsList (\ s a -> s{_desrsEventSubscriptionsList = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
desrsMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrsMarker = lens _desrsMarker (\ s a -> s{_desrsMarker = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeEventSubscriptionsResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeEventSubscriptionsResponse
         where

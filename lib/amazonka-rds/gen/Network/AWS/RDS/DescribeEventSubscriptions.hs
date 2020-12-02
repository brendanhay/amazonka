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
-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the subscription descriptions for a customer account. The description for a subscription includes SubscriptionName, SNSTopicARN, CustomerID, SourceType, SourceID, CreationTime, and Status.
--
--
-- If you specify a SubscriptionName, lists the description for that subscription.
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEventSubscriptions
    (
    -- * Creating a Request
      describeEventSubscriptions
    , DescribeEventSubscriptions
    -- * Request Lenses
    , dSubscriptionName
    , dFilters
    , dMarker
    , dMaxRecords

    -- * Destructuring the Response
    , describeEventSubscriptionsResponse
    , DescribeEventSubscriptionsResponse
    -- * Response Lenses
    , desrsEventSubscriptionsList
    , desrsMarker
    , desrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { _dSubscriptionName :: !(Maybe Text)
  , _dFilters          :: !(Maybe [Filter])
  , _dMarker           :: !(Maybe Text)
  , _dMaxRecords       :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSubscriptionName' - The name of the RDS event notification subscription you want to describe.
--
-- * 'dFilters' - This parameter is not currently supported.
--
-- * 'dMarker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeEventSubscriptions
    :: DescribeEventSubscriptions
describeEventSubscriptions =
  DescribeEventSubscriptions'
    { _dSubscriptionName = Nothing
    , _dFilters = Nothing
    , _dMarker = Nothing
    , _dMaxRecords = Nothing
    }


-- | The name of the RDS event notification subscription you want to describe.
dSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
dSubscriptionName = lens _dSubscriptionName (\ s a -> s{_dSubscriptionName = a})

-- | This parameter is not currently supported.
dFilters :: Lens' DescribeEventSubscriptions [Filter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a})

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrsMarker) = Nothing
          | stop (rs ^. desrsEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & dMarker .~ rs ^. desrsMarker

instance AWSRequest DescribeEventSubscriptions where
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeEventSubscriptionsResult"
              (\ s h x ->
                 DescribeEventSubscriptionsResponse' <$>
                   (x .@? "EventSubscriptionsList" .!@ mempty >>=
                      may (parseXMLList "EventSubscription"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventSubscriptions where

instance NFData DescribeEventSubscriptions where

instance ToHeaders DescribeEventSubscriptions where
        toHeaders = const mempty

instance ToPath DescribeEventSubscriptions where
        toPath = const "/"

instance ToQuery DescribeEventSubscriptions where
        toQuery DescribeEventSubscriptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEventSubscriptions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SubscriptionName" =: _dSubscriptionName,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dFilters),
               "Marker" =: _dMarker, "MaxRecords" =: _dMaxRecords]

-- | Data returned by the __DescribeEventSubscriptions__ action.
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
-- * 'desrsEventSubscriptionsList' - A list of EventSubscriptions data types.
--
-- * 'desrsMarker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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


-- | A list of EventSubscriptions data types.
desrsEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrsEventSubscriptionsList = lens _desrsEventSubscriptionsList (\ s a -> s{_desrsEventSubscriptionsList = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
desrsMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrsMarker = lens _desrsMarker (\ s a -> s{_desrsMarker = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeEventSubscriptionsResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeEventSubscriptionsResponse
         where

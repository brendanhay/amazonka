{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Lists all the subscription descriptions for a customer account. The
-- description for a subscription includes SubscriptionName, SNSTopicARN,
-- CustomerID, SourceType, SourceID, CreationTime, and Status.
--
-- If you specify a SubscriptionName, lists the description for that
-- subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEventSubscriptions.html>
module Network.AWS.RDS.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , describeEventSubscriptions
    -- ** Request lenses
    , dSubscriptionName
    , dFilters
    , dMaxRecords
    , dMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , describeEventSubscriptionsResponse
    -- ** Response lenses
    , desrEventSubscriptionsList
    , desrMarker
    , desrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventSubscriptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dSubscriptionName'
--
-- * 'dFilters'
--
-- * 'dMaxRecords'
--
-- * 'dMarker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'
    { _dSubscriptionName :: !(Maybe Text)
    , _dFilters          :: !(Maybe [Filter])
    , _dMaxRecords       :: !(Maybe Int)
    , _dMarker           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions =
    DescribeEventSubscriptions'
    { _dSubscriptionName = Nothing
    , _dFilters = Nothing
    , _dMaxRecords = Nothing
    , _dMarker = Nothing
    }

-- | The name of the RDS event notification subscription you want to
-- describe.
dSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
dSubscriptionName = lens _dSubscriptionName (\ s a -> s{_dSubscriptionName = a});

-- | This parameter is not currently supported.
dFilters :: Lens' DescribeEventSubscriptions [Filter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
dMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrMarker) = Nothing
          | stop (rs ^. desrEventSubscriptionsList) = Nothing
          | otherwise = Just $ rq & dMarker .~ rs ^. desrMarker

instance AWSRequest DescribeEventSubscriptions where
        type Sv DescribeEventSubscriptions = RDS
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeEventSubscriptionsResult"
              (\ s h x ->
                 DescribeEventSubscriptionsResponse' <$>
                   (x .@? "EventSubscriptionsList" .!@ mempty >>=
                      may (parseXMLList "EventSubscription"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _dMaxRecords, "Marker" =: _dMarker]

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'describeEventSubscriptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrEventSubscriptionsList'
--
-- * 'desrMarker'
--
-- * 'desrStatus'
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
    { _desrEventSubscriptionsList :: !(Maybe [EventSubscription])
    , _desrMarker                 :: !(Maybe Text)
    , _desrStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: Int -> DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse pStatus =
    DescribeEventSubscriptionsResponse'
    { _desrEventSubscriptionsList = Nothing
    , _desrMarker = Nothing
    , _desrStatus = pStatus
    }

-- | A list of EventSubscriptions data types.
desrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrEventSubscriptionsList = lens _desrEventSubscriptionsList (\ s a -> s{_desrEventSubscriptionsList = a}) . _Default;

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
desrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrMarker = lens _desrMarker (\ s a -> s{_desrMarker = a});

-- | FIXME: Undocumented member.
desrStatus :: Lens' DescribeEventSubscriptionsResponse Int
desrStatus = lens _desrStatus (\ s a -> s{_desrStatus = a});

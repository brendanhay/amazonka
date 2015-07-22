{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all the subscription descriptions for a customer account. The
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
    , drqSubscriptionName
    , drqFilters
    , drqMaxRecords
    , drqMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , describeEventSubscriptionsResponse
    -- ** Response lenses
    , drsEventSubscriptionsList
    , drsMarker
    , drsStatus
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
-- * 'drqSubscriptionName'
--
-- * 'drqFilters'
--
-- * 'drqMaxRecords'
--
-- * 'drqMarker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'
    { _drqSubscriptionName :: !(Maybe Text)
    , _drqFilters          :: !(Maybe [Filter])
    , _drqMaxRecords       :: !(Maybe Int)
    , _drqMarker           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions =
    DescribeEventSubscriptions'
    { _drqSubscriptionName = Nothing
    , _drqFilters = Nothing
    , _drqMaxRecords = Nothing
    , _drqMarker = Nothing
    }

-- | The name of the RDS event notification subscription you want to
-- describe.
drqSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
drqSubscriptionName = lens _drqSubscriptionName (\ s a -> s{_drqSubscriptionName = a});

-- | This parameter is not currently supported.
drqFilters :: Lens' DescribeEventSubscriptions [Filter]
drqFilters = lens _drqFilters (\ s a -> s{_drqFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
drqMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
drqMaxRecords = lens _drqMaxRecords (\ s a -> s{_drqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
drqMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
drqMarker = lens _drqMarker (\ s a -> s{_drqMarker = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. drsMarker) = Nothing
          | stop (rs ^. drsEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & drqMarker .~ rs ^. drsMarker

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
               "SubscriptionName" =: _drqSubscriptionName,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drqFilters),
               "MaxRecords" =: _drqMaxRecords,
               "Marker" =: _drqMarker]

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'describeEventSubscriptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsEventSubscriptionsList'
--
-- * 'drsMarker'
--
-- * 'drsStatus'
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
    { _drsEventSubscriptionsList :: !(Maybe [EventSubscription])
    , _drsMarker                 :: !(Maybe Text)
    , _drsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: Int -> DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse pStatus =
    DescribeEventSubscriptionsResponse'
    { _drsEventSubscriptionsList = Nothing
    , _drsMarker = Nothing
    , _drsStatus = pStatus
    }

-- | A list of EventSubscriptions data types.
drsEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
drsEventSubscriptionsList = lens _drsEventSubscriptionsList (\ s a -> s{_drsEventSubscriptionsList = a}) . _Default;

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
drsMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
drsMarker = lens _drsMarker (\ s a -> s{_drsMarker = a});

-- | FIXME: Undocumented member.
drsStatus :: Lens' DescribeEventSubscriptionsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});

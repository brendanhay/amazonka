{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeEventSubscriptions.html>
module Network.AWS.Redshift.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , describeEventSubscriptions
    -- ** Request lenses
    , des1SubscriptionName
    , des1MaxRecords
    , des1Marker

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
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventSubscriptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'des1SubscriptionName'
--
-- * 'des1MaxRecords'
--
-- * 'des1Marker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'
    { _des1SubscriptionName :: !(Maybe Text)
    , _des1MaxRecords       :: !(Maybe Int)
    , _des1Marker           :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions =
    DescribeEventSubscriptions'
    { _des1SubscriptionName = Nothing
    , _des1MaxRecords = Nothing
    , _des1Marker = Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
des1SubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
des1SubscriptionName = lens _des1SubscriptionName (\ s a -> s{_des1SubscriptionName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
des1MaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
des1MaxRecords = lens _des1MaxRecords (\ s a -> s{_des1MaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
des1Marker :: Lens' DescribeEventSubscriptions (Maybe Text)
des1Marker = lens _des1Marker (\ s a -> s{_des1Marker = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrMarker) = Nothing
          | stop (rs ^. desrEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & des1Marker .~ rs ^. desrMarker

instance AWSRequest DescribeEventSubscriptions where
        type Sv DescribeEventSubscriptions = Redshift
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
                     <*> (pure s))

instance ToHeaders DescribeEventSubscriptions where
        toHeaders = const mempty

instance ToPath DescribeEventSubscriptions where
        toPath = const "/"

instance ToQuery DescribeEventSubscriptions where
        toQuery DescribeEventSubscriptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEventSubscriptions" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "SubscriptionName" =: _des1SubscriptionName,
               "MaxRecords" =: _des1MaxRecords,
               "Marker" =: _des1Marker]

-- |
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
    , _desrStatus                 :: !Status
    } deriving (Eq,Show)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: Status -> DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse pStatus =
    DescribeEventSubscriptionsResponse'
    { _desrEventSubscriptionsList = Nothing
    , _desrMarker = Nothing
    , _desrStatus = pStatus
    }

-- | A list of event subscriptions.
desrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrEventSubscriptionsList = lens _desrEventSubscriptionsList (\ s a -> s{_desrEventSubscriptionsList = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
desrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrMarker = lens _desrMarker (\ s a -> s{_desrMarker = a});

-- | FIXME: Undocumented member.
desrStatus :: Lens' DescribeEventSubscriptionsResponse Status
desrStatus = lens _desrStatus (\ s a -> s{_desrStatus = a});

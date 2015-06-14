{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , describeSubscriptionName
    , describeMaxRecords
    , describeMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , describeEventSubscriptionsResponse
    -- ** Response lenses
    , desrEventSubscriptionsList
    , desrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'describeEventSubscriptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'describeSubscriptionName'
--
-- * 'describeMaxRecords'
--
-- * 'describeMarker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'{_describeSubscriptionName :: Maybe Text, _describeMaxRecords :: Maybe Int, _describeMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions'{_describeSubscriptionName = Nothing, _describeMaxRecords = Nothing, _describeMarker = Nothing};

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
describeSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
describeSubscriptionName = lens _describeSubscriptionName (\ s a -> s{_describeSubscriptionName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
describeMaxRecords = lens _describeMaxRecords (\ s a -> s{_describeMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
describeMarker = lens _describeMarker (\ s a -> s{_describeMarker = a});

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
                      parseXMLList "EventSubscription")
                     <*> x .@? "Marker")

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
               "SubscriptionName" =: _describeSubscriptionName,
               "MaxRecords" =: _describeMaxRecords,
               "Marker" =: _describeMarker]

-- | /See:/ 'describeEventSubscriptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrEventSubscriptionsList'
--
-- * 'desrMarker'
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'{_desrEventSubscriptionsList :: [EventSubscription], _desrMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'{_desrEventSubscriptionsList = mempty, _desrMarker = Nothing};

-- | A list of event subscriptions.
desrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrEventSubscriptionsList = lens _desrEventSubscriptionsList (\ s a -> s{_desrEventSubscriptionsList = a});

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
desrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrMarker = lens _desrMarker (\ s a -> s{_desrMarker = a});

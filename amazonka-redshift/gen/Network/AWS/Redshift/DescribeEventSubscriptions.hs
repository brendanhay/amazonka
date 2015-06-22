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
    , descSubscriptionName
    , descMaxRecords
    , descMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , describeEventSubscriptionsResponse
    -- ** Response lenses
    , desrEventSubscriptionsList
    , desrMarker
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventSubscriptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descSubscriptionName'
--
-- * 'descMaxRecords'
--
-- * 'descMarker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'{_descSubscriptionName :: Maybe Text, _descMaxRecords :: Maybe Int, _descMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions'{_descSubscriptionName = Nothing, _descMaxRecords = Nothing, _descMarker = Nothing};

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
descSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
descSubscriptionName = lens _descSubscriptionName (\ s a -> s{_descSubscriptionName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
descMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
descMaxRecords = lens _descMaxRecords (\ s a -> s{_descMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
descMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
descMarker = lens _descMarker (\ s a -> s{_descMarker = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrMarker) = Nothing
          | stop (rs ^. desrEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & descMarker .~ rs ^. desrMarker

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
                     <*> (x .@? "Marker"))

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
               "SubscriptionName" =: _descSubscriptionName,
               "MaxRecords" =: _descMaxRecords,
               "Marker" =: _descMarker]

-- | /See:/ 'describeEventSubscriptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrEventSubscriptionsList'
--
-- * 'desrMarker'
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'{_desrEventSubscriptionsList :: Maybe [EventSubscription], _desrMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'{_desrEventSubscriptionsList = Nothing, _desrMarker = Nothing};

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

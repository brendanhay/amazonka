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
-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEventSubscriptions
    (
    -- * Creating a Request
      describeEventSubscriptions
    , DescribeEventSubscriptions
    -- * Request Lenses
    , dessSubscriptionName
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

import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
    { _dessSubscriptionName :: !(Maybe Text)
    , _dessMarker           :: !(Maybe Text)
    , _dessMaxRecords       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEventSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dessSubscriptionName'
--
-- * 'dessMarker'
--
-- * 'dessMaxRecords'
describeEventSubscriptions
    :: DescribeEventSubscriptions
describeEventSubscriptions =
    DescribeEventSubscriptions'
    { _dessSubscriptionName = Nothing
    , _dessMarker = Nothing
    , _dessMaxRecords = Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
dessSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
dessSubscriptionName = lens _dessSubscriptionName (\ s a -> s{_dessSubscriptionName = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a < DescribeEventSubscriptions>
-- request exceed the value specified in 'MaxRecords', AWS returns a value
-- in the 'Marker' field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the 'Marker'
-- parameter and retrying the request.
dessMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
dessMarker = lens _dessMarker (\ s a -> s{_dessMarker = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified 'MaxRecords'
-- value, a value is returned in a 'marker' field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dessMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
dessMaxRecords = lens _dessMaxRecords (\ s a -> s{_dessMaxRecords = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrsMarker) = Nothing
          | stop (rs ^. desrsEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & dessMarker .~ rs ^. desrsMarker

instance AWSRequest DescribeEventSubscriptions where
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        request = postQuery redshift
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
               "Version" =: ("2012-12-01" :: ByteString),
               "SubscriptionName" =: _dessSubscriptionName,
               "Marker" =: _dessMarker,
               "MaxRecords" =: _dessMaxRecords]

-- |
--
-- /See:/ 'describeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
    { _desrsEventSubscriptionsList :: !(Maybe [EventSubscription])
    , _desrsMarker                 :: !(Maybe Text)
    , _desrsResponseStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeEventSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsEventSubscriptionsList'
--
-- * 'desrsMarker'
--
-- * 'desrsResponseStatus'
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
desrsEventSubscriptionsList = lens _desrsEventSubscriptionsList (\ s a -> s{_desrsEventSubscriptionsList = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the 'Marker' parameter and retrying the command. If the
-- 'Marker' field is empty, all response records have been retrieved for
-- the request.
desrsMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrsMarker = lens _desrsMarker (\ s a -> s{_desrsMarker = a});

-- | The response status code.
desrsResponseStatus :: Lens' DescribeEventSubscriptionsResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});

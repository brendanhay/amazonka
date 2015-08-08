{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeEventSubscriptions.html AWS API Reference> for DescribeEventSubscriptions.
module Network.AWS.Redshift.DescribeEventSubscriptions
    (
    -- * Creating a Request
      DescribeEventSubscriptions
    , describeEventSubscriptions
    -- * Request Lenses
    , dSubscriptionName
    , dMaxRecords
    , dMarker

    -- * Destructuring the Response
    , DescribeEventSubscriptionsResponse
    , describeEventSubscriptionsResponse
    -- * Response Lenses
    , desrsEventSubscriptionsList
    , desrsMarker
    , desrsStatus
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
-- * 'dSubscriptionName'
--
-- * 'dMaxRecords'
--
-- * 'dMarker'
data DescribeEventSubscriptions = DescribeEventSubscriptions'
    { _dSubscriptionName :: !(Maybe Text)
    , _dMaxRecords       :: !(Maybe Int)
    , _dMarker           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptions' smart constructor.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions =
    DescribeEventSubscriptions'
    { _dSubscriptionName = Nothing
    , _dMaxRecords = Nothing
    , _dMarker = Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
dSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
dSubscriptionName = lens _dSubscriptionName (\ s a -> s{_dSubscriptionName = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a});

instance AWSPager DescribeEventSubscriptions where
        page rq rs
          | stop (rs ^. desrsMarker) = Nothing
          | stop (rs ^. desrsEventSubscriptionsList) = Nothing
          | otherwise =
            Just $ rq & dMarker .~ rs ^. desrsMarker

instance AWSRequest DescribeEventSubscriptions where
        type Sv DescribeEventSubscriptions = Redshift
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        request = postQuery
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
               "SubscriptionName" =: _dSubscriptionName,
               "MaxRecords" =: _dMaxRecords, "Marker" =: _dMarker]

-- |
--
-- /See:/ 'describeEventSubscriptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrsEventSubscriptionsList'
--
-- * 'desrsMarker'
--
-- * 'desrsStatus'
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
    { _desrsEventSubscriptionsList :: !(Maybe [EventSubscription])
    , _desrsMarker                 :: !(Maybe Text)
    , _desrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEventSubscriptionsResponse' smart constructor.
describeEventSubscriptionsResponse :: Int -> DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse pStatus_ =
    DescribeEventSubscriptionsResponse'
    { _desrsEventSubscriptionsList = Nothing
    , _desrsMarker = Nothing
    , _desrsStatus = pStatus_
    }

-- | A list of event subscriptions.
desrsEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrsEventSubscriptionsList = lens _desrsEventSubscriptionsList (\ s a -> s{_desrsEventSubscriptionsList = a}) . _Default . _Coerce;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
desrsMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrsMarker = lens _desrsMarker (\ s a -> s{_desrsMarker = a});

-- | Undocumented member.
desrsStatus :: Lens' DescribeEventSubscriptionsResponse Int
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});

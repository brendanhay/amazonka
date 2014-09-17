{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
module Network.AWS.Redshift.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , mkDescribeEventSubscriptions
    -- ** Request lenses
    , des1SubscriptionName
    , des1MaxRecords
    , des1Marker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , mkDescribeEventSubscriptionsResponse
    -- ** Response lenses
    , desrMarker
    , desrEventSubscriptionsList
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data DescribeEventSubscriptions = DescribeEventSubscriptions
    { _des1SubscriptionName :: Maybe Text
    , _des1MaxRecords :: Maybe Integer
    , _des1Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventSubscriptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionName ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeEventSubscriptions :: DescribeEventSubscriptions
mkDescribeEventSubscriptions = DescribeEventSubscriptions
    { _des1SubscriptionName = Nothing
    , _des1MaxRecords = Nothing
    , _des1Marker = Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
des1SubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
des1SubscriptionName =
    lens _des1SubscriptionName (\s a -> s { _des1SubscriptionName = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
des1MaxRecords :: Lens' DescribeEventSubscriptions (Maybe Integer)
des1MaxRecords = lens _des1MaxRecords (\s a -> s { _des1MaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeEventSubscriptions request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
des1Marker :: Lens' DescribeEventSubscriptions (Maybe Text)
des1Marker = lens _des1Marker (\s a -> s { _des1Marker = a })

instance ToQuery DescribeEventSubscriptions where
    toQuery = genericQuery def

-- | 
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrMarker :: Maybe Text
    , _desrEventSubscriptionsList :: [EventSubscription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventSubscriptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @EventSubscriptionsList ::@ @[EventSubscription]@
--
mkDescribeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse
mkDescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrMarker = Nothing
    , _desrEventSubscriptionsList = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
desrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrMarker = lens _desrMarker (\s a -> s { _desrMarker = a })

-- | A list of event subscriptions.
desrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrEventSubscriptionsList =
    lens _desrEventSubscriptionsList
         (\s a -> s { _desrEventSubscriptionsList = a })

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = Redshift
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request = post "DescribeEventSubscriptions"
    response _ = xmlResponse

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq & des1Marker ?~ x)
        <$> (rs ^. desrMarker)

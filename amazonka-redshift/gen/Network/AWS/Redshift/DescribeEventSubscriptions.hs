{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      DescribeEventSubscriptionsMessage
    -- ** Request constructor
    , describeEventSubscriptionsMessage
    -- ** Request lenses
    , desmMarker
    , desmMaxRecords
    , desmSubscriptionName

    -- * Response
    , EventSubscriptionsMessage
    -- ** Response constructor
    , eventSubscriptionsMessage
    -- ** Response lenses
    , esmEventSubscriptionsList
    , esmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage
    { _desmMarker           :: Maybe Text
    , _desmMaxRecords       :: Maybe Int
    , _desmSubscriptionName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEventSubscriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmMarker' @::@ 'Maybe' 'Text'
--
-- * 'desmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'desmSubscriptionName' @::@ 'Maybe' 'Text'
--
describeEventSubscriptionsMessage :: DescribeEventSubscriptionsMessage
describeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage
    { _desmSubscriptionName = Nothing
    , _desmMaxRecords       = Nothing
    , _desmMarker           = Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
desmMarker :: Lens' DescribeEventSubscriptionsMessage (Maybe Text)
desmMarker = lens _desmMarker (\s a -> s { _desmMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
desmMaxRecords :: Lens' DescribeEventSubscriptionsMessage (Maybe Int)
desmMaxRecords = lens _desmMaxRecords (\s a -> s { _desmMaxRecords = a })

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
desmSubscriptionName :: Lens' DescribeEventSubscriptionsMessage (Maybe Text)
desmSubscriptionName =
    lens _desmSubscriptionName (\s a -> s { _desmSubscriptionName = a })
instance ToQuery DescribeEventSubscriptionsMessage

instance ToPath DescribeEventSubscriptionsMessage where
    toPath = const "/"

data EventSubscriptionsMessage = EventSubscriptionsMessage
    { _esmEventSubscriptionsList :: [EventSubscription]
    , _esmMarker                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EventSubscriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esmEventSubscriptionsList' @::@ ['EventSubscription']
--
-- * 'esmMarker' @::@ 'Maybe' 'Text'
--
eventSubscriptionsMessage :: EventSubscriptionsMessage
eventSubscriptionsMessage = EventSubscriptionsMessage
    { _esmMarker                 = Nothing
    , _esmEventSubscriptionsList = mempty
    }

-- | A list of event subscriptions.
esmEventSubscriptionsList :: Lens' EventSubscriptionsMessage [EventSubscription]
esmEventSubscriptionsList =
    lens _esmEventSubscriptionsList
        (\s a -> s { _esmEventSubscriptionsList = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
esmMarker :: Lens' EventSubscriptionsMessage (Maybe Text)
esmMarker = lens _esmMarker (\s a -> s { _esmMarker = a })
instance FromXML EventSubscriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscriptionsMessage"

instance AWSRequest DescribeEventSubscriptionsMessage where
    type Sv DescribeEventSubscriptionsMessage = Redshift
    type Rs DescribeEventSubscriptionsMessage = EventSubscriptionsMessage

    request  = post "DescribeEventSubscriptions"
    response = xmlResponse $ \h x -> EventSubscriptionsMessage
        <$> x %| "EventSubscriptionsList"
        <*> x %| "Marker"

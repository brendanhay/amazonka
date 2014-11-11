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

-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all the subscription descriptions for a customer account. The
-- description for a subscription includes SubscriptionName, SNSTopicARN,
-- CustomerID, SourceType, SourceID, CreationTime, and Status. If you specify
-- a SubscriptionName, lists the description for that subscription.
module Network.AWS.RDS.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptionsMessage
    -- ** Request constructor
    , describeEventSubscriptionsMessage
    -- ** Request lenses
    , desmFilters
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
import Network.AWS.RDS.Types

data DescribeEventSubscriptionsMessage = DescribeEventSubscriptionsMessage
    { _desmFilters          :: [Filter]
    , _desmMarker           :: Maybe Text
    , _desmMaxRecords       :: Maybe Int
    , _desmSubscriptionName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeEventSubscriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmFilters' @::@ ['Filter']
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
    , _desmFilters          = mempty
    , _desmMaxRecords       = Nothing
    , _desmMarker           = Nothing
    }

-- | This parameter is not currently supported.
desmFilters :: Lens' DescribeEventSubscriptionsMessage [Filter]
desmFilters = lens _desmFilters (\s a -> s { _desmFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords .
desmMarker :: Lens' DescribeEventSubscriptionsMessage (Maybe Text)
desmMarker = lens _desmMarker (\s a -> s { _desmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
desmMaxRecords :: Lens' DescribeEventSubscriptionsMessage (Maybe Int)
desmMaxRecords = lens _desmMaxRecords (\s a -> s { _desmMaxRecords = a })

-- | The name of the RDS event notification subscription you want to describe.
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

-- | A list of EventSubscriptions data types.
esmEventSubscriptionsList :: Lens' EventSubscriptionsMessage [EventSubscription]
esmEventSubscriptionsList =
    lens _esmEventSubscriptionsList
        (\s a -> s { _esmEventSubscriptionsList = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
esmMarker :: Lens' EventSubscriptionsMessage (Maybe Text)
esmMarker = lens _esmMarker (\s a -> s { _esmMarker = a })
instance FromXML EventSubscriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscriptionsMessage"

instance AWSRequest DescribeEventSubscriptionsMessage where
    type Sv DescribeEventSubscriptionsMessage = RDS
    type Rs DescribeEventSubscriptionsMessage = EventSubscriptionsMessage

    request  = post "DescribeEventSubscriptions"
    response = xmlResponse $ \h x -> EventSubscriptionsMessage
        <$> x %| "EventSubscriptionsList"
        <*> x %| "Marker"

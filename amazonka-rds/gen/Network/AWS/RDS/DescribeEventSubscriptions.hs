{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEventSubscriptions.html>
module Network.AWS.RDS.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , describeEventSubscriptions
    -- ** Request lenses
    , des1Filters
    , des1Marker
    , des1MaxRecords
    , des1SubscriptionName

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response constructor
    , describeEventSubscriptionsResponse
    -- ** Response lenses
    , desrEventSubscriptionsList
    , desrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { _des1Filters          :: List "Filter" Filter
    , _des1Marker           :: Maybe Text
    , _des1MaxRecords       :: Maybe Int
    , _des1SubscriptionName :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeEventSubscriptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'des1Filters' @::@ ['Filter']
--
-- * 'des1Marker' @::@ 'Maybe' 'Text'
--
-- * 'des1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'des1SubscriptionName' @::@ 'Maybe' 'Text'
--
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions
    { _des1SubscriptionName = Nothing
    , _des1Filters          = mempty
    , _des1MaxRecords       = Nothing
    , _des1Marker           = Nothing
    }

-- | This parameter is not currently supported.
des1Filters :: Lens' DescribeEventSubscriptions [Filter]
des1Filters = lens _des1Filters (\s a -> s { _des1Filters = a }) . _List

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords .
des1Marker :: Lens' DescribeEventSubscriptions (Maybe Text)
des1Marker = lens _des1Marker (\s a -> s { _des1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
des1MaxRecords :: Lens' DescribeEventSubscriptions (Maybe Int)
des1MaxRecords = lens _des1MaxRecords (\s a -> s { _des1MaxRecords = a })

-- | The name of the RDS event notification subscription you want to describe.
des1SubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
des1SubscriptionName =
    lens _des1SubscriptionName (\s a -> s { _des1SubscriptionName = a })

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrEventSubscriptionsList :: List "EventSubscription" EventSubscription
    , _desrMarker                 :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeEventSubscriptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrEventSubscriptionsList' @::@ ['EventSubscription']
--
-- * 'desrMarker' @::@ 'Maybe' 'Text'
--
describeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrMarker                 = Nothing
    , _desrEventSubscriptionsList = mempty
    }

-- | A list of EventSubscriptions data types.
desrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrEventSubscriptionsList =
    lens _desrEventSubscriptionsList
        (\s a -> s { _desrEventSubscriptionsList = a })
            . _List

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
desrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrMarker = lens _desrMarker (\s a -> s { _desrMarker = a })

instance ToPath DescribeEventSubscriptions where
    toPath = const "/"

instance ToQuery DescribeEventSubscriptions where
    toQuery DescribeEventSubscriptions{..} = mconcat
        [ "Filters"          =? _des1Filters
        , "Marker"           =? _des1Marker
        , "MaxRecords"       =? _des1MaxRecords
        , "SubscriptionName" =? _des1SubscriptionName
        ]

instance ToHeaders DescribeEventSubscriptions

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = RDS
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request  = post "DescribeEventSubscriptions"
    response = xmlResponse

instance FromXML DescribeEventSubscriptionsResponse where
    parseXML = withElement "DescribeEventSubscriptionsResult" $ \x -> DescribeEventSubscriptionsResponse
        <$> x .@  "EventSubscriptionsList"
        <*> x .@? "Marker"

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq & des1Marker ?~ x)
        <$> (rs ^. desrMarker)

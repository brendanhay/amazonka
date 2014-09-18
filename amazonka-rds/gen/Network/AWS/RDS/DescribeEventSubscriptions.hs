{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventSubscriptions
-- &MaxRecords=100 &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T004543Z &AWSAccessKeyId=
-- &Signature= true 012345678901 active 2013-01-28 00:29:23.736
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- true 012345678901 active 2013-01-28 00:29:42.851 EventSubscription02
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 0ce48079-68e4-11e2-91fe-5daa8e68c7d4.
module Network.AWS.RDS.DescribeEventSubscriptions
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
    , desrrMarker
    , desrrEventSubscriptionsList
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
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
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions
    { _des1SubscriptionName = Nothing
    , _des1MaxRecords = Nothing
    , _des1Marker = Nothing
    }

-- | The name of the RDS event notification subscription you want to describe.
des1SubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
des1SubscriptionName =
    lens _des1SubscriptionName (\s a -> s { _des1SubscriptionName = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
des1MaxRecords :: Lens' DescribeEventSubscriptions (Maybe Integer)
des1MaxRecords = lens _des1MaxRecords (\s a -> s { _des1MaxRecords = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
des1Marker :: Lens' DescribeEventSubscriptions (Maybe Text)
des1Marker = lens _des1Marker (\s a -> s { _des1Marker = a })

instance ToQuery DescribeEventSubscriptions where
    toQuery = genericQuery def

-- | Data returned by the DescribeEventSubscriptions action.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrrMarker :: Maybe Text
    , _desrrEventSubscriptionsList :: [EventSubscription]
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
describeEventSubscriptionsResponse :: DescribeEventSubscriptionsResponse
describeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _desrrMarker = Nothing
    , _desrrEventSubscriptionsList = mempty
    }

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
desrrMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
desrrMarker = lens _desrrMarker (\s a -> s { _desrrMarker = a })

-- | A list of EventSubscriptions data types.
desrrEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse [EventSubscription]
desrrEventSubscriptionsList =
    lens _desrrEventSubscriptionsList
         (\s a -> s { _desrrEventSubscriptionsList = a })

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = RDS
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request = post "DescribeEventSubscriptions"
    response _ = xmlResponse

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq & des1Marker ?~ x)
        <$> (rs ^. desrrMarker)

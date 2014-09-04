{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions
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
module Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , mkDescribeEventSubscriptionsMessage
    -- ** Request lenses
    , desnSubscriptionName
    , desnMaxRecords
    , desnMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response lenses
    , esmMarker
    , esmEventSubscriptionsList
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventSubscriptions' request.
mkDescribeEventSubscriptionsMessage :: DescribeEventSubscriptions
mkDescribeEventSubscriptionsMessage = DescribeEventSubscriptions
    { _desnSubscriptionName = Nothing
    , _desnMaxRecords = Nothing
    , _desnMarker = Nothing
    }
{-# INLINE mkDescribeEventSubscriptionsMessage #-}

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { _desnSubscriptionName :: Maybe Text
      -- ^ The name of the RDS event notification subscription you want to
      -- describe.
    , _desnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _desnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords .
    } deriving (Show, Generic)

-- | The name of the RDS event notification subscription you want to describe.
desnSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
desnSubscriptionName = lens _desnSubscriptionName (\s a -> s { _desnSubscriptionName = a })
{-# INLINE desnSubscriptionName #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
desnMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Integer)
desnMaxRecords = lens _desnMaxRecords (\s a -> s { _desnMaxRecords = a })
{-# INLINE desnMaxRecords #-}

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
desnMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
desnMarker = lens _desnMarker (\s a -> s { _desnMarker = a })
{-# INLINE desnMarker #-}

instance ToQuery DescribeEventSubscriptions where
    toQuery = genericQuery def

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _esmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords.
    , _esmEventSubscriptionsList :: [EventSubscription]
      -- ^ A list of EventSubscriptions data types.
    } deriving (Show, Generic)

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
esmMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
esmMarker = lens _esmMarker (\s a -> s { _esmMarker = a })
{-# INLINE esmMarker #-}

-- | A list of EventSubscriptions data types.
esmEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse ([EventSubscription])
esmEventSubscriptionsList = lens _esmEventSubscriptionsList (\s a -> s { _esmEventSubscriptionsList = a })
{-# INLINE esmEventSubscriptionsList #-}

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = RDS
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request = post "DescribeEventSubscriptions"
    response _ = xmlResponse

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq { _desnMarker = Just x })
        <$> (_esmMarker rs)

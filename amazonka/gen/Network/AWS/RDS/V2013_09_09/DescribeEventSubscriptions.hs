{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEventSubscriptions' request.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions
    { _desnMaxRecords = Nothing
    , _desnSubscriptionName = Nothing
    , _desnMarker = Nothing
    }

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { _desnMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _desnSubscriptionName :: Maybe Text
      -- ^ The name of the RDS event notification subscription you want to
      -- describe.
    , _desnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords .
    } deriving (Show, Generic)

makeLenses ''DescribeEventSubscriptions

instance ToQuery DescribeEventSubscriptions where
    toQuery = genericToQuery def

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _esmEventSubscriptionsList :: [EventSubscription]
      -- ^ A list of EventSubscriptions data types.
    , _esmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords.
    } deriving (Show, Generic)

makeLenses ''DescribeEventSubscriptionsResponse

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = RDS
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request = post "DescribeEventSubscriptions"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeEventSubscriptionsResponse
            <*> xml %| "EventSubscriptionsList"
            <*> xml %|? "String"

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq { _desnMarker = Just x })
        <$> (_esmMarker rs)

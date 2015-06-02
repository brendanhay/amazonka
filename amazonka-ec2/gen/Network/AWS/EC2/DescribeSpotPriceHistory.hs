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

-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the Spot Price history. The prices returned are listed in
-- chronological order, from the oldest to the most recent, for up to the past
-- 90 days. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance Pricing History> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- When you specify a start and end time, this operation returns the prices of
-- the instance types within the time range that you specified and the time when
-- the price changed. The price is valid within the time period that you
-- specified; the response merely indicates the last time that the price changed.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotPriceHistory.html>
module Network.AWS.EC2.DescribeSpotPriceHistory
    (
    -- * Request
      DescribeSpotPriceHistory
    -- ** Request constructor
    , describeSpotPriceHistory
    -- ** Request lenses
    , dsphAvailabilityZone
    , dsphDryRun
    , dsphEndTime
    , dsphFilters
    , dsphInstanceTypes
    , dsphMaxResults
    , dsphNextToken
    , dsphProductDescriptions
    , dsphStartTime

    -- * Response
    , DescribeSpotPriceHistoryResponse
    -- ** Response constructor
    , describeSpotPriceHistoryResponse
    -- ** Response lenses
    , dsphrNextToken
    , dsphrSpotPriceHistory
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphAvailabilityZone    :: Maybe Text
    , _dsphDryRun              :: Maybe Bool
    , _dsphEndTime             :: Maybe ISO8601
    , _dsphFilters             :: List "Filter" Filter
    , _dsphInstanceTypes       :: List "InstanceType" InstanceType
    , _dsphMaxResults          :: Maybe Int
    , _dsphNextToken           :: Maybe Text
    , _dsphProductDescriptions :: List "ProductDescription" Text
    , _dsphStartTime           :: Maybe ISO8601
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotPriceHistory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'dsphDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsphEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsphFilters' @::@ ['Filter']
--
-- * 'dsphInstanceTypes' @::@ ['InstanceType']
--
-- * 'dsphMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dsphNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsphProductDescriptions' @::@ ['Text']
--
-- * 'dsphStartTime' @::@ 'Maybe' 'UTCTime'
--
describeSpotPriceHistory :: DescribeSpotPriceHistory
describeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphDryRun              = Nothing
    , _dsphStartTime           = Nothing
    , _dsphEndTime             = Nothing
    , _dsphInstanceTypes       = mempty
    , _dsphProductDescriptions = mempty
    , _dsphFilters             = mempty
    , _dsphAvailabilityZone    = Nothing
    , _dsphMaxResults          = Nothing
    , _dsphNextToken           = Nothing
    }

-- | Filters the results by the specified Availability Zone.
dsphAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphAvailabilityZone =
    lens _dsphAvailabilityZone (\s a -> s { _dsphAvailabilityZone = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dsphDryRun :: Lens' DescribeSpotPriceHistory (Maybe Bool)
dsphDryRun = lens _dsphDryRun (\s a -> s { _dsphDryRun = a })

-- | The date and time, up to the current date, from which to stop retrieving the
-- price history data, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphEndTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphEndTime = lens _dsphEndTime (\s a -> s { _dsphEndTime = a }) . mapping _Time

-- | One or more filters.
--
-- 'availability-zone' - The Availability Zone for which prices should be
-- returned.
--
-- 'instance-type' - The type of instance (for example, 'm1.small').
--
-- 'product-description' - The product description for the Spot Price ('Linux/UNIX' | 'SUSE Linux' | 'Windows' | 'Linux/UNIX (Amazon VPC)' | 'SUSE Linux (Amazon VPC)' | 'Windows (Amazon VPC)').
--
-- 'spot-price' - The Spot Price. The value must match exactly (or use
-- wildcards; greater than or less than comparison is not supported).
--
-- 'timestamp' - The timestamp of the Spot Price history, in UTC format (for
-- example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). You can use wildcards (* and ?). Greater than
-- or less than comparison is not supported.
--
--
dsphFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphFilters = lens _dsphFilters (\s a -> s { _dsphFilters = a }) . _List

-- | Filters the results by the specified instance types.
dsphInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphInstanceTypes =
    lens _dsphInstanceTypes (\s a -> s { _dsphInstanceTypes = a })
        . _List

-- | The maximum number of results to return in a single call. Specify a value
-- between 1 and 1000. The default value is 1000. To retrieve the remaining
-- results, make another call with the returned 'NextToken' value.
dsphMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Int)
dsphMaxResults = lens _dsphMaxResults (\s a -> s { _dsphMaxResults = a })

-- | The token for the next set of results.
dsphNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphNextToken = lens _dsphNextToken (\s a -> s { _dsphNextToken = a })

-- | Filters the results by the specified basic product descriptions.
dsphProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphProductDescriptions =
    lens _dsphProductDescriptions (\s a -> s { _dsphProductDescriptions = a })
        . _List

-- | The date and time, up to the past 90 days, from which to start retrieving the
-- price history data, in UTC format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphStartTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphStartTime = lens _dsphStartTime (\s a -> s { _dsphStartTime = a }) . mapping _Time

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphrNextToken        :: Maybe Text
    , _dsphrSpotPriceHistory :: List "item" SpotPrice
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotPriceHistoryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsphrSpotPriceHistory' @::@ ['SpotPrice']
--
describeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse
describeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphrSpotPriceHistory = mempty
    , _dsphrNextToken        = Nothing
    }

-- | The token required to retrieve the next set of results. This value is 'null'
-- when there are no more results to return.
dsphrNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphrNextToken = lens _dsphrNextToken (\s a -> s { _dsphrNextToken = a })

-- | The historical Spot Prices.
dsphrSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse [SpotPrice]
dsphrSpotPriceHistory =
    lens _dsphrSpotPriceHistory (\s a -> s { _dsphrSpotPriceHistory = a })
        . _List

instance ToPath DescribeSpotPriceHistory where
    toPath = const "/"

instance ToQuery DescribeSpotPriceHistory where
    toQuery DescribeSpotPriceHistory{..} = mconcat
        [ "AvailabilityZone"   =? _dsphAvailabilityZone
        , "DryRun"             =? _dsphDryRun
        , "EndTime"            =? _dsphEndTime
        , "Filter"             `toQueryList` _dsphFilters
        , "InstanceType"       `toQueryList` _dsphInstanceTypes
        , "MaxResults"         =? _dsphMaxResults
        , "NextToken"          =? _dsphNextToken
        , "ProductDescription" `toQueryList` _dsphProductDescriptions
        , "StartTime"          =? _dsphStartTime
        ]

instance ToHeaders DescribeSpotPriceHistory

instance AWSRequest DescribeSpotPriceHistory where
    type Sv DescribeSpotPriceHistory = EC2
    type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse

    request  = post "DescribeSpotPriceHistory"
    response = xmlResponse

instance FromXML DescribeSpotPriceHistoryResponse where
    parseXML x = DescribeSpotPriceHistoryResponse
        <$> x .@? "nextToken"
        <*> x .@? "spotPriceHistorySet" .!@ mempty

instance AWSPager DescribeSpotPriceHistory where
    page rq rs
        | stop (rs ^. dsphrNextToken) = Nothing
        | otherwise = (\x -> rq & dsphNextToken ?~ x)
            <$> (rs ^. dsphrNextToken)

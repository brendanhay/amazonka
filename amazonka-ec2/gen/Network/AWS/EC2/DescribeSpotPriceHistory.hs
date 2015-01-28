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

-- | Describes the Spot Price history. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds the
-- current Spot Price. Amazon EC2 periodically sets the Spot Price based on
-- available Spot Instance capacity and current Spot Instance requests. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance Pricing History> in the /Amazon Elastic ComputeCloud User Guide for Linux/.
--
-- When you specify an Availability Zone, this operation describes the price
-- history for the specified Availability Zone with the most recent set of
-- prices listed first. If you don't specify an Availability Zone, you get the
-- prices across all Availability Zones, starting with the most recent set.
-- However, if you're using an API version earlier than 2011-05-15, you get the
-- lowest price across the region for the specified time period. The prices
-- returned are listed in chronological order, from the oldest to the most
-- recent.
--
-- When you specify the start and end time options, this operation returns two
-- pieces of data: the prices of the instance types within the time range that
-- you specified and the time when the price changed. The price is valid within
-- the time period that you specified; the response merely indicates the last
-- time that the price changed.
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

-- | The Availability Zone.
dsphAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphAvailabilityZone =
    lens _dsphAvailabilityZone (\s a -> s { _dsphAvailabilityZone = a })

dsphDryRun :: Lens' DescribeSpotPriceHistory (Maybe Bool)
dsphDryRun = lens _dsphDryRun (\s a -> s { _dsphDryRun = a })

-- | The end date and time of the Spot Price history data.
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
-- 'timestamp' - The timestamp of the Spot Price history (for example,
-- 2010-08-16T05:06:11.000Z). You can use wildcards (* and ?). Greater than or
-- less than comparison is not supported.
--
--
dsphFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphFilters = lens _dsphFilters (\s a -> s { _dsphFilters = a }) . _List

-- | One or more instance types.
dsphInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphInstanceTypes =
    lens _dsphInstanceTypes (\s a -> s { _dsphInstanceTypes = a })
        . _List

-- | The number of rows to return.
dsphMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Int)
dsphMaxResults = lens _dsphMaxResults (\s a -> s { _dsphMaxResults = a })

-- | The next set of rows to return.
dsphNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphNextToken = lens _dsphNextToken (\s a -> s { _dsphNextToken = a })

-- | One or more basic product descriptions.
dsphProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphProductDescriptions =
    lens _dsphProductDescriptions (\s a -> s { _dsphProductDescriptions = a })
        . _List

-- | The start date and time of the Spot Price history data.
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

-- | The string marking the next set of results. This is empty if there are no
-- more results.
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

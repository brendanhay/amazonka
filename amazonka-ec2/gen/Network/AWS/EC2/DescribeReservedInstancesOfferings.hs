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

-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
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

-- | Describes Reserved Instance offerings that are available for purchase. With
-- Reserved Instances, you purchase the right to launch instances for a period
-- of time. During that time period, you do not receive insufficient capacity
-- errors, and you pay a lower usage rate than the rate charged for On-Demand
-- instances for the actual time used.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /AmazonElastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesOfferings.html>
module Network.AWS.EC2.DescribeReservedInstancesOfferings
    (
    -- * Request
      DescribeReservedInstancesOfferings
    -- ** Request constructor
    , describeReservedInstancesOfferings
    -- ** Request lenses
    , drioAvailabilityZone
    , drioDryRun
    , drioFilters
    , drioIncludeMarketplace
    , drioInstanceTenancy
    , drioInstanceType
    , drioMaxDuration
    , drioMaxInstanceCount
    , drioMaxResults
    , drioMinDuration
    , drioNextToken
    , drioOfferingType
    , drioProductDescription
    , drioReservedInstancesOfferingIds

    -- * Response
    , DescribeReservedInstancesOfferingsResponse
    -- ** Response constructor
    , describeReservedInstancesOfferingsResponse
    -- ** Response lenses
    , driorNextToken
    , driorReservedInstancesOfferings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _drioAvailabilityZone             :: Maybe Text
    , _drioDryRun                       :: Maybe Bool
    , _drioFilters                      :: List "Filter" Filter
    , _drioIncludeMarketplace           :: Maybe Bool
    , _drioInstanceTenancy              :: Maybe Tenancy
    , _drioInstanceType                 :: Maybe InstanceType
    , _drioMaxDuration                  :: Maybe Integer
    , _drioMaxInstanceCount             :: Maybe Int
    , _drioMaxResults                   :: Maybe Int
    , _drioMinDuration                  :: Maybe Integer
    , _drioNextToken                    :: Maybe Text
    , _drioOfferingType                 :: Maybe OfferingTypeValues
    , _drioProductDescription           :: Maybe RIProductDescription
    , _drioReservedInstancesOfferingIds :: List "ReservedInstancesOfferingId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeReservedInstancesOfferings' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drioAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'drioDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'drioFilters' @::@ ['Filter']
--
-- * 'drioIncludeMarketplace' @::@ 'Maybe' 'Bool'
--
-- * 'drioInstanceTenancy' @::@ 'Maybe' 'Tenancy'
--
-- * 'drioInstanceType' @::@ 'Maybe' 'InstanceType'
--
-- * 'drioMaxDuration' @::@ 'Maybe' 'Integer'
--
-- * 'drioMaxInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'drioMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'drioMinDuration' @::@ 'Maybe' 'Integer'
--
-- * 'drioNextToken' @::@ 'Maybe' 'Text'
--
-- * 'drioOfferingType' @::@ 'Maybe' 'OfferingTypeValues'
--
-- * 'drioProductDescription' @::@ 'Maybe' 'RIProductDescription'
--
-- * 'drioReservedInstancesOfferingIds' @::@ ['Text']
--
describeReservedInstancesOfferings :: DescribeReservedInstancesOfferings
describeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _drioDryRun                       = Nothing
    , _drioReservedInstancesOfferingIds = mempty
    , _drioInstanceType                 = Nothing
    , _drioAvailabilityZone             = Nothing
    , _drioProductDescription           = Nothing
    , _drioFilters                      = mempty
    , _drioInstanceTenancy              = Nothing
    , _drioOfferingType                 = Nothing
    , _drioNextToken                    = Nothing
    , _drioMaxResults                   = Nothing
    , _drioIncludeMarketplace           = Nothing
    , _drioMinDuration                  = Nothing
    , _drioMaxDuration                  = Nothing
    , _drioMaxInstanceCount             = Nothing
    }

-- | The Availability Zone in which the Reserved Instance can be used.
drioAvailabilityZone :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioAvailabilityZone =
    lens _drioAvailabilityZone (\s a -> s { _drioAvailabilityZone = a })

drioDryRun :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
drioDryRun = lens _drioDryRun (\s a -> s { _drioDryRun = a })

-- | One or more filters.
--
-- 'availability-zone' - The Availability Zone where the Reserved Instance can
-- be used.
--
-- 'duration' - The duration of the Reserved Instance (for example, one year or
-- three years), in seconds ('31536000' | '94608000').
--
-- 'fixed-price' - The purchase price of the Reserved Instance (for example,
-- 9800.0).
--
-- 'instance-type' - The instance type on which the Reserved Instance can be
-- used.
--
-- 'marketplace' - Set to 'true' to show only Reserved Instance Marketplace
-- offerings. When this filter is not used, which is the default behavior, all
-- offerings from AWS and Reserved Instance Marketplace are listed.
--
-- 'product-description' - The description of the Reserved Instance ('Linux/UNIX'
-- | 'Linux/UNIX (Amazon VPC)' | 'Windows' | 'Windows (Amazon VPC)').
--
-- 'reserved-instances-offering-id' - The Reserved Instances offering ID.
--
-- 'usage-price' - The usage price of the Reserved Instance, per hour (for
-- example, 0.84).
--
--
drioFilters :: Lens' DescribeReservedInstancesOfferings [Filter]
drioFilters = lens _drioFilters (\s a -> s { _drioFilters = a }) . _List

-- | Include Marketplace offerings in the response.
drioIncludeMarketplace :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
drioIncludeMarketplace =
    lens _drioIncludeMarketplace (\s a -> s { _drioIncludeMarketplace = a })

-- | The tenancy of the Reserved Instance offering. A Reserved Instance with 'dedicated' tenancy runs on single-tenant hardware and can only be launched within a VPC.
--
-- Default: 'default'
drioInstanceTenancy :: Lens' DescribeReservedInstancesOfferings (Maybe Tenancy)
drioInstanceTenancy =
    lens _drioInstanceTenancy (\s a -> s { _drioInstanceTenancy = a })

-- | The instance type on which the Reserved Instance can be used. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud UserGuide for Linux/.
drioInstanceType :: Lens' DescribeReservedInstancesOfferings (Maybe InstanceType)
drioInstanceType = lens _drioInstanceType (\s a -> s { _drioInstanceType = a })

-- | The maximum duration (in seconds) to filter when searching for offerings.
--
-- Default: 94608000 (3 years)
drioMaxDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMaxDuration = lens _drioMaxDuration (\s a -> s { _drioMaxDuration = a })

-- | The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
drioMaxInstanceCount :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
drioMaxInstanceCount =
    lens _drioMaxInstanceCount (\s a -> s { _drioMaxInstanceCount = a })

-- | The maximum number of offerings to return. The maximum is 100.
--
-- Default: 100
drioMaxResults :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
drioMaxResults = lens _drioMaxResults (\s a -> s { _drioMaxResults = a })

-- | The minimum duration (in seconds) to filter when searching for offerings.
--
-- Default: 2592000 (1 month)
drioMinDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMinDuration = lens _drioMinDuration (\s a -> s { _drioMinDuration = a })

-- | The token to use when requesting the next paginated set of offerings.
drioNextToken :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioNextToken = lens _drioNextToken (\s a -> s { _drioNextToken = a })

-- | The Reserved Instance offering type. If you are using tools that predate the
-- 2011-11-01 API version, you only have access to the 'Medium Utilization'
-- Reserved Instance offering type.
drioOfferingType :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingTypeValues)
drioOfferingType = lens _drioOfferingType (\s a -> s { _drioOfferingType = a })

-- | The Reserved Instance description. Instances that include '(Amazon VPC)' in the
-- description are for use with Amazon VPC.
drioProductDescription :: Lens' DescribeReservedInstancesOfferings (Maybe RIProductDescription)
drioProductDescription =
    lens _drioProductDescription (\s a -> s { _drioProductDescription = a })

-- | One or more Reserved Instances offering IDs.
drioReservedInstancesOfferingIds :: Lens' DescribeReservedInstancesOfferings [Text]
drioReservedInstancesOfferingIds =
    lens _drioReservedInstancesOfferingIds
        (\s a -> s { _drioReservedInstancesOfferingIds = a })
            . _List

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { _driorNextToken                  :: Maybe Text
    , _driorReservedInstancesOfferings :: List "item" ReservedInstancesOffering
    } deriving (Eq, Read, Show)

-- | 'DescribeReservedInstancesOfferingsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'driorNextToken' @::@ 'Maybe' 'Text'
--
-- * 'driorReservedInstancesOfferings' @::@ ['ReservedInstancesOffering']
--
describeReservedInstancesOfferingsResponse :: DescribeReservedInstancesOfferingsResponse
describeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { _driorReservedInstancesOfferings = mempty
    , _driorNextToken                  = Nothing
    }

-- | The next paginated set of results to return.
driorNextToken :: Lens' DescribeReservedInstancesOfferingsResponse (Maybe Text)
driorNextToken = lens _driorNextToken (\s a -> s { _driorNextToken = a })

-- | A list of Reserved Instances offerings.
driorReservedInstancesOfferings :: Lens' DescribeReservedInstancesOfferingsResponse [ReservedInstancesOffering]
driorReservedInstancesOfferings =
    lens _driorReservedInstancesOfferings
        (\s a -> s { _driorReservedInstancesOfferings = a })
            . _List

instance ToPath DescribeReservedInstancesOfferings where
    toPath = const "/"

instance ToQuery DescribeReservedInstancesOfferings where
    toQuery DescribeReservedInstancesOfferings{..} = mconcat
        [ "AvailabilityZone"            =? _drioAvailabilityZone
        , "DryRun"                      =? _drioDryRun
        , "Filter"                      `toQueryList` _drioFilters
        , "IncludeMarketplace"          =? _drioIncludeMarketplace
        , "InstanceTenancy"             =? _drioInstanceTenancy
        , "InstanceType"                =? _drioInstanceType
        , "MaxDuration"                 =? _drioMaxDuration
        , "MaxInstanceCount"            =? _drioMaxInstanceCount
        , "MaxResults"                  =? _drioMaxResults
        , "MinDuration"                 =? _drioMinDuration
        , "NextToken"                   =? _drioNextToken
        , "OfferingType"                =? _drioOfferingType
        , "ProductDescription"          =? _drioProductDescription
        , "ReservedInstancesOfferingId" `toQueryList` _drioReservedInstancesOfferingIds
        ]

instance ToHeaders DescribeReservedInstancesOfferings

instance AWSRequest DescribeReservedInstancesOfferings where
    type Sv DescribeReservedInstancesOfferings = EC2
    type Rs DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsResponse

    request  = post "DescribeReservedInstancesOfferings"
    response = xmlResponse

instance FromXML DescribeReservedInstancesOfferingsResponse where
    parseXML x = DescribeReservedInstancesOfferingsResponse
        <$> x .@? "nextToken"
        <*> x .@? "reservedInstancesOfferingsSet" .!@ mempty

instance AWSPager DescribeReservedInstancesOfferings where
    page rq rs
        | stop (rs ^. driorNextToken) = Nothing
        | otherwise = (\x -> rq & drioNextToken ?~ x)
            <$> (rs ^. driorNextToken)

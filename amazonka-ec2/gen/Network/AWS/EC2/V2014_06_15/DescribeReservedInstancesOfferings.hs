{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes Reserved Instance offerings that are available for purchase. With
-- Reserved Instances, you purchase the right to launch instances for a period
-- of time. During that time period, you do not receive insufficient capacity
-- errors, and you pay a lower usage rate than the rate charged for On-Demand
-- instances for the actual time used. For more information, see Reserved
-- Instance Marketplace in the Amazon Elastic Compute Cloud User Guide.
-- Example Describing Reserved Instance Marketplace Offerings Only This
-- example requests a list of Linux/Unix, Light Utilization Reserved Instances
-- that are available through the Reserved Instance Marketplace only. When
-- using the Query API, all strings must be URL-encoded.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;Filter.0.Name=marketplace &amp;Filter.0.Value.1=true
-- &amp;IncludeMarketplace=true &amp;OfferingType=Light+Utilization
-- &amp;ProductDescription=Linux%2FUNIX &amp;Version=2013-10-01
-- &amp;AUTHPARAMS 2bc7dafa-dafd-4257-bdf9-c0814EXAMPLE
-- a6ce8269-7b8c-42cd-a7f5-0841cEXAMPLE m1.large us-east-1a 90720000 96.03
-- 0.027 Linux/UNIX default USD Light Utilization true 96.03 1
-- 2bc7dafa-dafd-4257-bdf9-c0814EXAMPLE m1.xlarge us-east-1b 28512000 61.0
-- 0.034 Linux/UNIX default USD Light Utilization Hourly 0.29 true 61.0 2
-- Example Describing AWS Offerings Only This example lists AWS offerings
-- only. https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;IncludeMarketplace=false &amp;AUTHPARAMS Example Using Tokens to
-- Manage Results You can use pagination support to query the results
-- sequentially and in parts. Specify the maximum number of results that are
-- returned in the response. Then, each paginated response contains a token
-- that can be provided as input to a subsequent
-- DescribeReservedInstancesOfferings call to fetch the next page. (Make sure
-- that you use URL encoding for the token value.)
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;MaxResults=5 &amp;AUTHPARAMS d072f652-cc57-458c-89e0-e6c02EXAMPLE ...
-- 649fd0c8-7846-46b8-8f84-a6400EXAMPLE m1.large us-east-1a 94608000 1200.0
-- 0.0 Linux/UNIX (Amazon VPC) default USD Heavy Utilization Hourly 0.052
-- false e5a2ff3b-a4f3-477c-8928-dbd00EXAMPLE m1.large us-east-1a 94608000
-- 1000.0 0.076 Linux/UNIX (Amazon VPC) default USD Medium Utilization false
-- ... h/C8YKPQBHEjW8xKz1827/Zzyb0VqsqkjRo3TqhFYeE=
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;MaxResults=5
-- &amp;NextToken=h%2FC8YKPQBHEjW8xKz1827%2FZzyb0VqsqkjRo3TqhFYeE%3D
-- &amp;AUTHPARAMS Example Using Filters This example filters the response to
-- include only one-year, m1.small or m1.large Linux/UNIX Reserved Instances.
-- If you want Linux/UNIX Reserved Instances specifically for use with a VPC,
-- set the product description to Linux/UNIX (Amazon VPC).
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesOfferings
-- &amp;Filter.1.Name=duration &amp;Filter.1.Value.1=31536000
-- &amp;Filter.2.Name=instance-type &amp;Filter.2.Value.1=m1.small
-- &amp;Filter.2.Value.2=m1.large &amp;Filter.3.Name=product-description
-- &amp;Filter.3.Value.1=Linux%2FUNIX &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesOfferings
    (
    -- * Request
      DescribeReservedInstancesOfferings
    -- ** Request constructor
    , mkDescribeReservedInstancesOfferings
    -- ** Request lenses
    , drioReservedInstancesOfferingIds
    , drioInstanceType
    , drioAvailabilityZone
    , drioProductDescription
    , drioFilters
    , drioInstanceTenancy
    , drioOfferingType
    , drioNextToken
    , drioMaxResults
    , drioIncludeMarketplace
    , drioMinDuration
    , drioMaxDuration
    , drioMaxInstanceCount

    -- * Response
    , DescribeReservedInstancesOfferingsResponse
    -- ** Response lenses
    , driorReservedInstancesOfferings
    , driorNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _drioReservedInstancesOfferingIds :: [Text]
    , _drioInstanceType :: Maybe InstanceType
    , _drioAvailabilityZone :: Maybe Text
    , _drioProductDescription :: Maybe RIProductDescription
    , _drioFilters :: [Filter]
    , _drioInstanceTenancy :: Maybe Tenancy
    , _drioOfferingType :: Maybe OfferingTypeValues
    , _drioNextToken :: Maybe Text
    , _drioMaxResults :: Maybe Integer
    , _drioIncludeMarketplace :: Maybe Bool
    , _drioMinDuration :: Maybe Integer
    , _drioMaxDuration :: Maybe Integer
    , _drioMaxInstanceCount :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstancesOfferings' request.
mkDescribeReservedInstancesOfferings :: DescribeReservedInstancesOfferings
mkDescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _drioReservedInstancesOfferingIds = mempty
    , _drioInstanceType = Nothing
    , _drioAvailabilityZone = Nothing
    , _drioProductDescription = Nothing
    , _drioFilters = mempty
    , _drioInstanceTenancy = Nothing
    , _drioOfferingType = Nothing
    , _drioNextToken = Nothing
    , _drioMaxResults = Nothing
    , _drioIncludeMarketplace = Nothing
    , _drioMinDuration = Nothing
    , _drioMaxDuration = Nothing
    , _drioMaxInstanceCount = Nothing
    }

-- | One or more Reserved Instances offering IDs.
drioReservedInstancesOfferingIds :: Lens' DescribeReservedInstancesOfferings [Text]
drioReservedInstancesOfferingIds =
    lens _drioReservedInstancesOfferingIds
         (\s a -> s { _drioReservedInstancesOfferingIds = a })

-- | The instance type on which the Reserved Instance can be used. For more
-- information, see Instance Types in the Amazon Elastic Compute Cloud User
-- Guide.
drioInstanceType :: Lens' DescribeReservedInstancesOfferings (Maybe InstanceType)
drioInstanceType =
    lens _drioInstanceType (\s a -> s { _drioInstanceType = a })

-- | The Availability Zone in which the Reserved Instance can be used.
drioAvailabilityZone :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioAvailabilityZone =
    lens _drioAvailabilityZone (\s a -> s { _drioAvailabilityZone = a })

-- | The Reserved Instance description. Instances that include (Amazon VPC) in
-- the description are for use with Amazon VPC.
drioProductDescription :: Lens' DescribeReservedInstancesOfferings (Maybe RIProductDescription)
drioProductDescription =
    lens _drioProductDescription (\s a -> s { _drioProductDescription = a })

-- | One or more filters. availability-zone - The Availability Zone where the
-- Reserved Instance can be used. duration - The duration of the Reserved
-- Instance (for example, one year or three years), in seconds. fixed-price -
-- The purchase price of the Reserved Instance (for example, 9800.0).
-- instance-type - The instance type on which the Reserved Instance can be
-- used. marketplace - Set to true to show only Reserved Instance Marketplace
-- offerings. When this filter is not used, which is the default behavior, all
-- offerings from AWS and Reserved Instance Marketplace are listed.
-- product-description - The description of the Reserved Instance (Linux/UNIX
-- | Linux/UNIX (Amazon VPC) | Windows | Windows (Amazon VPC)).
-- reserved-instances-offering-id - The Reserved Instances offering ID.
-- usage-price - The usage price of the Reserved Instance, per hour (for
-- example, 0.84).
drioFilters :: Lens' DescribeReservedInstancesOfferings [Filter]
drioFilters = lens _drioFilters (\s a -> s { _drioFilters = a })

-- | The tenancy of the Reserved Instance offering. A Reserved Instance with
-- dedicated tenancy runs on single-tenant hardware and can only be launched
-- within a VPC. Default: default.
drioInstanceTenancy :: Lens' DescribeReservedInstancesOfferings (Maybe Tenancy)
drioInstanceTenancy =
    lens _drioInstanceTenancy (\s a -> s { _drioInstanceTenancy = a })

-- | The Reserved Instance offering type.
drioOfferingType :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingTypeValues)
drioOfferingType =
    lens _drioOfferingType (\s a -> s { _drioOfferingType = a })

-- | The token to use when requesting the next paginated set of offerings.
drioNextToken :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioNextToken = lens _drioNextToken (\s a -> s { _drioNextToken = a })

-- | The maximum number of offerings to return.
drioMaxResults :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMaxResults = lens _drioMaxResults (\s a -> s { _drioMaxResults = a })

-- | Include Marketplace offerings in the response.
drioIncludeMarketplace :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
drioIncludeMarketplace =
    lens _drioIncludeMarketplace (\s a -> s { _drioIncludeMarketplace = a })

-- | The minimum duration (in seconds) to filter when searching for offerings.
drioMinDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMinDuration = lens _drioMinDuration (\s a -> s { _drioMinDuration = a })

-- | The maximum duration (in seconds) to filter when searching for offerings.
drioMaxDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMaxDuration = lens _drioMaxDuration (\s a -> s { _drioMaxDuration = a })

-- | The maximum number of instances to filter when searching for offerings.
drioMaxInstanceCount :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMaxInstanceCount =
    lens _drioMaxInstanceCount (\s a -> s { _drioMaxInstanceCount = a })

instance ToQuery DescribeReservedInstancesOfferings where
    toQuery = genericQuery def

-- | 
data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { _driorReservedInstancesOfferings :: [ReservedInstancesOffering]
    , _driorNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of Reserved Instances offerings.
driorReservedInstancesOfferings :: Lens' DescribeReservedInstancesOfferingsResponse [ReservedInstancesOffering]
driorReservedInstancesOfferings =
    lens _driorReservedInstancesOfferings
         (\s a -> s { _driorReservedInstancesOfferings = a })

-- | The next paginated set of results to return.
driorNextToken :: Lens' DescribeReservedInstancesOfferingsResponse (Maybe Text)
driorNextToken = lens _driorNextToken (\s a -> s { _driorNextToken = a })

instance FromXML DescribeReservedInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstancesOfferings where
    type Sv DescribeReservedInstancesOfferings = EC2
    type Rs DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsResponse

    request = post "DescribeReservedInstancesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedInstancesOfferings where
    next rq rs = (\x -> rq & drioNextToken ?~ x)
        <$> (rs ^. driorNextToken)

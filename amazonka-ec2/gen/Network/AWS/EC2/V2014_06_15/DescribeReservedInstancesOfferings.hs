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
    , mkDescribeReservedInstancesOfferingsRequest
    -- ** Request lenses
    , driorReservedInstancesOfferingIds
    , driorInstanceType
    , driorAvailabilityZone
    , driorProductDescription
    , driorFilters
    , driorInstanceTenancy
    , driorOfferingType
    , driorNextToken
    , driorMaxResults
    , driorIncludeMarketplace
    , driorMinDuration
    , driorMaxDuration
    , driorMaxInstanceCount

    -- * Response
    , DescribeReservedInstancesOfferingsResponse
    -- ** Response lenses
    , driosReservedInstancesOfferings
    , driosNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstancesOfferings' request.
mkDescribeReservedInstancesOfferingsRequest :: DescribeReservedInstancesOfferings
mkDescribeReservedInstancesOfferingsRequest = DescribeReservedInstancesOfferings
    { _driorReservedInstancesOfferingIds = mempty
    , _driorInstanceType = Nothing
    , _driorAvailabilityZone = Nothing
    , _driorProductDescription = Nothing
    , _driorFilters = mempty
    , _driorInstanceTenancy = Nothing
    , _driorOfferingType = Nothing
    , _driorNextToken = Nothing
    , _driorMaxResults = Nothing
    , _driorIncludeMarketplace = Nothing
    , _driorMinDuration = Nothing
    , _driorMaxDuration = Nothing
    , _driorMaxInstanceCount = Nothing
    }
{-# INLINE mkDescribeReservedInstancesOfferingsRequest #-}

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _driorReservedInstancesOfferingIds :: [Text]
      -- ^ One or more Reserved Instances offering IDs.
    , _driorInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used. For
      -- more information, see Instance Types in the Amazon Elastic
      -- Compute Cloud User Guide.
    , _driorAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _driorProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description. Instances that include (Amazon
      -- VPC) in the description are for use with Amazon VPC.
    , _driorFilters :: [Filter]
      -- ^ One or more filters. availability-zone - The Availability Zone
      -- where the Reserved Instance can be used. duration - The duration
      -- of the Reserved Instance (for example, one year or three years),
      -- in seconds. fixed-price - The purchase price of the Reserved
      -- Instance (for example, 9800.0). instance-type - The instance type
      -- on which the Reserved Instance can be used. marketplace - Set to
      -- true to show only Reserved Instance Marketplace offerings. When
      -- this filter is not used, which is the default behavior, all
      -- offerings from AWS and Reserved Instance Marketplace are listed.
      -- product-description - The description of the Reserved Instance
      -- (Linux/UNIX | Linux/UNIX (Amazon VPC) | Windows | Windows (Amazon
      -- VPC)). reserved-instances-offering-id - The Reserved Instances
      -- offering ID. usage-price - The usage price of the Reserved
      -- Instance, per hour (for example, 0.84).
    , _driorInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the Reserved Instance offering. A Reserved
      -- Instance with dedicated tenancy runs on single-tenant hardware
      -- and can only be launched within a VPC. Default: default.
    , _driorOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _driorNextToken :: Maybe Text
      -- ^ The token to use when requesting the next paginated set of
      -- offerings.
    , _driorMaxResults :: Maybe Integer
      -- ^ The maximum number of offerings to return.
    , _driorIncludeMarketplace :: Maybe Bool
      -- ^ Include Marketplace offerings in the response.
    , _driorMinDuration :: Maybe Integer
      -- ^ The minimum duration (in seconds) to filter when searching for
      -- offerings.
    , _driorMaxDuration :: Maybe Integer
      -- ^ The maximum duration (in seconds) to filter when searching for
      -- offerings.
    , _driorMaxInstanceCount :: Maybe Integer
      -- ^ The maximum number of instances to filter when searching for
      -- offerings.
    } deriving (Show, Generic)

-- | One or more Reserved Instances offering IDs.
driorReservedInstancesOfferingIds :: Lens' DescribeReservedInstancesOfferings ([Text])
driorReservedInstancesOfferingIds = lens _driorReservedInstancesOfferingIds (\s a -> s { _driorReservedInstancesOfferingIds = a })
{-# INLINE driorReservedInstancesOfferingIds #-}

-- | The instance type on which the Reserved Instance can be used. For more
-- information, see Instance Types in the Amazon Elastic Compute Cloud User
-- Guide.
driorInstanceType :: Lens' DescribeReservedInstancesOfferings (Maybe InstanceType)
driorInstanceType = lens _driorInstanceType (\s a -> s { _driorInstanceType = a })
{-# INLINE driorInstanceType #-}

-- | The Availability Zone in which the Reserved Instance can be used.
driorAvailabilityZone :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
driorAvailabilityZone = lens _driorAvailabilityZone (\s a -> s { _driorAvailabilityZone = a })
{-# INLINE driorAvailabilityZone #-}

-- | The Reserved Instance description. Instances that include (Amazon VPC) in
-- the description are for use with Amazon VPC.
driorProductDescription :: Lens' DescribeReservedInstancesOfferings (Maybe RIProductDescription)
driorProductDescription = lens _driorProductDescription (\s a -> s { _driorProductDescription = a })
{-# INLINE driorProductDescription #-}

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
driorFilters :: Lens' DescribeReservedInstancesOfferings ([Filter])
driorFilters = lens _driorFilters (\s a -> s { _driorFilters = a })
{-# INLINE driorFilters #-}

-- | The tenancy of the Reserved Instance offering. A Reserved Instance with
-- dedicated tenancy runs on single-tenant hardware and can only be launched
-- within a VPC. Default: default.
driorInstanceTenancy :: Lens' DescribeReservedInstancesOfferings (Maybe Tenancy)
driorInstanceTenancy = lens _driorInstanceTenancy (\s a -> s { _driorInstanceTenancy = a })
{-# INLINE driorInstanceTenancy #-}

-- | The Reserved Instance offering type.
driorOfferingType :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingTypeValues)
driorOfferingType = lens _driorOfferingType (\s a -> s { _driorOfferingType = a })
{-# INLINE driorOfferingType #-}

-- | The token to use when requesting the next paginated set of offerings.
driorNextToken :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
driorNextToken = lens _driorNextToken (\s a -> s { _driorNextToken = a })
{-# INLINE driorNextToken #-}

-- | The maximum number of offerings to return.
driorMaxResults :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorMaxResults = lens _driorMaxResults (\s a -> s { _driorMaxResults = a })
{-# INLINE driorMaxResults #-}

-- | Include Marketplace offerings in the response.
driorIncludeMarketplace :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
driorIncludeMarketplace = lens _driorIncludeMarketplace (\s a -> s { _driorIncludeMarketplace = a })
{-# INLINE driorIncludeMarketplace #-}

-- | The minimum duration (in seconds) to filter when searching for offerings.
driorMinDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorMinDuration = lens _driorMinDuration (\s a -> s { _driorMinDuration = a })
{-# INLINE driorMinDuration #-}

-- | The maximum duration (in seconds) to filter when searching for offerings.
driorMaxDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorMaxDuration = lens _driorMaxDuration (\s a -> s { _driorMaxDuration = a })
{-# INLINE driorMaxDuration #-}

-- | The maximum number of instances to filter when searching for offerings.
driorMaxInstanceCount :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorMaxInstanceCount = lens _driorMaxInstanceCount (\s a -> s { _driorMaxInstanceCount = a })
{-# INLINE driorMaxInstanceCount #-}

instance ToQuery DescribeReservedInstancesOfferings where
    toQuery = genericQuery def

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { _driosReservedInstancesOfferings :: [ReservedInstancesOffering]
      -- ^ A list of Reserved Instances offerings.
    , _driosNextToken :: Maybe Text
      -- ^ The next paginated set of results to return.
    } deriving (Show, Generic)

-- | A list of Reserved Instances offerings.
driosReservedInstancesOfferings :: Lens' DescribeReservedInstancesOfferingsResponse ([ReservedInstancesOffering])
driosReservedInstancesOfferings = lens _driosReservedInstancesOfferings (\s a -> s { _driosReservedInstancesOfferings = a })
{-# INLINE driosReservedInstancesOfferings #-}

-- | The next paginated set of results to return.
driosNextToken :: Lens' DescribeReservedInstancesOfferingsResponse (Maybe Text)
driosNextToken = lens _driosNextToken (\s a -> s { _driosNextToken = a })
{-# INLINE driosNextToken #-}

instance FromXML DescribeReservedInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstancesOfferings where
    type Sv DescribeReservedInstancesOfferings = EC2
    type Rs DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsResponse

    request = post "DescribeReservedInstancesOfferings"
    response _ = xmlResponse

instance AWSPager DescribeReservedInstancesOfferings where
    next rq rs = (\x -> rq { _driorNextToken = Just x })
        <$> (_driosNextToken rs)

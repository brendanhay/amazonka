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
    , describeReservedInstancesOfferings
    -- ** Request lenses
    , driorIncludeMarketplace
    , driorFilters
    , driorInstanceType
    , driorMaxResults
    , driorMaxInstanceCount
    , driorMinDuration
    , driorMaxDuration
    , driorOfferingType
    , driorProductDescription
    , driorReservedInstancesOfferingIds
    , driorInstanceTenancy
    , driorAvailabilityZone
    , driorNextToken

    -- * Response
    , DescribeReservedInstancesOfferingsResponse
    -- ** Response lenses
    , driosReservedInstancesOfferings
    , driosNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedInstancesOfferings' request.
describeReservedInstancesOfferings :: DescribeReservedInstancesOfferings
describeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _driorIncludeMarketplace = Nothing
    , _driorFilters = mempty
    , _driorInstanceType = Nothing
    , _driorMaxResults = Nothing
    , _driorMaxInstanceCount = Nothing
    , _driorMinDuration = Nothing
    , _driorMaxDuration = Nothing
    , _driorOfferingType = Nothing
    , _driorProductDescription = Nothing
    , _driorReservedInstancesOfferingIds = mempty
    , _driorInstanceTenancy = Nothing
    , _driorAvailabilityZone = Nothing
    , _driorNextToken = Nothing
    }

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { _driorIncludeMarketplace :: Maybe Bool
      -- ^ Include Marketplace offerings in the response.
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
    , _driorInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used. For
      -- more information, see Instance Types in the Amazon Elastic
      -- Compute Cloud User Guide.
    , _driorMaxResults :: Maybe Integer
      -- ^ The maximum number of offerings to return.
    , _driorMaxInstanceCount :: Maybe Integer
      -- ^ The maximum number of instances to filter when searching for
      -- offerings.
    , _driorMinDuration :: Maybe Integer
      -- ^ The minimum duration (in seconds) to filter when searching for
      -- offerings.
    , _driorMaxDuration :: Maybe Integer
      -- ^ The maximum duration (in seconds) to filter when searching for
      -- offerings.
    , _driorOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , _driorProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance description. Instances that include (Amazon
      -- VPC) in the description are for use with Amazon VPC.
    , _driorReservedInstancesOfferingIds :: [Text]
      -- ^ One or more Reserved Instances offering IDs.
    , _driorInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the Reserved Instance offering. A Reserved
      -- Instance with dedicated tenancy runs on single-tenant hardware
      -- and can only be launched within a VPC. Default: default.
    , _driorAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , _driorNextToken :: Maybe Text
      -- ^ The token to use when requesting the next paginated set of
      -- offerings.
    } deriving (Show, Generic)

-- | Include Marketplace offerings in the response.
driorIncludeMarketplace
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorIncludeMarketplace f x =
    (\y -> x { _driorIncludeMarketplace = y })
       <$> f (_driorIncludeMarketplace x)
{-# INLINE driorIncludeMarketplace #-}

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
driorFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorFilters f x =
    (\y -> x { _driorFilters = y })
       <$> f (_driorFilters x)
{-# INLINE driorFilters #-}

-- | The instance type on which the Reserved Instance can be used. For more
-- information, see Instance Types in the Amazon Elastic Compute Cloud User
-- Guide.
driorInstanceType
    :: Functor f
    => (Maybe InstanceType
    -> f (Maybe InstanceType))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorInstanceType f x =
    (\y -> x { _driorInstanceType = y })
       <$> f (_driorInstanceType x)
{-# INLINE driorInstanceType #-}

-- | The maximum number of offerings to return.
driorMaxResults
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorMaxResults f x =
    (\y -> x { _driorMaxResults = y })
       <$> f (_driorMaxResults x)
{-# INLINE driorMaxResults #-}

-- | The maximum number of instances to filter when searching for offerings.
driorMaxInstanceCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorMaxInstanceCount f x =
    (\y -> x { _driorMaxInstanceCount = y })
       <$> f (_driorMaxInstanceCount x)
{-# INLINE driorMaxInstanceCount #-}

-- | The minimum duration (in seconds) to filter when searching for offerings.
driorMinDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorMinDuration f x =
    (\y -> x { _driorMinDuration = y })
       <$> f (_driorMinDuration x)
{-# INLINE driorMinDuration #-}

-- | The maximum duration (in seconds) to filter when searching for offerings.
driorMaxDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorMaxDuration f x =
    (\y -> x { _driorMaxDuration = y })
       <$> f (_driorMaxDuration x)
{-# INLINE driorMaxDuration #-}

-- | The Reserved Instance offering type.
driorOfferingType
    :: Functor f
    => (Maybe OfferingTypeValues
    -> f (Maybe OfferingTypeValues))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorOfferingType f x =
    (\y -> x { _driorOfferingType = y })
       <$> f (_driorOfferingType x)
{-# INLINE driorOfferingType #-}

-- | The Reserved Instance description. Instances that include (Amazon VPC) in
-- the description are for use with Amazon VPC.
driorProductDescription
    :: Functor f
    => (Maybe RIProductDescription
    -> f (Maybe RIProductDescription))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorProductDescription f x =
    (\y -> x { _driorProductDescription = y })
       <$> f (_driorProductDescription x)
{-# INLINE driorProductDescription #-}

-- | One or more Reserved Instances offering IDs.
driorReservedInstancesOfferingIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorReservedInstancesOfferingIds f x =
    (\y -> x { _driorReservedInstancesOfferingIds = y })
       <$> f (_driorReservedInstancesOfferingIds x)
{-# INLINE driorReservedInstancesOfferingIds #-}

-- | The tenancy of the Reserved Instance offering. A Reserved Instance with
-- dedicated tenancy runs on single-tenant hardware and can only be launched
-- within a VPC. Default: default.
driorInstanceTenancy
    :: Functor f
    => (Maybe Tenancy
    -> f (Maybe Tenancy))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorInstanceTenancy f x =
    (\y -> x { _driorInstanceTenancy = y })
       <$> f (_driorInstanceTenancy x)
{-# INLINE driorInstanceTenancy #-}

-- | The Availability Zone in which the Reserved Instance can be used.
driorAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorAvailabilityZone f x =
    (\y -> x { _driorAvailabilityZone = y })
       <$> f (_driorAvailabilityZone x)
{-# INLINE driorAvailabilityZone #-}

-- | The token to use when requesting the next paginated set of offerings.
driorNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedInstancesOfferings
    -> f DescribeReservedInstancesOfferings
driorNextToken f x =
    (\y -> x { _driorNextToken = y })
       <$> f (_driorNextToken x)
{-# INLINE driorNextToken #-}

instance ToQuery DescribeReservedInstancesOfferings where
    toQuery = genericQuery def

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { _driosReservedInstancesOfferings :: [ReservedInstancesOffering]
      -- ^ A list of Reserved Instances offerings.
    , _driosNextToken :: Maybe Text
      -- ^ The next paginated set of results to return.
    } deriving (Show, Generic)

-- | A list of Reserved Instances offerings.
driosReservedInstancesOfferings
    :: Functor f
    => ([ReservedInstancesOffering]
    -> f ([ReservedInstancesOffering]))
    -> DescribeReservedInstancesOfferingsResponse
    -> f DescribeReservedInstancesOfferingsResponse
driosReservedInstancesOfferings f x =
    (\y -> x { _driosReservedInstancesOfferings = y })
       <$> f (_driosReservedInstancesOfferings x)
{-# INLINE driosReservedInstancesOfferings #-}

-- | The next paginated set of results to return.
driosNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedInstancesOfferingsResponse
    -> f DescribeReservedInstancesOfferingsResponse
driosNextToken f x =
    (\y -> x { _driosNextToken = y })
       <$> f (_driosNextToken x)
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

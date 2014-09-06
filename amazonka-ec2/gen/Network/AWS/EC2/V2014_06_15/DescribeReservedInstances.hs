{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeReservedInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the Reserved Instances that you purchased. For
-- more information about Reserved Instances, see Reserved Instances in the
-- Amazon Elastic Compute Cloud User Guide. Example This example describes
-- Reserved Instances owned by your account.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstances &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ...
-- e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE m1.xlarge us-east-1b 31536000 61.0
-- 0.034 3 Linux/UNIX active default USD Light Utilization ... Example This
-- example filters the response to include only one-year, m1.small Linux/UNIX
-- Reserved Instances. If you want Linux/UNIX Reserved Instances specifically
-- for use with a VPC, set the product description to Linux/UNIX (Amazon VPC).
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstances
-- &amp;Filter.1.Name=duration &amp;Filter.1.Value.1=31536000
-- &amp;Filter.2.Name=instance-type &amp;Filter.2.Value.1=m1.small
-- &amp;Filter.3.Name=product-description &amp;Filter.3.Value.1=Linux%2FUNIX
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeReservedInstances
    (
    -- * Request
      DescribeReservedInstances
    -- ** Request constructor
    , mkDescribeReservedInstances
    -- ** Request lenses
    , driReservedInstancesIds
    , driFilters
    , driOfferingType

    -- * Response
    , DescribeReservedInstancesResponse
    -- ** Response lenses
    , drirsReservedInstances
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeReservedInstances = DescribeReservedInstances
    { _driReservedInstancesIds :: [Text]
    , _driFilters :: [Filter]
    , _driOfferingType :: Maybe OfferingTypeValues
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstances' request.
mkDescribeReservedInstances :: DescribeReservedInstances
mkDescribeReservedInstances = DescribeReservedInstances
    { _driReservedInstancesIds = mempty
    , _driFilters = mempty
    , _driOfferingType = Nothing
    }
{-# INLINE mkDescribeReservedInstances #-}

-- | One or more Reserved Instance IDs. Default: Describes all your Reserved
-- Instances, or only those otherwise specified.
driReservedInstancesIds :: Lens' DescribeReservedInstances [Text]
driReservedInstancesIds =
    lens _driReservedInstancesIds
         (\s a -> s { _driReservedInstancesIds = a })
{-# INLINE driReservedInstancesIds #-}

-- | One or more filters. availability-zone - The Availability Zone where the
-- Reserved Instance can be used. duration - The duration of the Reserved
-- Instance (one year or three years), in seconds (31536000 | 94608000). end -
-- The time when the Reserved Instance expires. fixed-price - The purchase
-- price of the Reserved Instance (for example, 9800.0). instance-type - The
-- instance type on which the Reserved Instance can be used.
-- product-description - The product description of the Reserved Instance
-- (Linux/UNIX | Linux/UNIX (Amazon VPC) | Windows | Windows (Amazon VPC)).
-- reserved-instances-id - The ID of the Reserved Instance. start - The time
-- at which the Reserved Instance purchase request was placed. state - The
-- state of the Reserved Instance (pending-payment | active | payment-failed |
-- retired). tag:key=value - The key/value combination of a tag assigned to
-- the resource. tag-key - The key of a tag assigned to the resource. This
-- filter is independent of the tag-value filter. For example, if you use both
-- the filter "tag-key=Purpose" and the filter "tag-value=X", you get any
-- resources assigned both the tag key Purpose (regardless of what the tag's
-- value is), and the tag value X (regardless of what the tag's key is). If
-- you want to list only resources where Purpose is X, see the tag:key=value
-- filter. tag-value - The value of a tag assigned to the resource. This
-- filter is independent of the tag-key filter. usage-price - The usage price
-- of the Reserved Instance, per hour (for example, 0.84).
driFilters :: Lens' DescribeReservedInstances [Filter]
driFilters = lens _driFilters (\s a -> s { _driFilters = a })
{-# INLINE driFilters #-}

-- | The Reserved Instance offering type.
driOfferingType :: Lens' DescribeReservedInstances (Maybe OfferingTypeValues)
driOfferingType = lens _driOfferingType (\s a -> s { _driOfferingType = a })
{-# INLINE driOfferingType #-}

instance ToQuery DescribeReservedInstances where
    toQuery = genericQuery def

-- | 
newtype DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { _drirsReservedInstances :: [ReservedInstances]
    } deriving (Show, Generic)

-- | A list of Reserved Instances.
drirsReservedInstances :: Lens' DescribeReservedInstancesResponse [ReservedInstances]
drirsReservedInstances =
    lens _drirsReservedInstances (\s a -> s { _drirsReservedInstances = a })
{-# INLINE drirsReservedInstances #-}

instance FromXML DescribeReservedInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstances where
    type Sv DescribeReservedInstances = EC2
    type Rs DescribeReservedInstances = DescribeReservedInstancesResponse

    request = post "DescribeReservedInstances"
    response _ = xmlResponse

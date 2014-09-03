{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeRouteTables
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your route tables. For more information about
-- route tables, see Route Tables in the Amazon Virtual Private Cloud User
-- Guide. Example This example describes all your route tables. The first
-- route table in the returned list is the VPC's main route table. Its
-- association ID represents the association between the table and the VPC.
-- https://ec2.amazonaws.com/?Action=DescribeRouteTables &amp;AUTHPARAMS
-- 6f570b0b-9c18-4b07-bdec-73740dcf861a rtb-13ad487a vpc-11ad4878 10.0.0.0/22
-- local active CreateRouteTable rtbassoc-12ad487b rtb-13ad487a true
-- rtb-f9ad4890 vpc-11ad4878 10.0.0.0/22 local active CreateRouteTable
-- 0.0.0.0/0 igw-eaad4883 active rtbassoc-faad4893 rtb-f9ad4890
-- subnet-15ad487c.
module Network.AWS.EC2.V2014_06_15.DescribeRouteTables where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeRouteTables' request.
describeRouteTables :: DescribeRouteTables
describeRouteTables = DescribeRouteTables
    { _drtsDryRun = Nothing
    , _drtsFilters = mempty
    , _drtsRouteTableIds = mempty
    }

data DescribeRouteTables = DescribeRouteTables
    { _drtsDryRun :: Maybe Bool
      -- ^ 
    , _drtsFilters :: [Filter]
      -- ^ One or more filters. association.route-table-association-id - The
      -- ID of an association ID for the route table.
      -- association.route-table-id - The ID of the route table involved
      -- in the association. association.subnet-id - The ID of the subnet
      -- involved in the association. association.main - Indicates whether
      -- the route table is the main route table for the VPC.
      -- route-table-id - The ID of the route table.
      -- route.destination-cidr-block - The CIDR range specified in a
      -- route in the table. route.gateway-id - The ID of a gateway
      -- specified in a route in the table. route.instance-id - The ID of
      -- an instance specified in a route in the table. route.origin -
      -- Describes how the route was created (CreateRouteTable |
      -- CreateRoute | EnableVgwRoutePropagation). route.state - The state
      -- of a route in the route table (active | blackhole). The blackhole
      -- state indicates that the route's target isn't available (for
      -- example, the specified gateway isn't attached to the VPC, the
      -- specified NAT instance has been terminated, and so on).
      -- route.vpc-peering-connection-id - The ID of a VPC peering
      -- connection specified in a route in the table. tag:key=value - The
      -- key/value combination of a tag assigned to the resource. tag-key
      -- - The key of a tag assigned to the resource. This filter is
      -- independent of the tag-value filter. For example, if you use both
      -- the filter "tag-key=Purpose" and the filter "tag-value=X", you
      -- get any resources assigned both the tag key Purpose (regardless
      -- of what the tag's value is), and the tag value X (regardless of
      -- what the tag's key is). If you want to list only resources where
      -- Purpose is X, see the tag:key=value filter. tag-value - The value
      -- of a tag assigned to the resource. This filter is independent of
      -- the tag-key filter. vpc-id - The ID of the VPC for the route
      -- table.
    , _drtsRouteTableIds :: [Text]
      -- ^ One or more route table IDs. Default: Describes all your route
      -- tables.
    } deriving (Show, Generic)

makeLenses ''DescribeRouteTables

instance ToQuery DescribeRouteTables where
    toQuery = genericQuery def

data DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { _drttRouteTables :: [RouteTable]
      -- ^ Information about one or more route tables.
    } deriving (Show, Generic)

makeLenses ''DescribeRouteTablesResponse

instance FromXML DescribeRouteTablesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeRouteTables where
    type Sv DescribeRouteTables = EC2
    type Rs DescribeRouteTables = DescribeRouteTablesResponse

    request = post "DescribeRouteTables"
    response _ = xmlResponse

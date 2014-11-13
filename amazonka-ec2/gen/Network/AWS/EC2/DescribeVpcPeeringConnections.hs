{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your VPC peering connections.
module Network.AWS.EC2.DescribeVpcPeeringConnections
    (
    -- * Request
      DescribeVpcPeeringConnections
    -- ** Request constructor
    , describeVpcPeeringConnections
    -- ** Request lenses
    , dvpc1DryRun
    , dvpc1Filters
    , dvpc1VpcPeeringConnectionIds

    -- * Response
    , DescribeVpcPeeringConnectionsResponse
    -- ** Response constructor
    , describeVpcPeeringConnectionsResponse
    -- ** Response lenses
    , dvpcrVpcPeeringConnections
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections
    { _dvpc1DryRun                  :: Maybe Bool
    , _dvpc1Filters                 :: [Filter]
    , _dvpc1VpcPeeringConnectionIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVpcPeeringConnections' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpc1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvpc1Filters' @::@ ['Filter']
--
-- * 'dvpc1VpcPeeringConnectionIds' @::@ ['Text']
--
describeVpcPeeringConnections :: DescribeVpcPeeringConnections
describeVpcPeeringConnections = DescribeVpcPeeringConnections
    { _dvpc1DryRun                  = Nothing
    , _dvpc1VpcPeeringConnectionIds = mempty
    , _dvpc1Filters                 = mempty
    }

dvpc1DryRun :: Lens' DescribeVpcPeeringConnections (Maybe Bool)
dvpc1DryRun = lens _dvpc1DryRun (\s a -> s { _dvpc1DryRun = a })

-- | One or more filters. accepter-vpc-info.cidr-block - The CIDR block of the
-- peer VPC. accepter-vpc-info.owner-id - The AWS account ID of the owner of
-- the peer VPC. accepter-vpc-info.vpc-id - The ID of the peer VPC.
-- expiration-time - The expiration date and time for the VPC peering
-- connection. requester-vpc-info.cidr-block - The CIDR block of the
-- requester's VPC. requester-vpc-info.owner-id - The AWS account ID of the
-- owner of the requester VPC. requester-vpc-info.vpc-id - The ID of the
-- requester VPC. status-code - The status of the VPC peering connection
-- (pending-acceptance | failed | expired | provisioning | active | deleted
-- | rejected). status-message - A message that provides more information
-- about the status of the VPC peering connection, if applicable.
-- tag:key=value - The key/value combination of a tag assigned to the
-- resource. tag-key - The key of a tag assigned to the resource. This
-- filter is independent of the tag-value filter. For example, if you use
-- both the filter "tag-key=Purpose" and the filter "tag-value=X", you get
-- any resources assigned both the tag key Purpose (regardless of what the
-- tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter.
-- vpc-peering-connection-id - The ID of the VPC peering connection.
dvpc1Filters :: Lens' DescribeVpcPeeringConnections [Filter]
dvpc1Filters = lens _dvpc1Filters (\s a -> s { _dvpc1Filters = a })

-- | One or more VPC peering connection IDs. Default: Describes all your VPC
-- peering connections.
dvpc1VpcPeeringConnectionIds :: Lens' DescribeVpcPeeringConnections [Text]
dvpc1VpcPeeringConnectionIds =
    lens _dvpc1VpcPeeringConnectionIds
        (\s a -> s { _dvpc1VpcPeeringConnectionIds = a })

instance ToQuery DescribeVpcPeeringConnections

instance ToPath DescribeVpcPeeringConnections where
    toPath = const "/"

newtype DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse
    { _dvpcrVpcPeeringConnections :: [VpcPeeringConnection]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeVpcPeeringConnectionsResponse where
    type Item DescribeVpcPeeringConnectionsResponse = VpcPeeringConnection

    fromList = DescribeVpcPeeringConnectionsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dvpcrVpcPeeringConnections

-- | 'DescribeVpcPeeringConnectionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcrVpcPeeringConnections' @::@ ['VpcPeeringConnection']
--
describeVpcPeeringConnectionsResponse :: DescribeVpcPeeringConnectionsResponse
describeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse
    { _dvpcrVpcPeeringConnections = mempty
    }

-- | Information about the VPC peering connections.
dvpcrVpcPeeringConnections :: Lens' DescribeVpcPeeringConnectionsResponse [VpcPeeringConnection]
dvpcrVpcPeeringConnections =
    lens _dvpcrVpcPeeringConnections
        (\s a -> s { _dvpcrVpcPeeringConnections = a })

instance AWSRequest DescribeVpcPeeringConnections where
    type Sv DescribeVpcPeeringConnections = EC2
    type Rs DescribeVpcPeeringConnections = DescribeVpcPeeringConnectionsResponse

    request  = post "DescribeVpcPeeringConnections"
    response = xmlResponse $ \h x -> DescribeVpcPeeringConnectionsResponse
        <$> x %| "vpcPeeringConnectionSet"

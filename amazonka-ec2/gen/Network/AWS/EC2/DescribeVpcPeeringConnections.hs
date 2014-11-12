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
    , dvpcDryRun
    , dvpcFilters
    , dvpcVpcPeeringConnectionIds

    -- * Response
    , DescribeVpcPeeringConnectionsResult
    -- ** Response constructor
    , describeVpcPeeringConnectionsResult
    -- ** Response lenses
    , dvpcrVpcPeeringConnections
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections
    { _dvpcDryRun                  :: Maybe Bool
    , _dvpcFilters                 :: [Filter]
    , _dvpcVpcPeeringConnectionIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeVpcPeeringConnections' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvpcFilters' @::@ ['Filter']
--
-- * 'dvpcVpcPeeringConnectionIds' @::@ ['Text']
--
describeVpcPeeringConnections :: DescribeVpcPeeringConnections
describeVpcPeeringConnections = DescribeVpcPeeringConnections
    { _dvpcDryRun                  = Nothing
    , _dvpcVpcPeeringConnectionIds = mempty
    , _dvpcFilters                 = mempty
    }

dvpcDryRun :: Lens' DescribeVpcPeeringConnections (Maybe Bool)
dvpcDryRun = lens _dvpcDryRun (\s a -> s { _dvpcDryRun = a })

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
dvpcFilters :: Lens' DescribeVpcPeeringConnections [Filter]
dvpcFilters = lens _dvpcFilters (\s a -> s { _dvpcFilters = a })

-- | One or more VPC peering connection IDs. Default: Describes all your VPC
-- peering connections.
dvpcVpcPeeringConnectionIds :: Lens' DescribeVpcPeeringConnections [Text]
dvpcVpcPeeringConnectionIds =
    lens _dvpcVpcPeeringConnectionIds
        (\s a -> s { _dvpcVpcPeeringConnectionIds = a })

instance ToQuery DescribeVpcPeeringConnections

instance ToPath DescribeVpcPeeringConnections where
    toPath = const "/"

newtype DescribeVpcPeeringConnectionsResult = DescribeVpcPeeringConnectionsResult
    { _dvpcrVpcPeeringConnections :: [VpcPeeringConnection]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeVpcPeeringConnectionsResult
    type Item DescribeVpcPeeringConnectionsResult = VpcPeeringConnection

    fromList = DescribeVpcPeeringConnectionsResult . fromList
    toList   = toList . _dvpcrVpcPeeringConnections

-- | 'DescribeVpcPeeringConnectionsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcrVpcPeeringConnections' @::@ ['VpcPeeringConnection']
--
describeVpcPeeringConnectionsResult :: DescribeVpcPeeringConnectionsResult
describeVpcPeeringConnectionsResult = DescribeVpcPeeringConnectionsResult
    { _dvpcrVpcPeeringConnections = mempty
    }

-- | Information about the VPC peering connections.
dvpcrVpcPeeringConnections :: Lens' DescribeVpcPeeringConnectionsResult [VpcPeeringConnection]
dvpcrVpcPeeringConnections =
    lens _dvpcrVpcPeeringConnections
        (\s a -> s { _dvpcrVpcPeeringConnections = a })

instance FromXML DescribeVpcPeeringConnectionsResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeVpcPeeringConnectionsResult"

instance AWSRequest DescribeVpcPeeringConnections where
    type Sv DescribeVpcPeeringConnections = EC2
    type Rs DescribeVpcPeeringConnections = DescribeVpcPeeringConnectionsResult

    request  = post "DescribeVpcPeeringConnections"
    response = xmlResponse $ \h x -> DescribeVpcPeeringConnectionsResult
        <$> x %| "vpcPeeringConnectionSet"

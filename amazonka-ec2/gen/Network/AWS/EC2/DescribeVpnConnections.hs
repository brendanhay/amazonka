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

-- Module      : Network.AWS.EC2.DescribeVpnConnections
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

-- | Describes one or more of your VPN connections.
--
-- For more information about VPN connections, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware VirtualPrivate Gateway to Your VPC> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnConnections.html>
module Network.AWS.EC2.DescribeVpnConnections
    (
    -- * Request
      DescribeVpnConnections
    -- ** Request constructor
    , describeVpnConnections
    -- ** Request lenses
    , dvc1DryRun
    , dvc1Filters
    , dvc1VpnConnectionIds

    -- * Response
    , DescribeVpnConnectionsResponse
    -- ** Response constructor
    , describeVpnConnectionsResponse
    -- ** Response lenses
    , dvcrVpnConnections
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpnConnections = DescribeVpnConnections
    { _dvc1DryRun           :: Maybe Bool
    , _dvc1Filters          :: List "Filter" Filter
    , _dvc1VpnConnectionIds :: List "VpnConnectionId" Text
    } deriving (Eq, Show)

-- | 'DescribeVpnConnections' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvc1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvc1Filters' @::@ ['Filter']
--
-- * 'dvc1VpnConnectionIds' @::@ ['Text']
--
describeVpnConnections :: DescribeVpnConnections
describeVpnConnections = DescribeVpnConnections
    { _dvc1DryRun           = Nothing
    , _dvc1VpnConnectionIds = mempty
    , _dvc1Filters          = mempty
    }

dvc1DryRun :: Lens' DescribeVpnConnections (Maybe Bool)
dvc1DryRun = lens _dvc1DryRun (\s a -> s { _dvc1DryRun = a })

-- | One or more filters.
--
-- 'customer-gateway-configuration' - The configuration information for the
-- customer gateway.
--
-- 'customer-gateway-id' - The ID of a customer gateway associated with the VPN
-- connection.
--
-- 'state' - The state of the VPN connection ('pending' | 'available' | 'deleting' | 'deleted').
--
-- 'option.static-routes-only' - Indicates whether the connection has static
-- routes only. Used for devices that do not support Border Gateway Protocol
-- (BGP).
--
-- 'route.destination-cidr-block' - The destination CIDR block. This
-- corresponds to the subnet used in a customer data center.
--
-- 'bgp-asn' - The BGP Autonomous System Number (ASN) associated with a BGP
-- device.
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the 'tag-value' filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
-- 'type' - The type of VPN connection. Currently the only supported type is 'ipsec.1'.
--
-- 'vpn-connection-id' - The ID of the VPN connection.
--
-- 'vpn-gateway-id' - The ID of a virtual private gateway associated with the
-- VPN connection.
--
--
dvc1Filters :: Lens' DescribeVpnConnections [Filter]
dvc1Filters = lens _dvc1Filters (\s a -> s { _dvc1Filters = a }) . _List

-- | One or more VPN connection IDs.
--
-- Default: Describes your VPN connections.
dvc1VpnConnectionIds :: Lens' DescribeVpnConnections [Text]
dvc1VpnConnectionIds =
    lens _dvc1VpnConnectionIds (\s a -> s { _dvc1VpnConnectionIds = a })
        . _List

newtype DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { _dvcrVpnConnections :: List "item" VpnConnection
    } deriving (Eq, Show, Monoid, Semigroup)

-- | 'DescribeVpnConnectionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvcrVpnConnections' @::@ ['VpnConnection']
--
describeVpnConnectionsResponse :: DescribeVpnConnectionsResponse
describeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { _dvcrVpnConnections = mempty
    }

-- | Information about one or more VPN connections.
dvcrVpnConnections :: Lens' DescribeVpnConnectionsResponse [VpnConnection]
dvcrVpnConnections =
    lens _dvcrVpnConnections (\s a -> s { _dvcrVpnConnections = a })
        . _List

instance ToPath DescribeVpnConnections where
    toPath = const "/"

instance ToQuery DescribeVpnConnections where
    toQuery DescribeVpnConnections{..} = mconcat
        [ "dryRun"          =? _dvc1DryRun
        , toQuery          _dvc1Filters
        , toQuery          _dvc1VpnConnectionIds
        ]

instance ToHeaders DescribeVpnConnections

instance AWSRequest DescribeVpnConnections where
    type Sv DescribeVpnConnections = EC2
    type Rs DescribeVpnConnections = DescribeVpnConnectionsResponse

    request  = post "DescribeVpnConnections"
    response = xmlResponse

instance FromXML DescribeVpnConnectionsResponse where
    parseXML x = DescribeVpnConnectionsResponse
        <$> parseXML x

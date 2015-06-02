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

-- Module      : Network.AWS.EC2.DescribeCustomerGateways
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

-- | Describes one or more of your VPN customer gateways.
--
-- For more information about VPN customer gateways, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a HardwareVirtual Private Gateway to Your VPC> in the /Amazon Virtual Private Cloud UserGuide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeCustomerGateways.html>
module Network.AWS.EC2.DescribeCustomerGateways
    (
    -- * Request
      DescribeCustomerGateways
    -- ** Request constructor
    , describeCustomerGateways
    -- ** Request lenses
    , dcgCustomerGatewayIds
    , dcgDryRun
    , dcgFilters

    -- * Response
    , DescribeCustomerGatewaysResponse
    -- ** Response constructor
    , describeCustomerGatewaysResponse
    -- ** Response lenses
    , dcgrCustomerGateways
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeCustomerGateways = DescribeCustomerGateways
    { _dcgCustomerGatewayIds :: List "CustomerGatewayId" Text
    , _dcgDryRun             :: Maybe Bool
    , _dcgFilters            :: List "Filter" Filter
    } deriving (Eq, Read, Show)

-- | 'DescribeCustomerGateways' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcgCustomerGatewayIds' @::@ ['Text']
--
-- * 'dcgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dcgFilters' @::@ ['Filter']
--
describeCustomerGateways :: DescribeCustomerGateways
describeCustomerGateways = DescribeCustomerGateways
    { _dcgDryRun             = Nothing
    , _dcgCustomerGatewayIds = mempty
    , _dcgFilters            = mempty
    }

-- | One or more customer gateway IDs.
--
-- Default: Describes all your customer gateways.
dcgCustomerGatewayIds :: Lens' DescribeCustomerGateways [Text]
dcgCustomerGatewayIds =
    lens _dcgCustomerGatewayIds (\s a -> s { _dcgCustomerGatewayIds = a })
        . _List

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dcgDryRun :: Lens' DescribeCustomerGateways (Maybe Bool)
dcgDryRun = lens _dcgDryRun (\s a -> s { _dcgDryRun = a })

-- | One or more filters.
--
-- 'bgp-asn' - The customer gateway's Border Gateway Protocol (BGP) Autonomous
-- System Number (ASN).
--
-- 'customer-gateway-id' - The ID of the customer gateway.
--
-- 'ip-address' - The IP address of the customer gateway's Internet-routable
-- external interface.
--
-- 'state' - The state of the customer gateway ('pending' | 'available' | 'deleting'
-- | 'deleted').
--
-- 'type' - The type of customer gateway. Currently, the only supported type is 'ipsec.1'.
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
--
dcgFilters :: Lens' DescribeCustomerGateways [Filter]
dcgFilters = lens _dcgFilters (\s a -> s { _dcgFilters = a }) . _List

newtype DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { _dcgrCustomerGateways :: List "item" CustomerGateway
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeCustomerGatewaysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcgrCustomerGateways' @::@ ['CustomerGateway']
--
describeCustomerGatewaysResponse :: DescribeCustomerGatewaysResponse
describeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { _dcgrCustomerGateways = mempty
    }

-- | Information about one or more customer gateways.
dcgrCustomerGateways :: Lens' DescribeCustomerGatewaysResponse [CustomerGateway]
dcgrCustomerGateways =
    lens _dcgrCustomerGateways (\s a -> s { _dcgrCustomerGateways = a })
        . _List

instance ToPath DescribeCustomerGateways where
    toPath = const "/"

instance ToQuery DescribeCustomerGateways where
    toQuery DescribeCustomerGateways{..} = mconcat
        [ "CustomerGatewayId" `toQueryList` _dcgCustomerGatewayIds
        , "DryRun"            =? _dcgDryRun
        , "Filter"            `toQueryList` _dcgFilters
        ]

instance ToHeaders DescribeCustomerGateways

instance AWSRequest DescribeCustomerGateways where
    type Sv DescribeCustomerGateways = EC2
    type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse

    request  = post "DescribeCustomerGateways"
    response = xmlResponse

instance FromXML DescribeCustomerGatewaysResponse where
    parseXML x = DescribeCustomerGatewaysResponse
        <$> x .@? "customerGatewaySet" .!@ mempty

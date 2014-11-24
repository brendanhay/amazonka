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

-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your Elastic IP addresses. An Elastic IP address
-- is for use in either the EC2-Classic platform or in a VPC. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html
-- Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAddresses.html>
module Network.AWS.EC2.DescribeAddresses
    (
    -- * Request
      DescribeAddresses
    -- ** Request constructor
    , describeAddresses
    -- ** Request lenses
    , daAllocationIds
    , daDryRun
    , daFilters
    , daPublicIps

    -- * Response
    , DescribeAddressesResponse
    -- ** Response constructor
    , describeAddressesResponse
    -- ** Response lenses
    , darAddresses
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeAddresses = DescribeAddresses
    { _daAllocationIds :: List "AllocationId" Text
    , _daDryRun        :: Maybe Bool
    , _daFilters       :: List "Filter" Filter
    , _daPublicIps     :: List "PublicIp" Text
    } deriving (Eq, Show)

-- | 'DescribeAddresses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAllocationIds' @::@ ['Text']
--
-- * 'daDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'daFilters' @::@ ['Filter']
--
-- * 'daPublicIps' @::@ ['Text']
--
describeAddresses :: DescribeAddresses
describeAddresses = DescribeAddresses
    { _daDryRun        = Nothing
    , _daPublicIps     = mempty
    , _daFilters       = mempty
    , _daAllocationIds = mempty
    }

-- | [EC2-VPC] One or more allocation IDs. Default: Describes all your Elastic
-- IP addresses.
daAllocationIds :: Lens' DescribeAddresses [Text]
daAllocationIds = lens _daAllocationIds (\s a -> s { _daAllocationIds = a }) . _List

daDryRun :: Lens' DescribeAddresses (Maybe Bool)
daDryRun = lens _daDryRun (\s a -> s { _daDryRun = a })

-- | One or more filters. Filter names and values are case-sensitive.
-- @allocation-id@ - [EC2-VPC] The allocation ID for the address.
-- @association-id@ - [EC2-VPC] The association ID for the address. @domain@
-- - Indicates whether the address is for use in EC2-Classic (@standard@) or
-- in a VPC (@vpc@). @instance-id@ - The ID of the instance the address is
-- associated with, if any. @network-interface-id@ - [EC2-VPC] The ID of the
-- network interface that the address is associated with, if any.
-- @network-interface-owner-id@ - The AWS account ID of the owner.
-- @private-ip-address@ - [EC2-VPC] The private IP address associated with
-- the Elastic IP address. @public-ip@ - The Elastic IP address.
daFilters :: Lens' DescribeAddresses [Filter]
daFilters = lens _daFilters (\s a -> s { _daFilters = a }) . _List

-- | [EC2-Classic] One or more Elastic IP addresses. Default: Describes all
-- your Elastic IP addresses.
daPublicIps :: Lens' DescribeAddresses [Text]
daPublicIps = lens _daPublicIps (\s a -> s { _daPublicIps = a }) . _List

newtype DescribeAddressesResponse = DescribeAddressesResponse
    { _darAddresses :: List "item" Address
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAddressesResponse where
    type Item DescribeAddressesResponse = Address

    fromList = DescribeAddressesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _darAddresses

-- | 'DescribeAddressesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darAddresses' @::@ ['Address']
--
describeAddressesResponse :: DescribeAddressesResponse
describeAddressesResponse = DescribeAddressesResponse
    { _darAddresses = mempty
    }

-- | Information about one or more Elastic IP addresses.
darAddresses :: Lens' DescribeAddressesResponse [Address]
darAddresses = lens _darAddresses (\s a -> s { _darAddresses = a }) . _List

instance ToPath DescribeAddresses where
    toPath = const "/"

instance ToQuery DescribeAddresses where
    toQuery DescribeAddresses{..} = mconcat
        [ "AllocationId" =? _daAllocationIds
        , "dryRun"       =? _daDryRun
        , "Filter"       =? _daFilters
        , "PublicIp"     =? _daPublicIps
        ]

instance ToHeaders DescribeAddresses

instance AWSRequest DescribeAddresses where
    type Sv DescribeAddresses = EC2
    type Rs DescribeAddresses = DescribeAddressesResponse

    request  = post "DescribeAddresses"
    response = xmlResponse

instance FromXML DescribeAddressesResponse where
    parseXML x = DescribeAddressesResponse
        <$> x .@  "addressesSet"

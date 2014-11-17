{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Assigns one or more secondary private IP addresses to the specified network
-- interface. You can specify one or more specific secondary IP addresses, or
-- you can specify the number of secondary IP addresses to be automatically
-- assigned within the subnet's CIDR block range. The number of secondary IP
-- addresses that you can assign to an instance varies by instance type. For
-- information about instance types, see Instance Types in the Amazon Elastic
-- Compute Cloud User Guide. For more information about Elastic IP addresses,
-- see Elastic IP Addresses in the Amazon Elastic Compute Cloud User Guide.
-- AssignPrivateIpAddresses is available only in EC2-VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AssignPrivateIpAddresses.html>
module Network.AWS.EC2.AssignPrivateIpAddresses
    (
    -- * Request
      AssignPrivateIpAddresses
    -- ** Request constructor
    , assignPrivateIpAddresses
    -- ** Request lenses
    , apiaAllowReassignment
    , apiaNetworkInterfaceId
    , apiaPrivateIpAddresses
    , apiaSecondaryPrivateIpAddressCount

    -- * Response
    , AssignPrivateIpAddressesResponse
    -- ** Response constructor
    , assignPrivateIpAddressesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { _apiaAllowReassignment              :: Maybe Bool
    , _apiaNetworkInterfaceId             :: Text
    , _apiaPrivateIpAddresses             :: [Text]
    , _apiaSecondaryPrivateIpAddressCount :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssignPrivateIpAddresses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apiaAllowReassignment' @::@ 'Maybe' 'Bool'
--
-- * 'apiaNetworkInterfaceId' @::@ 'Text'
--
-- * 'apiaPrivateIpAddresses' @::@ ['Text']
--
-- * 'apiaSecondaryPrivateIpAddressCount' @::@ 'Maybe' 'Int'
--
assignPrivateIpAddresses :: Text -- ^ 'apiaNetworkInterfaceId'
                         -> AssignPrivateIpAddresses
assignPrivateIpAddresses p1 = AssignPrivateIpAddresses
    { _apiaNetworkInterfaceId             = p1
    , _apiaPrivateIpAddresses             = mempty
    , _apiaSecondaryPrivateIpAddressCount = Nothing
    , _apiaAllowReassignment              = Nothing
    }

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
apiaAllowReassignment :: Lens' AssignPrivateIpAddresses (Maybe Bool)
apiaAllowReassignment =
    lens _apiaAllowReassignment (\s a -> s { _apiaAllowReassignment = a })

-- | The ID of the network interface.
apiaNetworkInterfaceId :: Lens' AssignPrivateIpAddresses Text
apiaNetworkInterfaceId =
    lens _apiaNetworkInterfaceId (\s a -> s { _apiaNetworkInterfaceId = a })

-- | One or more IP addresses to be assigned as a secondary private IP address
-- to the network interface. You can't specify this parameter when also
-- specifying a number of secondary IP addresses. If you don't specify an IP
-- address, Amazon EC2 automatically selects an IP address within the subnet
-- range.
apiaPrivateIpAddresses :: Lens' AssignPrivateIpAddresses [Text]
apiaPrivateIpAddresses =
    lens _apiaPrivateIpAddresses (\s a -> s { _apiaPrivateIpAddresses = a })

-- | The number of secondary IP addresses to assign to the network interface.
-- You can't specify this parameter when also specifying private IP
-- addresses.
apiaSecondaryPrivateIpAddressCount :: Lens' AssignPrivateIpAddresses (Maybe Int)
apiaSecondaryPrivateIpAddressCount =
    lens _apiaSecondaryPrivateIpAddressCount
        (\s a -> s { _apiaSecondaryPrivateIpAddressCount = a })

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AssignPrivateIpAddressesResponse' constructor.
assignPrivateIpAddressesResponse :: AssignPrivateIpAddressesResponse
assignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse

instance AWSRequest AssignPrivateIpAddresses where
    type Sv AssignPrivateIpAddresses = EC2
    type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse

    request  = post "AssignPrivateIpAddresses"
    response = nullResponse AssignPrivateIpAddressesResponse

instance ToPath AssignPrivateIpAddresses where
    toPath = const "/"

instance ToHeaders AssignPrivateIpAddresses

instance ToQuery AssignPrivateIpAddresses

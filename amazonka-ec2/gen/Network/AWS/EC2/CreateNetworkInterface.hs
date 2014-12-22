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

-- Module      : Network.AWS.EC2.CreateNetworkInterface
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

-- | Creates a network interface in the specified subnet.
--
-- For more information about network interfaces, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkInterface.html>
module Network.AWS.EC2.CreateNetworkInterface
    (
    -- * Request
      CreateNetworkInterface
    -- ** Request constructor
    , createNetworkInterface
    -- ** Request lenses
    , cniDescription
    , cniDryRun
    , cniGroups
    , cniPrivateIpAddress
    , cniPrivateIpAddresses
    , cniSecondaryPrivateIpAddressCount
    , cniSubnetId

    -- * Response
    , CreateNetworkInterfaceResponse
    -- ** Response constructor
    , createNetworkInterfaceResponse
    -- ** Response lenses
    , cnirNetworkInterface
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateNetworkInterface = CreateNetworkInterface
    { _cniDescription                    :: Maybe Text
    , _cniDryRun                         :: Maybe Bool
    , _cniGroups                         :: List "SecurityGroupId" Text
    , _cniPrivateIpAddress               :: Maybe Text
    , _cniPrivateIpAddresses             :: List "item" PrivateIpAddressSpecification
    , _cniSecondaryPrivateIpAddressCount :: Maybe Int
    , _cniSubnetId                       :: Text
    } deriving (Eq, Show)

-- | 'CreateNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cniDescription' @::@ 'Maybe' 'Text'
--
-- * 'cniDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cniGroups' @::@ ['Text']
--
-- * 'cniPrivateIpAddress' @::@ 'Maybe' 'Text'
--
-- * 'cniPrivateIpAddresses' @::@ ['PrivateIpAddressSpecification']
--
-- * 'cniSecondaryPrivateIpAddressCount' @::@ 'Maybe' 'Int'
--
-- * 'cniSubnetId' @::@ 'Text'
--
createNetworkInterface :: Text -- ^ 'cniSubnetId'
                       -> CreateNetworkInterface
createNetworkInterface p1 = CreateNetworkInterface
    { _cniSubnetId                       = p1
    , _cniDescription                    = Nothing
    , _cniPrivateIpAddress               = Nothing
    , _cniGroups                         = mempty
    , _cniPrivateIpAddresses             = mempty
    , _cniSecondaryPrivateIpAddressCount = Nothing
    , _cniDryRun                         = Nothing
    }

-- | A description for the network interface.
cniDescription :: Lens' CreateNetworkInterface (Maybe Text)
cniDescription = lens _cniDescription (\s a -> s { _cniDescription = a })

cniDryRun :: Lens' CreateNetworkInterface (Maybe Bool)
cniDryRun = lens _cniDryRun (\s a -> s { _cniDryRun = a })

-- | The IDs of one or more security groups.
cniGroups :: Lens' CreateNetworkInterface [Text]
cniGroups = lens _cniGroups (\s a -> s { _cniGroups = a }) . _List

-- | The primary private IP address of the network interface. If you don't specify
-- an IP address, Amazon EC2 selects one for you from the subnet range. If you
-- specify an IP address, you cannot indicate any IP addresses specified in 'privateIpAddresses' as primary (only one IP address can be designated as primary).
cniPrivateIpAddress :: Lens' CreateNetworkInterface (Maybe Text)
cniPrivateIpAddress =
    lens _cniPrivateIpAddress (\s a -> s { _cniPrivateIpAddress = a })

-- | One or more private IP addresses.
cniPrivateIpAddresses :: Lens' CreateNetworkInterface [PrivateIpAddressSpecification]
cniPrivateIpAddresses =
    lens _cniPrivateIpAddresses (\s a -> s { _cniPrivateIpAddresses = a })
        . _List

-- | The number of secondary private IP addresses to assign to a network
-- interface. When you specify a number of secondary IP addresses, Amazon EC2
-- selects these IP addresses within the subnet range. You can't specify this
-- option and specify more than one private IP address using 'privateIpAddresses'.
--
-- The number of IP addresses you can assign to a network interface varies by
-- instance type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI Private IP Addresses Per ENI PerInstance Type> in the /Amazon Elastic Compute Cloud User Guide/.
cniSecondaryPrivateIpAddressCount :: Lens' CreateNetworkInterface (Maybe Int)
cniSecondaryPrivateIpAddressCount =
    lens _cniSecondaryPrivateIpAddressCount
        (\s a -> s { _cniSecondaryPrivateIpAddressCount = a })

-- | The ID of the subnet to associate with the network interface.
cniSubnetId :: Lens' CreateNetworkInterface Text
cniSubnetId = lens _cniSubnetId (\s a -> s { _cniSubnetId = a })

newtype CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { _cnirNetworkInterface :: Maybe NetworkInterface
    } deriving (Eq, Show)

-- | 'CreateNetworkInterfaceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnirNetworkInterface' @::@ 'Maybe' 'NetworkInterface'
--
createNetworkInterfaceResponse :: CreateNetworkInterfaceResponse
createNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { _cnirNetworkInterface = Nothing
    }

-- | Information about the network interface.
cnirNetworkInterface :: Lens' CreateNetworkInterfaceResponse (Maybe NetworkInterface)
cnirNetworkInterface =
    lens _cnirNetworkInterface (\s a -> s { _cnirNetworkInterface = a })

instance ToPath CreateNetworkInterface where
    toPath = const "/"

instance ToQuery CreateNetworkInterface where
    toQuery CreateNetworkInterface{..} = mconcat
        [ "description"                    =? _cniDescription
        , "dryRun"                         =? _cniDryRun
        , "SecurityGroupId"                `toQueryList` _cniGroups
        , "privateIpAddress"               =? _cniPrivateIpAddress
        , "privateIpAddresses"             `toQueryList` _cniPrivateIpAddresses
        , "secondaryPrivateIpAddressCount" =? _cniSecondaryPrivateIpAddressCount
        , "subnetId"                       =? _cniSubnetId
        ]

instance ToHeaders CreateNetworkInterface

instance AWSRequest CreateNetworkInterface where
    type Sv CreateNetworkInterface = EC2
    type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse

    request  = post "CreateNetworkInterface"
    response = xmlResponse

instance FromXML CreateNetworkInterfaceResponse where
    parseXML x = CreateNetworkInterfaceResponse
        <$> x .@? "networkInterface"

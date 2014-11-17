{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unassigns one or more secondary private IP addresses from a network
-- interface.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-UnassignPrivateIpAddresses.html>
module Network.AWS.EC2.UnassignPrivateIpAddresses
    (
    -- * Request
      UnassignPrivateIpAddresses
    -- ** Request constructor
    , unassignPrivateIpAddresses
    -- ** Request lenses
    , upiaNetworkInterfaceId
    , upiaPrivateIpAddresses

    -- * Response
    , UnassignPrivateIpAddressesResponse
    -- ** Response constructor
    , unassignPrivateIpAddressesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { _upiaNetworkInterfaceId :: Text
    , _upiaPrivateIpAddresses :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'UnassignPrivateIpAddresses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upiaNetworkInterfaceId' @::@ 'Text'
--
-- * 'upiaPrivateIpAddresses' @::@ ['Text']
--
unassignPrivateIpAddresses :: Text -- ^ 'upiaNetworkInterfaceId'
                           -> UnassignPrivateIpAddresses
unassignPrivateIpAddresses p1 = UnassignPrivateIpAddresses
    { _upiaNetworkInterfaceId = p1
    , _upiaPrivateIpAddresses = mempty
    }

-- | The ID of the network interface.
upiaNetworkInterfaceId :: Lens' UnassignPrivateIpAddresses Text
upiaNetworkInterfaceId =
    lens _upiaNetworkInterfaceId (\s a -> s { _upiaNetworkInterfaceId = a })

-- | The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
upiaPrivateIpAddresses :: Lens' UnassignPrivateIpAddresses [Text]
upiaPrivateIpAddresses =
    lens _upiaPrivateIpAddresses (\s a -> s { _upiaPrivateIpAddresses = a })

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnassignPrivateIpAddressesResponse' constructor.
unassignPrivateIpAddressesResponse :: UnassignPrivateIpAddressesResponse
unassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse

instance AWSRequest UnassignPrivateIpAddresses where
    type Sv UnassignPrivateIpAddresses = EC2
    type Rs UnassignPrivateIpAddresses = UnassignPrivateIpAddressesResponse

    request  = post "UnassignPrivateIpAddresses"
    response = nullResponse UnassignPrivateIpAddressesResponse

instance ToPath UnassignPrivateIpAddresses where
    toPath = const "/"

instance ToHeaders UnassignPrivateIpAddresses

instance ToQuery UnassignPrivateIpAddresses where
    toQuery UnassignPrivateIpAddresses{..} = mconcat
        [ "networkInterfaceId" =? _upiaNetworkInterfaceId
        , "privateIpAddress"   =? _upiaPrivateIpAddresses
        ]

instance ToXML UnassignPrivateIpAddresses where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UnassignPrivateIpAddresses"

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- interface. Example The following example unassigns two secondary private IP
-- addresses from the specified network interface.
-- https://ec2.amazonaws.com/?Action=UnassignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-197d9972 &amp;PrivateIpAddress.0=10.0.2.60
-- &amp;PrivateIpAddress.1=10.0.2.65 &amp;AUTHPARAMS
-- &lt;UnassignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/UnassignPrivateIpAddresses&gt;.
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

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { _upiaNetworkInterfaceId :: Text
    , _upiaPrivateIpAddresses :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnassignPrivateIpAddresses' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Text@
--
-- * @PrivateIpAddresses ::@ @[Text]@
--
unassignPrivateIpAddresses :: Text -- ^ 'upiaNetworkInterfaceId'
                           -> [Text] -- ^ 'upiaPrivateIpAddresses'
                           -> UnassignPrivateIpAddresses
unassignPrivateIpAddresses p1 p2 = UnassignPrivateIpAddresses
    { _upiaNetworkInterfaceId = p1
    , _upiaPrivateIpAddresses = p2
    }

-- | The ID of the network interface.
upiaNetworkInterfaceId :: Lens' UnassignPrivateIpAddresses Text
upiaNetworkInterfaceId =
    lens _upiaNetworkInterfaceId (\s a -> s { _upiaNetworkInterfaceId = a })

-- | The secondary private IP addresses to unassign from the network interface.
-- You can specify this option multiple times to unassign more than one IP
-- address.
upiaPrivateIpAddresses :: Lens' UnassignPrivateIpAddresses [Text]
upiaPrivateIpAddresses =
    lens _upiaPrivateIpAddresses (\s a -> s { _upiaPrivateIpAddresses = a })

instance ToQuery UnassignPrivateIpAddresses where
    toQuery = genericQuery def

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UnassignPrivateIpAddressesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
unassignPrivateIpAddressesResponse :: UnassignPrivateIpAddressesResponse
unassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse

instance AWSRequest UnassignPrivateIpAddresses where
    type Sv UnassignPrivateIpAddresses = EC2
    type Rs UnassignPrivateIpAddresses = UnassignPrivateIpAddressesResponse

    request = post "UnassignPrivateIpAddresses"
    response _ = nullaryResponse UnassignPrivateIpAddressesResponse

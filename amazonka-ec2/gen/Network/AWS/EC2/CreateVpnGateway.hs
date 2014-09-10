{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a virtual private gateway. A virtual private gateway is the
-- endpoint on the VPC side of your VPN connection. You can create a virtual
-- private gateway before creating the VPC itself. For more information about
-- virtual private gateways, see Adding a Hardware Virtual Private Gateway to
-- Your VPC in the Amazon Virtual Private Cloud User Guide. Example This
-- example creates a virtual private gateway.
-- https://ec2.amazonaws.com/?Action=CreateVpnGateway &amp;Type=ipsec.1
-- &amp;AUTHPARAMS &lt;CreateVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnGateway&gt; &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;attachments/&gt; &lt;tagSet/&gt; &lt;/vpnGateway&gt;
-- &lt;/CreateVpnGatewayResponse&gt;.
module Network.AWS.EC2
    (
    -- * Request
      CreateVpnGateway
    -- ** Request constructor
    , mkCreateVpnGateway
    -- ** Request lenses
    , cvgType
    , cvgAvailabilityZone

    -- * Response
    , CreateVpnGatewayResponse
    -- ** Response constructor
    , mkCreateVpnGatewayResponse
    -- ** Response lenses
    , cvgrVpnGateway
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateVpnGateway = CreateVpnGateway
    { _cvgType :: GatewayType
    , _cvgAvailabilityZone :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVpnGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @GatewayType@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
mkCreateVpnGateway :: GatewayType -- ^ 'cvgType'
                   -> CreateVpnGateway
mkCreateVpnGateway p1 = CreateVpnGateway
    { _cvgType = p1
    , _cvgAvailabilityZone = Nothing
    }

-- | The type of VPN connection this virtual private gateway supports.
cvgType :: Lens' CreateVpnGateway GatewayType
cvgType = lens _cvgType (\s a -> s { _cvgType = a })

-- | The Availability Zone for the virtual private gateway.
cvgAvailabilityZone :: Lens' CreateVpnGateway (Maybe Text)
cvgAvailabilityZone =
    lens _cvgAvailabilityZone (\s a -> s { _cvgAvailabilityZone = a })

instance ToQuery CreateVpnGateway where
    toQuery = genericQuery def

newtype CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { _cvgrVpnGateway :: Maybe VpnGateway
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVpnGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpnGateway ::@ @Maybe VpnGateway@
--
mkCreateVpnGatewayResponse :: CreateVpnGatewayResponse
mkCreateVpnGatewayResponse = CreateVpnGatewayResponse
    { _cvgrVpnGateway = Nothing
    }

-- | Information about the virtual private gateway.
cvgrVpnGateway :: Lens' CreateVpnGatewayResponse (Maybe VpnGateway)
cvgrVpnGateway = lens _cvgrVpnGateway (\s a -> s { _cvgrVpnGateway = a })

instance FromXML CreateVpnGatewayResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVpnGateway where
    type Sv CreateVpnGateway = EC2
    type Rs CreateVpnGateway = CreateVpnGatewayResponse

    request = post "CreateVpnGateway"
    response _ = xmlResponse

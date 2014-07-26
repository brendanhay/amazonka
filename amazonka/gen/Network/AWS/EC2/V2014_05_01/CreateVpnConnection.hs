{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CreateVpnConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a VPN connection between an existing virtual private gateway and a
-- VPN customer gateway. The only supported connection type is ipsec.1. The
-- response includes information that you need to give to your network
-- administrator to configure your customer gateway. We strongly recommend
-- that you use HTTPS when calling this operation because the response
-- contains sensitive cryptographic information for configuring your customer
-- gateway. If you decide to shut down your VPN connection for any reason and
-- later create a new VPN connection, you must reconfigure your customer
-- gateway with the new information returned from this call. For more
-- information about VPN connections, see Adding a Hardware Virtual Private
-- Gateway to Your VPC in the Amazon Virtual Private Cloud User Guide. Example
-- 1 This example creates a VPN connection between the virtual private gateway
-- with the ID vgw-8db04f81 and the customer gateway with the ID cgw-b4dc3961.
-- The response includes configuration information for the customer gateway.
-- Because it's a long set of information, we haven't included the complete
-- response here. To see an example of the configuation information, see the
-- Amazon Virtual Private Cloud Network Administrator Guide.
-- https://ec2.amazonaws.com/?Action=CreateVpnConnection &amp;Type=ipsec.1
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;VpnGatewayId=vgw-8db04f81
-- &amp;AUTHPARAMS &lt;CreateVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnConnection&gt;
-- &lt;vpnConnectionId&gt;vpn-44a8938f&lt;/vpnConnectionId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt; &lt;tagSet/&gt;
-- &lt;/vpnConnection&gt; &lt;/CreateVpnConnectionResponse&gt; Example 2 This
-- example creates a VPN connection with the static routes option between the
-- virtual private gateway with the ID vgw-8db04f81, and the customer gateway
-- with the ID cgw-b4dc3961, for a device that does not support the Border
-- Gateway Protocol (BGP). The response includes configuration information for
-- the VPN connection's customer gateway. Because it's a long set of
-- information, we haven't included the complete response here.
-- https://ec2.amazonaws.com/?Action=CreateVpnConnection &amp;Type=ipsec.1
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;VpnGatewayId=vgw-8db04f81
-- &amp;Options.StaticRoutesOnly=true &amp;AUTHPARAMS
-- &lt;CreateVpnConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;5cc7891f-1f3b-4fc4-a626-bdea8f63ff5a&lt;/requestId&gt;
-- &lt;vpnConnection&gt;
-- &lt;vpnConnectionId&gt;vpn-83ad48ea&lt;/vpnConnectionId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;customerGatewayConfiguration&gt;
-- ...Customer gateway configuration data in escaped XML format...
-- &lt;/customerGatewayConfiguration&gt;
-- &lt;customerGatewayId&gt;cgw-63ae4b0a&lt;/customerGatewayId&gt;
-- &lt;vpnGatewayId&gt;vgw-4ea04527&lt;/vpnGatewayId&gt; &lt;options&gt;
-- &lt;staticRoutesOnly&gt;true&lt;/staticRoutesOnly&gt; &lt;/options&gt;
-- &lt;routes/&gt; &lt;/vpnConnection&gt;
-- &lt;/CreateVpnConnectionResponse&gt;.
module Network.AWS.EC2.V2014_05_01.CreateVpnConnection where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateVpnConnection = CreateVpnConnection
    { _cvcrVpnGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    , _cvcrCustomerGatewayId :: Text
      -- ^ The ID of the customer gateway.
    , _cvcrType :: Text
      -- ^ The type of VPN connection.
    , _cvcrDryRun :: Bool
      -- ^ 
    , _cvcrOptions :: VpnConnectionOptionsSpecification
      -- ^ Indicates whether the VPN connection requires static routes. If
      -- you are creating a VPN connection for a device that does not
      -- support BGP, you must specify true. Default: false.
    } deriving (Generic)

instance ToQuery CreateVpnConnection where
    toQuery = genericToQuery def

instance AWSRequest CreateVpnConnection where
    type Sv CreateVpnConnection = EC2
    type Rs CreateVpnConnection = CreateVpnConnectionResponse

    request = post "CreateVpnConnection"

    response _ = xmlResponse

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { _cvcsVpnConnection :: Maybe VpnConnection
      -- ^ Information about the VPN connection.
    } deriving (Generic)

instance FromXML CreateVpnConnectionResponse where
    fromXMLOptions = xmlOptions

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateCustomerGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information to AWS about your VPN customer gateway device. The
-- customer gateway is the appliance at your end of the VPN connection. (The
-- device on the AWS side of the VPN connection is the virtual private
-- gateway.) You must provide the Internet-routable IP address of the customer
-- gateway's external interface. The IP address must be static and can't be
-- behind a device performing network address translation (NAT). For devices
-- that use Border Gateway Protocol (BGP), you can also provide the device's
-- BGP Autonomous System Number (ASN). You can use an existing ASN assigned to
-- your network. If you don't have an ASN already, you can use a private ASN
-- (in the 64512 - 65534 range). Amazon EC2 supports all 2-byte ASN numbers in
-- the range of 1 - 65534, with the exception of 7224, which is reserved in
-- the us-east-1 region, and 9059, which is reserved in the eu-west-1 region.
-- For more information about VPN customer gateways, see Adding a Hardware
-- Virtual Private Gateway to Your VPC in the Amazon Virtual Private Cloud
-- User Guide. Example This example passes information to AWS about the
-- customer gateway with the IP address 12.1.2.3 and BGP ASN 65534.
-- https://ec2.amazonaws.com/?Action=CreateCustomerGateway &amp;Type=ipsec.1
-- &amp;IpAddress=12.1.2.3 &amp;BgpAsn=65534 &amp;AUTHPARAMS
-- &lt;CreateCustomerGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;customerGateway&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;ipAddress&gt;12.1.2.3&lt;/ipAddress&gt;
-- &lt;bgpAsn&gt;65534&lt;/bgpAsn&gt; &lt;tagSet/&gt; &lt;/customerGateway&gt;
-- &lt;/CreateCustomerGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateCustomerGateway where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCustomerGateway' request.
createCustomerGateway :: GatewayType -- ^ '_ccgrType'
                      -> Integer -- ^ '_ccgrBgpAsn'
                      -> Text -- ^ '_ccgrPublicIp'
                      -> CreateCustomerGateway
createCustomerGateway p1 p2 p3 = CreateCustomerGateway
    { _ccgrType = p1
    , _ccgrBgpAsn = p2
    , _ccgrPublicIp = p3
    , _ccgrDryRun = Nothing
    }

data CreateCustomerGateway = CreateCustomerGateway
    { _ccgrType :: GatewayType
      -- ^ The type of VPN connection that this customer gateway supports.
    , _ccgrBgpAsn :: Integer
      -- ^ For devices that support BGP, the customer gateway's BGP ASN.
      -- Default: 65000.
    , _ccgrPublicIp :: Text
      -- ^ The Internet-routable IP address for the customer gateway's
      -- outside interface. The address must be static.
    , _ccgrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''CreateCustomerGateway

instance ToQuery CreateCustomerGateway where
    toQuery = genericToQuery def

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { _ccgsCustomerGateway :: Maybe CustomerGateway
      -- ^ Information about the customer gateway.
    } deriving (Generic)

makeLenses ''CreateCustomerGatewayResponse

instance FromXML CreateCustomerGatewayResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCustomerGateway where
    type Sv CreateCustomerGateway = EC2
    type Rs CreateCustomerGateway = CreateCustomerGatewayResponse

    request = post "CreateCustomerGateway"
    response _ = xmlResponse

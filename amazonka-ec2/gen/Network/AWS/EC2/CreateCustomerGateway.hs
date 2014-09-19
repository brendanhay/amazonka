{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateCustomerGateway
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;customerGateway&gt;
-- &lt;customerGatewayId&gt;cgw-b4dc3961&lt;/customerGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;ipAddress&gt;12.1.2.3&lt;/ipAddress&gt;
-- &lt;bgpAsn&gt;65534&lt;/bgpAsn&gt; &lt;tagSet/&gt; &lt;/customerGateway&gt;
-- &lt;/CreateCustomerGatewayResponse&gt;.
module Network.AWS.EC2.CreateCustomerGateway
    (
    -- * Request
      CreateCustomerGateway
    -- ** Request constructor
    , createCustomerGateway
    -- ** Request lenses
    , ccgType
    , ccgPublicIp
    , ccgBgpAsn

    -- * Response
    , CreateCustomerGatewayResponse
    -- ** Response constructor
    , createCustomerGatewayResponse
    -- ** Response lenses
    , ccgrCustomerGateway
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateCustomerGateway = CreateCustomerGateway
    { _ccgType :: GatewayType
    , _ccgPublicIp :: Text
    , _ccgBgpAsn :: !Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCustomerGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @GatewayType@
--
-- * @PublicIp ::@ @Text@
--
-- * @BgpAsn ::@ @Integer@
--
createCustomerGateway :: GatewayType -- ^ 'ccgType'
                      -> Text -- ^ 'ccgPublicIp'
                      -> Integer -- ^ 'ccgBgpAsn'
                      -> CreateCustomerGateway
createCustomerGateway p1 p2 p3 = CreateCustomerGateway
    { _ccgType = p1
    , _ccgPublicIp = p2
    , _ccgBgpAsn = p3
    }

-- | The type of VPN connection that this customer gateway supports.
ccgType :: Lens' CreateCustomerGateway GatewayType
ccgType = lens _ccgType (\s a -> s { _ccgType = a })

-- | The Internet-routable IP address for the customer gateway's outside
-- interface. The address must be static.
ccgPublicIp :: Lens' CreateCustomerGateway Text
ccgPublicIp = lens _ccgPublicIp (\s a -> s { _ccgPublicIp = a })

-- | For devices that support BGP, the customer gateway's BGP ASN. Default:
-- 65000.
ccgBgpAsn :: Lens' CreateCustomerGateway Integer
ccgBgpAsn = lens _ccgBgpAsn (\s a -> s { _ccgBgpAsn = a })

instance ToQuery CreateCustomerGateway where
    toQuery = genericQuery def

newtype CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { _ccgrCustomerGateway :: Maybe CustomerGateway
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCustomerGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CustomerGateway ::@ @Maybe CustomerGateway@
--
createCustomerGatewayResponse :: CreateCustomerGatewayResponse
createCustomerGatewayResponse = CreateCustomerGatewayResponse
    { _ccgrCustomerGateway = Nothing
    }

-- | Information about the customer gateway.
ccgrCustomerGateway :: Lens' CreateCustomerGatewayResponse (Maybe CustomerGateway)
ccgrCustomerGateway =
    lens _ccgrCustomerGateway (\s a -> s { _ccgrCustomerGateway = a })

instance FromXML CreateCustomerGatewayResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCustomerGateway where
    type Sv CreateCustomerGateway = EC2
    type Rs CreateCustomerGateway = CreateCustomerGatewayResponse

    request = post "CreateCustomerGateway"
    response _ = xmlResponse

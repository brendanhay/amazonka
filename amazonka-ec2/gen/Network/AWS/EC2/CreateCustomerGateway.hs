{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- (in the 64512 - 65534 range). For more information about VPN customer
-- gateways, see Adding a Hardware Virtual Private Gateway to Your VPC in the
-- Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateCustomerGateway.html>
module Network.AWS.EC2.CreateCustomerGateway
    (
    -- * Request
      CreateCustomerGateway
    -- ** Request constructor
    , createCustomerGateway
    -- ** Request lenses
    , ccgBgpAsn
    , ccgDryRun
    , ccgPublicIp
    , ccgType

    -- * Response
    , CreateCustomerGatewayResponse
    -- ** Response constructor
    , createCustomerGatewayResponse
    -- ** Response lenses
    , ccgrCustomerGateway
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateCustomerGateway = CreateCustomerGateway
    { _ccgBgpAsn   :: Int
    , _ccgDryRun   :: Maybe Bool
    , _ccgPublicIp :: Text
    , _ccgType     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCustomerGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccgBgpAsn' @::@ 'Int'
--
-- * 'ccgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ccgPublicIp' @::@ 'Text'
--
-- * 'ccgType' @::@ 'Text'
--
createCustomerGateway :: Text -- ^ 'ccgType'
                      -> Text -- ^ 'ccgPublicIp'
                      -> Int -- ^ 'ccgBgpAsn'
                      -> CreateCustomerGateway
createCustomerGateway p1 p2 p3 = CreateCustomerGateway
    { _ccgType     = p1
    , _ccgPublicIp = p2
    , _ccgBgpAsn   = p3
    , _ccgDryRun   = Nothing
    }

-- | For devices that support BGP, the customer gateway's BGP ASN. Default:
-- 65000.
ccgBgpAsn :: Lens' CreateCustomerGateway Int
ccgBgpAsn = lens _ccgBgpAsn (\s a -> s { _ccgBgpAsn = a })

ccgDryRun :: Lens' CreateCustomerGateway (Maybe Bool)
ccgDryRun = lens _ccgDryRun (\s a -> s { _ccgDryRun = a })

-- | The Internet-routable IP address for the customer gateway's outside
-- interface. The address must be static.
ccgPublicIp :: Lens' CreateCustomerGateway Text
ccgPublicIp = lens _ccgPublicIp (\s a -> s { _ccgPublicIp = a })

-- | The type of VPN connection that this customer gateway supports (ipsec.1).
ccgType :: Lens' CreateCustomerGateway Text
ccgType = lens _ccgType (\s a -> s { _ccgType = a })

newtype CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { _ccgrCustomerGateway :: Maybe CustomerGateway
    } deriving (Eq, Show, Generic)

-- | 'CreateCustomerGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccgrCustomerGateway' @::@ 'Maybe' 'CustomerGateway'
--
createCustomerGatewayResponse :: CreateCustomerGatewayResponse
createCustomerGatewayResponse = CreateCustomerGatewayResponse
    { _ccgrCustomerGateway = Nothing
    }

-- | Information about the customer gateway.
ccgrCustomerGateway :: Lens' CreateCustomerGatewayResponse (Maybe CustomerGateway)
ccgrCustomerGateway =
    lens _ccgrCustomerGateway (\s a -> s { _ccgrCustomerGateway = a })

instance AWSRequest CreateCustomerGateway where
    type Sv CreateCustomerGateway = EC2
    type Rs CreateCustomerGateway = CreateCustomerGatewayResponse

    request  = post "CreateCustomerGateway"
    response = xmlResponse

instance FromXML CreateCustomerGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCustomerGatewayResponse"

instance ToPath CreateCustomerGateway where
    toPath = const "/"

instance ToHeaders CreateCustomerGateway

instance ToQuery CreateCustomerGateway where
    toQuery CreateCustomerGateway{..} = mconcat
        [ "dryRun"    =? _ccgDryRun
        , "Type"      =? _ccgType
        , "IpAddress" =? _ccgPublicIp
        , "BgpAsn"    =? _ccgBgpAsn
        ]

instance ToXML CreateCustomerGateway where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateCustomerGateway"

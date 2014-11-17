{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpnConnection
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
-- Gateway to Your VPC in the Amazon Virtual Private Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnection.html>
module Network.AWS.EC2.CreateVpnConnection
    (
    -- * Request
      CreateVpnConnection
    -- ** Request constructor
    , createVpnConnection
    -- ** Request lenses
    , cvcCustomerGatewayId
    , cvcDryRun
    , cvcOptions
    , cvcType
    , cvcVpnGatewayId

    -- * Response
    , CreateVpnConnectionResponse
    -- ** Response constructor
    , createVpnConnectionResponse
    -- ** Response lenses
    , cvcrVpnConnection
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateVpnConnection = CreateVpnConnection
    { _cvcCustomerGatewayId :: Text
    , _cvcDryRun            :: Maybe Bool
    , _cvcOptions           :: Maybe VpnConnectionOptionsSpecification
    , _cvcType              :: Text
    , _cvcVpnGatewayId      :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreateVpnConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcCustomerGatewayId' @::@ 'Text'
--
-- * 'cvcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvcOptions' @::@ 'Maybe' 'VpnConnectionOptionsSpecification'
--
-- * 'cvcType' @::@ 'Text'
--
-- * 'cvcVpnGatewayId' @::@ 'Text'
--
createVpnConnection :: Text -- ^ 'cvcType'
                    -> Text -- ^ 'cvcCustomerGatewayId'
                    -> Text -- ^ 'cvcVpnGatewayId'
                    -> CreateVpnConnection
createVpnConnection p1 p2 p3 = CreateVpnConnection
    { _cvcType              = p1
    , _cvcCustomerGatewayId = p2
    , _cvcVpnGatewayId      = p3
    , _cvcDryRun            = Nothing
    , _cvcOptions           = Nothing
    }

-- | The ID of the customer gateway.
cvcCustomerGatewayId :: Lens' CreateVpnConnection Text
cvcCustomerGatewayId =
    lens _cvcCustomerGatewayId (\s a -> s { _cvcCustomerGatewayId = a })

cvcDryRun :: Lens' CreateVpnConnection (Maybe Bool)
cvcDryRun = lens _cvcDryRun (\s a -> s { _cvcDryRun = a })

-- | Indicates whether the VPN connection requires static routes. If you are
-- creating a VPN connection for a device that does not support BGP, you
-- must specify true. Default: false.
cvcOptions :: Lens' CreateVpnConnection (Maybe VpnConnectionOptionsSpecification)
cvcOptions = lens _cvcOptions (\s a -> s { _cvcOptions = a })

-- | The type of VPN connection (ipsec.1).
cvcType :: Lens' CreateVpnConnection Text
cvcType = lens _cvcType (\s a -> s { _cvcType = a })

-- | The ID of the virtual private gateway.
cvcVpnGatewayId :: Lens' CreateVpnConnection Text
cvcVpnGatewayId = lens _cvcVpnGatewayId (\s a -> s { _cvcVpnGatewayId = a })

newtype CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { _cvcrVpnConnection :: Maybe VpnConnection
    } deriving (Eq, Show, Generic)

-- | 'CreateVpnConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcrVpnConnection' @::@ 'Maybe' 'VpnConnection'
--
createVpnConnectionResponse :: CreateVpnConnectionResponse
createVpnConnectionResponse = CreateVpnConnectionResponse
    { _cvcrVpnConnection = Nothing
    }

-- | Information about the VPN connection.
cvcrVpnConnection :: Lens' CreateVpnConnectionResponse (Maybe VpnConnection)
cvcrVpnConnection =
    lens _cvcrVpnConnection (\s a -> s { _cvcrVpnConnection = a })

instance ToPath CreateVpnConnection where
    toPath = const "/"

instance ToQuery CreateVpnConnection

instance ToHeaders CreateVpnConnection

instance AWSRequest CreateVpnConnection where
    type Sv CreateVpnConnection = EC2
    type Rs CreateVpnConnection = CreateVpnConnectionResponse

    request  = post "CreateVpnConnection"
    response = xmlResponse

instance FromXML CreateVpnConnectionResponse where
    parseXML c = CreateVpnConnectionResponse
        <$> c .:? "vpnConnection"

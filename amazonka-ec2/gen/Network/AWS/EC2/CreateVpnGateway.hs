{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.CreateVpnGateway
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
-- Your VPC in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateVpnGateway
    (
    -- * Request
      CreateVpnGateway
    -- ** Request constructor
    , createVpnGateway
    -- ** Request lenses
    , cvgAvailabilityZone
    , cvgDryRun
    , cvgType

    -- * Response
    , CreateVpnGatewayResult
    -- ** Response constructor
    , createVpnGatewayResult
    -- ** Response lenses
    , cvgrVpnGateway
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateVpnGateway = CreateVpnGateway
    { _cvgAvailabilityZone :: Maybe Text
    , _cvgDryRun           :: Maybe Bool
    , _cvgType             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVpnGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cvgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvgType' @::@ 'Text'
--
createVpnGateway :: Text -- ^ 'cvgType'
                 -> CreateVpnGateway
createVpnGateway p1 = CreateVpnGateway
    { _cvgType             = p1
    , _cvgDryRun           = Nothing
    , _cvgAvailabilityZone = Nothing
    }

-- | The Availability Zone for the virtual private gateway.
cvgAvailabilityZone :: Lens' CreateVpnGateway (Maybe Text)
cvgAvailabilityZone =
    lens _cvgAvailabilityZone (\s a -> s { _cvgAvailabilityZone = a })

cvgDryRun :: Lens' CreateVpnGateway (Maybe Bool)
cvgDryRun = lens _cvgDryRun (\s a -> s { _cvgDryRun = a })

-- | The type of VPN connection this virtual private gateway supports.
cvgType :: Lens' CreateVpnGateway Text
cvgType = lens _cvgType (\s a -> s { _cvgType = a })

instance ToPath CreateVpnGateway where
    toPath = const "/"

instance ToQuery CreateVpnGateway

newtype CreateVpnGatewayResult = CreateVpnGatewayResult
    { _cvgrVpnGateway :: Maybe VpnGateway
    } deriving (Eq, Show, Generic)

-- | 'CreateVpnGatewayResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvgrVpnGateway' @::@ 'Maybe' 'VpnGateway'
--
createVpnGatewayResult :: CreateVpnGatewayResult
createVpnGatewayResult = CreateVpnGatewayResult
    { _cvgrVpnGateway = Nothing
    }

-- | Information about the virtual private gateway.
cvgrVpnGateway :: Lens' CreateVpnGatewayResult (Maybe VpnGateway)
cvgrVpnGateway = lens _cvgrVpnGateway (\s a -> s { _cvgrVpnGateway = a })

instance AWSRequest CreateVpnGateway where
    type Sv CreateVpnGateway = EC2
    type Rs CreateVpnGateway = CreateVpnGatewayResult

    request  = post "CreateVpnGateway"
    response = const . xmlResponse $ \h x -> CreateVpnGatewayResult
        <$> x %| "vpnGateway"

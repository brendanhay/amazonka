{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateVpnGateway
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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpnGateway&gt; &lt;vpnGatewayId&gt;vgw-8db04f81&lt;/vpnGatewayId&gt;
-- &lt;state&gt;pending&lt;/state&gt; &lt;type&gt;ipsec.1&lt;/type&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;attachments/&gt; &lt;tagSet/&gt; &lt;/vpnGateway&gt;
-- &lt;/CreateVpnGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateVpnGateway where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'CreateVpnGateway' request.
createVpnGateway :: GatewayType -- ^ '_cvgrType'
                 -> CreateVpnGateway
createVpnGateway p1 = CreateVpnGateway
    { _cvgrType = p1
    , _cvgrDryRun = Nothing
    , _cvgrAvailabilityZone = Nothing
    }

data CreateVpnGateway = CreateVpnGateway
    { _cvgrType :: GatewayType
      -- ^ The type of VPN connection this virtual private gateway supports.
    , _cvgrDryRun :: Maybe Bool
      -- ^ 
    , _cvgrAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the virtual private gateway.
    } deriving (Generic)

instance ToQuery CreateVpnGateway where
    toQuery = genericToQuery def

instance AWSRequest CreateVpnGateway where
    type Sv CreateVpnGateway = EC2
    type Rs CreateVpnGateway = CreateVpnGatewayResponse

    request = post "CreateVpnGateway"
    response _ = xmlResponse

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { _cvgsVpnGateway :: Maybe VpnGateway
      -- ^ Information about the virtual private gateway.
    } deriving (Generic)

instance FromXML CreateVpnGatewayResponse where
    fromXMLOptions = xmlOptions

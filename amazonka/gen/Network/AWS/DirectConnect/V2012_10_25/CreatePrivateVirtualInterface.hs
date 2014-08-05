{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
module Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { _cpviwConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _cpviwNewPrivateVirtualInterface :: NewPrivateVirtualInterface
      -- ^ Detailed information for the private virtual interface to be
      -- created. Default: None.
    } deriving (Show, Generic)

makeLenses ''CreatePrivateVirtualInterface

instance ToPath CreatePrivateVirtualInterface

instance ToQuery CreatePrivateVirtualInterface

instance ToHeaders CreatePrivateVirtualInterface

instance ToJSON CreatePrivateVirtualInterface

data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _vmAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vmAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vmAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vmConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vmCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vmLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vmOwnerAccount :: Maybe Text
    , _vmRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _vmCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vmVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vmVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vmVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vmVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vmVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the
      -- virtual interface is pending confirmation from the virtual
      -- interface owner. If the owner of the virtual interface is
      -- different from the owner of the connection on which it is
      -- provisioned, then the virtual interface will remain in this state
      -- until it is confirmed by the virtual interface owner. Verifying:
      -- This state only applies to public virtual interfaces. Each public
      -- virtual interface needs validation before the virtual interface
      -- can be created. Pending: A virtual interface is in this state
      -- from the time that it is created until the virtual interface is
      -- ready to forward traffic. Available: A virtual interface that is
      -- able to forward traffic. Deleting: A virtual interface is in this
      -- state immediately after calling DeleteVirtualInterface until it
      -- can no longer forward traffic. Deleted: A virtual interface that
      -- cannot forward traffic. Rejected: The virtual interface owner has
      -- declined creation of the virtual interface. If a virtual
      -- interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected'
      -- state.
    , _vmVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    } deriving (Show, Generic)

makeLenses ''CreatePrivateVirtualInterfaceResponse

instance FromJSON CreatePrivateVirtualInterfaceResponse

instance AWSRequest CreatePrivateVirtualInterface where
    type Sv CreatePrivateVirtualInterface = DirectConnect
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

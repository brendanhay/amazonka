{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface
    (
    -- * Request
      CreatePrivateVirtualInterface
    -- ** Request constructor
    , mkCreatePrivateVirtualInterfaceRequest
    -- ** Request lenses
    , cpvivConnectionId
    , cpvivNewPrivateVirtualInterface

    -- * Response
    , CreatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , vkOwnerAccount
    , vkVirtualInterfaceId
    , vkLocation
    , vkConnectionId
    , vkVirtualInterfaceType
    , vkVirtualInterfaceName
    , vkVlan
    , vkAsn
    , vkAuthKey
    , vkAmazonAddress
    , vkCustomerAddress
    , vkVirtualInterfaceState
    , vkCustomerRouterConfig
    , vkVirtualGatewayId
    , vkRouteFilterPrefixes
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePrivateVirtualInterface' request.
mkCreatePrivateVirtualInterfaceRequest :: Text -- ^ 'cpvivConnectionId'
                                       -> NewPrivateVirtualInterface -- ^ 'cpvivNewPrivateVirtualInterface'
                                       -> CreatePrivateVirtualInterface
mkCreatePrivateVirtualInterfaceRequest p1 p2 = CreatePrivateVirtualInterface
    { _cpvivConnectionId = p1
    , _cpvivNewPrivateVirtualInterface = p2
    }
{-# INLINE mkCreatePrivateVirtualInterfaceRequest #-}

data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { _cpvivConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _cpvivNewPrivateVirtualInterface :: NewPrivateVirtualInterface
      -- ^ Detailed information for the private virtual interface to be
      -- created. Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvivConnectionId :: Lens' CreatePrivateVirtualInterface (Text)
cpvivConnectionId = lens _cpvivConnectionId (\s a -> s { _cpvivConnectionId = a })
{-# INLINE cpvivConnectionId #-}

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
cpvivNewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface (NewPrivateVirtualInterface)
cpvivNewPrivateVirtualInterface = lens _cpvivNewPrivateVirtualInterface (\s a -> s { _cpvivNewPrivateVirtualInterface = a })
{-# INLINE cpvivNewPrivateVirtualInterface #-}

instance ToPath CreatePrivateVirtualInterface

instance ToQuery CreatePrivateVirtualInterface

instance ToHeaders CreatePrivateVirtualInterface

instance ToJSON CreatePrivateVirtualInterface

data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _vkOwnerAccount :: Maybe Text
    , _vkVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vkLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vkConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vkVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _vkVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vkVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vkAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vkAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vkAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vkCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vkVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , _vkCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vkVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vkRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

vkOwnerAccount :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkOwnerAccount = lens _vkOwnerAccount (\s a -> s { _vkOwnerAccount = a })
{-# INLINE vkOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vkVirtualInterfaceId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkVirtualInterfaceId = lens _vkVirtualInterfaceId (\s a -> s { _vkVirtualInterfaceId = a })
{-# INLINE vkVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vkLocation :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkLocation = lens _vkLocation (\s a -> s { _vkLocation = a })
{-# INLINE vkLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vkConnectionId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkConnectionId = lens _vkConnectionId (\s a -> s { _vkConnectionId = a })
{-# INLINE vkConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vkVirtualInterfaceType :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkVirtualInterfaceType = lens _vkVirtualInterfaceType (\s a -> s { _vkVirtualInterfaceType = a })
{-# INLINE vkVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vkVirtualInterfaceName :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkVirtualInterfaceName = lens _vkVirtualInterfaceName (\s a -> s { _vkVirtualInterfaceName = a })
{-# INLINE vkVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
vkVlan :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
vkVlan = lens _vkVlan (\s a -> s { _vkVlan = a })
{-# INLINE vkVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vkAsn :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
vkAsn = lens _vkAsn (\s a -> s { _vkAsn = a })
{-# INLINE vkAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vkAuthKey :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkAuthKey = lens _vkAuthKey (\s a -> s { _vkAuthKey = a })
{-# INLINE vkAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vkAmazonAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkAmazonAddress = lens _vkAmazonAddress (\s a -> s { _vkAmazonAddress = a })
{-# INLINE vkAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vkCustomerAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkCustomerAddress = lens _vkCustomerAddress (\s a -> s { _vkCustomerAddress = a })
{-# INLINE vkCustomerAddress #-}

-- | State of the virtual interface. Confirming: The creation of the virtual
-- interface is pending confirmation from the virtual interface owner. If the
-- owner of the virtual interface is different from the owner of the
-- connection on which it is provisioned, then the virtual interface will
-- remain in this state until it is confirmed by the virtual interface owner.
-- Verifying: This state only applies to public virtual interfaces. Each
-- public virtual interface needs validation before the virtual interface can
-- be created. Pending: A virtual interface is in this state from the time
-- that it is created until the virtual interface is ready to forward traffic.
-- Available: A virtual interface that is able to forward traffic. Deleting: A
-- virtual interface is in this state immediately after calling
-- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
-- virtual interface that cannot forward traffic. Rejected: The virtual
-- interface owner has declined creation of the virtual interface. If a
-- virtual interface in the 'Confirming' state is deleted by the virtual
-- interface owner, the virtual interface will enter the 'Rejected' state.
vkVirtualInterfaceState :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
vkVirtualInterfaceState = lens _vkVirtualInterfaceState (\s a -> s { _vkVirtualInterfaceState = a })
{-# INLINE vkVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
vkCustomerRouterConfig :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkCustomerRouterConfig = lens _vkCustomerRouterConfig (\s a -> s { _vkCustomerRouterConfig = a })
{-# INLINE vkCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vkVirtualGatewayId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
vkVirtualGatewayId = lens _vkVirtualGatewayId (\s a -> s { _vkVirtualGatewayId = a })
{-# INLINE vkVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vkRouteFilterPrefixes :: Lens' CreatePrivateVirtualInterfaceResponse ([RouteFilterPrefix])
vkRouteFilterPrefixes = lens _vkRouteFilterPrefixes (\s a -> s { _vkRouteFilterPrefixes = a })
{-# INLINE vkRouteFilterPrefixes #-}

instance FromJSON CreatePrivateVirtualInterfaceResponse

instance AWSRequest CreatePrivateVirtualInterface where
    type Sv CreatePrivateVirtualInterface = DirectConnect
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

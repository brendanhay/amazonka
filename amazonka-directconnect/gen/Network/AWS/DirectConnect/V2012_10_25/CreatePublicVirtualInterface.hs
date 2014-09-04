{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
module Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface
    (
    -- * Request
      CreatePublicVirtualInterface
    -- ** Request constructor
    , mkCreatePublicVirtualInterfaceRequest
    -- ** Request lenses
    , cpviwConnectionId
    , cpviwNewPublicVirtualInterface

    -- * Response
    , CreatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , vlOwnerAccount
    , vlVirtualInterfaceId
    , vlLocation
    , vlConnectionId
    , vlVirtualInterfaceType
    , vlVirtualInterfaceName
    , vlVlan
    , vlAsn
    , vlAuthKey
    , vlAmazonAddress
    , vlCustomerAddress
    , vlVirtualInterfaceState
    , vlCustomerRouterConfig
    , vlVirtualGatewayId
    , vlRouteFilterPrefixes
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePublicVirtualInterface' request.
mkCreatePublicVirtualInterfaceRequest :: Text -- ^ 'cpviwConnectionId'
                                      -> NewPublicVirtualInterface -- ^ 'cpviwNewPublicVirtualInterface'
                                      -> CreatePublicVirtualInterface
mkCreatePublicVirtualInterfaceRequest p1 p2 = CreatePublicVirtualInterface
    { _cpviwConnectionId = p1
    , _cpviwNewPublicVirtualInterface = p2
    }
{-# INLINE mkCreatePublicVirtualInterfaceRequest #-}

data CreatePublicVirtualInterface = CreatePublicVirtualInterface
    { _cpviwConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _cpviwNewPublicVirtualInterface :: NewPublicVirtualInterface
      -- ^ Detailed information for the public virtual interface to be
      -- created. Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpviwConnectionId :: Lens' CreatePublicVirtualInterface (Text)
cpviwConnectionId = lens _cpviwConnectionId (\s a -> s { _cpviwConnectionId = a })
{-# INLINE cpviwConnectionId #-}

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
cpviwNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface (NewPublicVirtualInterface)
cpviwNewPublicVirtualInterface = lens _cpviwNewPublicVirtualInterface (\s a -> s { _cpviwNewPublicVirtualInterface = a })
{-# INLINE cpviwNewPublicVirtualInterface #-}

instance ToPath CreatePublicVirtualInterface

instance ToQuery CreatePublicVirtualInterface

instance ToHeaders CreatePublicVirtualInterface

instance ToJSON CreatePublicVirtualInterface

data CreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _vlOwnerAccount :: Maybe Text
    , _vlVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vlLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vlConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vlVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _vlVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vlVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vlAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vlAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vlAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vlCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vlVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , _vlCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vlVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vlRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

vlOwnerAccount :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlOwnerAccount = lens _vlOwnerAccount (\s a -> s { _vlOwnerAccount = a })
{-# INLINE vlOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vlVirtualInterfaceId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceId = lens _vlVirtualInterfaceId (\s a -> s { _vlVirtualInterfaceId = a })
{-# INLINE vlVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vlLocation :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlLocation = lens _vlLocation (\s a -> s { _vlLocation = a })
{-# INLINE vlLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vlConnectionId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlConnectionId = lens _vlConnectionId (\s a -> s { _vlConnectionId = a })
{-# INLINE vlConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vlVirtualInterfaceType :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceType = lens _vlVirtualInterfaceType (\s a -> s { _vlVirtualInterfaceType = a })
{-# INLINE vlVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vlVirtualInterfaceName :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceName = lens _vlVirtualInterfaceName (\s a -> s { _vlVirtualInterfaceName = a })
{-# INLINE vlVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
vlVlan :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
vlVlan = lens _vlVlan (\s a -> s { _vlVlan = a })
{-# INLINE vlVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vlAsn :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
vlAsn = lens _vlAsn (\s a -> s { _vlAsn = a })
{-# INLINE vlAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vlAuthKey :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlAuthKey = lens _vlAuthKey (\s a -> s { _vlAuthKey = a })
{-# INLINE vlAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vlAmazonAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlAmazonAddress = lens _vlAmazonAddress (\s a -> s { _vlAmazonAddress = a })
{-# INLINE vlAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vlCustomerAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlCustomerAddress = lens _vlCustomerAddress (\s a -> s { _vlCustomerAddress = a })
{-# INLINE vlCustomerAddress #-}

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
vlVirtualInterfaceState :: Lens' CreatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
vlVirtualInterfaceState = lens _vlVirtualInterfaceState (\s a -> s { _vlVirtualInterfaceState = a })
{-# INLINE vlVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
vlCustomerRouterConfig :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlCustomerRouterConfig = lens _vlCustomerRouterConfig (\s a -> s { _vlCustomerRouterConfig = a })
{-# INLINE vlCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vlVirtualGatewayId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualGatewayId = lens _vlVirtualGatewayId (\s a -> s { _vlVirtualGatewayId = a })
{-# INLINE vlVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vlRouteFilterPrefixes :: Lens' CreatePublicVirtualInterfaceResponse ([RouteFilterPrefix])
vlRouteFilterPrefixes = lens _vlRouteFilterPrefixes (\s a -> s { _vlRouteFilterPrefixes = a })
{-# INLINE vlRouteFilterPrefixes #-}

instance FromJSON CreatePublicVirtualInterfaceResponse

instance AWSRequest CreatePublicVirtualInterface where
    type Sv CreatePublicVirtualInterface = DirectConnect
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

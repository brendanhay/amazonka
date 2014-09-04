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
    , createPublicVirtualInterface
    -- ** Request lenses
    , cpviwConnectionId
    , cpviwNewPublicVirtualInterface

    -- * Response
    , CreatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , vlAsn
    , vlAmazonAddress
    , vlAuthKey
    , vlConnectionId
    , vlCustomerAddress
    , vlLocation
    , vlOwnerAccount
    , vlRouteFilterPrefixes
    , vlCustomerRouterConfig
    , vlVlan
    , vlVirtualGatewayId
    , vlVirtualInterfaceId
    , vlVirtualInterfaceName
    , vlVirtualInterfaceState
    , vlVirtualInterfaceType
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreatePublicVirtualInterface' request.
createPublicVirtualInterface :: Text -- ^ 'cpviwConnectionId'
                             -> NewPublicVirtualInterface -- ^ 'cpviwNewPublicVirtualInterface'
                             -> CreatePublicVirtualInterface
createPublicVirtualInterface p1 p2 = CreatePublicVirtualInterface
    { _cpviwConnectionId = p1
    , _cpviwNewPublicVirtualInterface = p2
    }
{-# INLINE createPublicVirtualInterface #-}

data CreatePublicVirtualInterface = CreatePublicVirtualInterface
    { _cpviwConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _cpviwNewPublicVirtualInterface :: NewPublicVirtualInterface
      -- ^ Detailed information for the public virtual interface to be
      -- created. Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpviwConnectionId :: Lens' CreatePublicVirtualInterface (Text)
cpviwConnectionId f x =
    f (_cpviwConnectionId x)
        <&> \y -> x { _cpviwConnectionId = y }
{-# INLINE cpviwConnectionId #-}

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
cpviwNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface (NewPublicVirtualInterface)
cpviwNewPublicVirtualInterface f x =
    f (_cpviwNewPublicVirtualInterface x)
        <&> \y -> x { _cpviwNewPublicVirtualInterface = y }
{-# INLINE cpviwNewPublicVirtualInterface #-}

instance ToPath CreatePublicVirtualInterface

instance ToQuery CreatePublicVirtualInterface

instance ToHeaders CreatePublicVirtualInterface

instance ToJSON CreatePublicVirtualInterface

data CreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { _vlAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vlAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vlAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vlConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vlCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vlLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vlOwnerAccount :: Maybe Text
    , _vlRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _vlCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vlVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vlVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vlVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vlVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
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
    , _vlVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    } deriving (Show, Generic)

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vlAsn :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
vlAsn f x =
    f (_vlAsn x)
        <&> \y -> x { _vlAsn = y }
{-# INLINE vlAsn #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vlAmazonAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlAmazonAddress f x =
    f (_vlAmazonAddress x)
        <&> \y -> x { _vlAmazonAddress = y }
{-# INLINE vlAmazonAddress #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vlAuthKey :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlAuthKey f x =
    f (_vlAuthKey x)
        <&> \y -> x { _vlAuthKey = y }
{-# INLINE vlAuthKey #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vlConnectionId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlConnectionId f x =
    f (_vlConnectionId x)
        <&> \y -> x { _vlConnectionId = y }
{-# INLINE vlConnectionId #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vlCustomerAddress :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlCustomerAddress f x =
    f (_vlCustomerAddress x)
        <&> \y -> x { _vlCustomerAddress = y }
{-# INLINE vlCustomerAddress #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vlLocation :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlLocation f x =
    f (_vlLocation x)
        <&> \y -> x { _vlLocation = y }
{-# INLINE vlLocation #-}

vlOwnerAccount :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlOwnerAccount f x =
    f (_vlOwnerAccount x)
        <&> \y -> x { _vlOwnerAccount = y }
{-# INLINE vlOwnerAccount #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vlRouteFilterPrefixes :: Lens' CreatePublicVirtualInterfaceResponse ([RouteFilterPrefix])
vlRouteFilterPrefixes f x =
    f (_vlRouteFilterPrefixes x)
        <&> \y -> x { _vlRouteFilterPrefixes = y }
{-# INLINE vlRouteFilterPrefixes #-}

-- | Information for generating the customer router configuration.
vlCustomerRouterConfig :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlCustomerRouterConfig f x =
    f (_vlCustomerRouterConfig x)
        <&> \y -> x { _vlCustomerRouterConfig = y }
{-# INLINE vlCustomerRouterConfig #-}

-- | The VLAN ID. Example: 101.
vlVlan :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Integer)
vlVlan f x =
    f (_vlVlan x)
        <&> \y -> x { _vlVlan = y }
{-# INLINE vlVlan #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vlVirtualGatewayId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualGatewayId f x =
    f (_vlVirtualGatewayId x)
        <&> \y -> x { _vlVirtualGatewayId = y }
{-# INLINE vlVirtualGatewayId #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vlVirtualInterfaceId :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceId f x =
    f (_vlVirtualInterfaceId x)
        <&> \y -> x { _vlVirtualInterfaceId = y }
{-# INLINE vlVirtualInterfaceId #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vlVirtualInterfaceName :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceName f x =
    f (_vlVirtualInterfaceName x)
        <&> \y -> x { _vlVirtualInterfaceName = y }
{-# INLINE vlVirtualInterfaceName #-}

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
vlVirtualInterfaceState f x =
    f (_vlVirtualInterfaceState x)
        <&> \y -> x { _vlVirtualInterfaceState = y }
{-# INLINE vlVirtualInterfaceState #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vlVirtualInterfaceType :: Lens' CreatePublicVirtualInterfaceResponse (Maybe Text)
vlVirtualInterfaceType f x =
    f (_vlVirtualInterfaceType x)
        <&> \y -> x { _vlVirtualInterfaceType = y }
{-# INLINE vlVirtualInterfaceType #-}

instance FromJSON CreatePublicVirtualInterfaceResponse

instance AWSRequest CreatePublicVirtualInterface where
    type Sv CreatePublicVirtualInterface = DirectConnect
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

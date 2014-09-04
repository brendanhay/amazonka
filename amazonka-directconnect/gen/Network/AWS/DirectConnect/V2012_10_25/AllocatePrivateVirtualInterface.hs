{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a private virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a private
-- virtual interface which will be owned by another AWS customer. Virtual
-- interfaces created using this function must be confirmed by the virtual
-- interface owner by calling ConfirmPrivateVirtualInterface. Until this step
-- has been completed, the virtual interface will be in 'Confirming' state,
-- and will not be available for handling traffic.
module Network.AWS.DirectConnect.V2012_10_25.AllocatePrivateVirtualInterface
    (
    -- * Request
      AllocatePrivateVirtualInterface
    -- ** Request constructor
    , allocatePrivateVirtualInterface
    -- ** Request lenses
    , apvirConnectionId
    , apvirNewPrivateVirtualInterfaceAllocation
    , apvirOwnerAccount

    -- * Response
    , AllocatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , viAsn
    , viAmazonAddress
    , viAuthKey
    , viConnectionId
    , viCustomerAddress
    , viLocation
    , viOwnerAccount
    , viRouteFilterPrefixes
    , viCustomerRouterConfig
    , viVlan
    , viVirtualGatewayId
    , viVirtualInterfaceId
    , viVirtualInterfaceName
    , viVirtualInterfaceState
    , viVirtualInterfaceType
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AllocatePrivateVirtualInterface' request.
allocatePrivateVirtualInterface :: Text -- ^ 'apvirConnectionId'
                                -> NewPrivateVirtualInterfaceAllocation -- ^ 'apvirNewPrivateVirtualInterfaceAllocation'
                                -> Text -- ^ 'apvirOwnerAccount'
                                -> AllocatePrivateVirtualInterface
allocatePrivateVirtualInterface p1 p2 p3 = AllocatePrivateVirtualInterface
    { _apvirConnectionId = p1
    , _apvirNewPrivateVirtualInterfaceAllocation = p2
    , _apvirOwnerAccount = p3
    }
{-# INLINE allocatePrivateVirtualInterface #-}

data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { _apvirConnectionId :: Text
      -- ^ The connection ID on which the private virtual interface is
      -- provisioned. Default: None.
    , _apvirNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
      -- ^ Detailed information for the private virtual interface to be
      -- provisioned. Default: None.
    , _apvirOwnerAccount :: Text
      -- ^ The AWS account that will own the new private virtual interface.
      -- Default: None.
    } deriving (Show, Generic)

-- | The connection ID on which the private virtual interface is provisioned.
-- Default: None.
apvirConnectionId :: Lens' AllocatePrivateVirtualInterface (Text)
apvirConnectionId f x =
    f (_apvirConnectionId x)
        <&> \y -> x { _apvirConnectionId = y }
{-# INLINE apvirConnectionId #-}

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
apvirNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface (NewPrivateVirtualInterfaceAllocation)
apvirNewPrivateVirtualInterfaceAllocation f x =
    f (_apvirNewPrivateVirtualInterfaceAllocation x)
        <&> \y -> x { _apvirNewPrivateVirtualInterfaceAllocation = y }
{-# INLINE apvirNewPrivateVirtualInterfaceAllocation #-}

-- | The AWS account that will own the new private virtual interface. Default:
-- None.
apvirOwnerAccount :: Lens' AllocatePrivateVirtualInterface (Text)
apvirOwnerAccount f x =
    f (_apvirOwnerAccount x)
        <&> \y -> x { _apvirOwnerAccount = y }
{-# INLINE apvirOwnerAccount #-}

instance ToPath AllocatePrivateVirtualInterface

instance ToQuery AllocatePrivateVirtualInterface

instance ToHeaders AllocatePrivateVirtualInterface

instance ToJSON AllocatePrivateVirtualInterface

data AllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _viAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _viAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _viAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _viConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _viCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _viLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _viOwnerAccount :: Maybe Text
    , _viRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _viCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _viVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _viVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _viVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _viVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _viVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , _viVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    } deriving (Show, Generic)

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
viAsn :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
viAsn f x =
    f (_viAsn x)
        <&> \y -> x { _viAsn = y }
{-# INLINE viAsn #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
viAmazonAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viAmazonAddress f x =
    f (_viAmazonAddress x)
        <&> \y -> x { _viAmazonAddress = y }
{-# INLINE viAmazonAddress #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
viAuthKey :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viAuthKey f x =
    f (_viAuthKey x)
        <&> \y -> x { _viAuthKey = y }
{-# INLINE viAuthKey #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
viConnectionId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viConnectionId f x =
    f (_viConnectionId x)
        <&> \y -> x { _viConnectionId = y }
{-# INLINE viConnectionId #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
viCustomerAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viCustomerAddress f x =
    f (_viCustomerAddress x)
        <&> \y -> x { _viCustomerAddress = y }
{-# INLINE viCustomerAddress #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
viLocation :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viLocation f x =
    f (_viLocation x)
        <&> \y -> x { _viLocation = y }
{-# INLINE viLocation #-}

viOwnerAccount :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viOwnerAccount f x =
    f (_viOwnerAccount x)
        <&> \y -> x { _viOwnerAccount = y }
{-# INLINE viOwnerAccount #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
viRouteFilterPrefixes :: Lens' AllocatePrivateVirtualInterfaceResponse ([RouteFilterPrefix])
viRouteFilterPrefixes f x =
    f (_viRouteFilterPrefixes x)
        <&> \y -> x { _viRouteFilterPrefixes = y }
{-# INLINE viRouteFilterPrefixes #-}

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viCustomerRouterConfig f x =
    f (_viCustomerRouterConfig x)
        <&> \y -> x { _viCustomerRouterConfig = y }
{-# INLINE viCustomerRouterConfig #-}

-- | The VLAN ID. Example: 101.
viVlan :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
viVlan f x =
    f (_viVlan x)
        <&> \y -> x { _viVlan = y }
{-# INLINE viVlan #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
viVirtualGatewayId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualGatewayId f x =
    f (_viVirtualGatewayId x)
        <&> \y -> x { _viVirtualGatewayId = y }
{-# INLINE viVirtualGatewayId #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
viVirtualInterfaceId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceId f x =
    f (_viVirtualInterfaceId x)
        <&> \y -> x { _viVirtualInterfaceId = y }
{-# INLINE viVirtualInterfaceId #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
viVirtualInterfaceName :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceName f x =
    f (_viVirtualInterfaceName x)
        <&> \y -> x { _viVirtualInterfaceName = y }
{-# INLINE viVirtualInterfaceName #-}

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
viVirtualInterfaceState :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
viVirtualInterfaceState f x =
    f (_viVirtualInterfaceState x)
        <&> \y -> x { _viVirtualInterfaceState = y }
{-# INLINE viVirtualInterfaceState #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
viVirtualInterfaceType :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceType f x =
    f (_viVirtualInterfaceType x)
        <&> \y -> x { _viVirtualInterfaceType = y }
{-# INLINE viVirtualInterfaceType #-}

instance FromJSON AllocatePrivateVirtualInterfaceResponse

instance AWSRequest AllocatePrivateVirtualInterface where
    type Sv AllocatePrivateVirtualInterface = DirectConnect
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

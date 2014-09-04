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
    , mkAllocatePrivateVirtualInterfaceRequest
    -- ** Request lenses
    , apvirConnectionId
    , apvirOwnerAccount
    , apvirNewPrivateVirtualInterfaceAllocation

    -- * Response
    , AllocatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , viOwnerAccount
    , viVirtualInterfaceId
    , viLocation
    , viConnectionId
    , viVirtualInterfaceType
    , viVirtualInterfaceName
    , viVlan
    , viAsn
    , viAuthKey
    , viAmazonAddress
    , viCustomerAddress
    , viVirtualInterfaceState
    , viCustomerRouterConfig
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePrivateVirtualInterface' request.
mkAllocatePrivateVirtualInterfaceRequest :: Text -- ^ 'apvirConnectionId'
                                         -> Text -- ^ 'apvirOwnerAccount'
                                         -> NewPrivateVirtualInterfaceAllocation -- ^ 'apvirNewPrivateVirtualInterfaceAllocation'
                                         -> AllocatePrivateVirtualInterface
mkAllocatePrivateVirtualInterfaceRequest p1 p2 p3 = AllocatePrivateVirtualInterface
    { _apvirConnectionId = p1
    , _apvirOwnerAccount = p2
    , _apvirNewPrivateVirtualInterfaceAllocation = p3
    }
{-# INLINE mkAllocatePrivateVirtualInterfaceRequest #-}

data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { _apvirConnectionId :: Text
      -- ^ The connection ID on which the private virtual interface is
      -- provisioned. Default: None.
    , _apvirOwnerAccount :: Text
      -- ^ The AWS account that will own the new private virtual interface.
      -- Default: None.
    , _apvirNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
      -- ^ Detailed information for the private virtual interface to be
      -- provisioned. Default: None.
    } deriving (Show, Generic)

-- | The connection ID on which the private virtual interface is provisioned.
-- Default: None.
apvirConnectionId :: Lens' AllocatePrivateVirtualInterface (Text)
apvirConnectionId = lens _apvirConnectionId (\s a -> s { _apvirConnectionId = a })
{-# INLINE apvirConnectionId #-}

-- | The AWS account that will own the new private virtual interface. Default:
-- None.
apvirOwnerAccount :: Lens' AllocatePrivateVirtualInterface (Text)
apvirOwnerAccount = lens _apvirOwnerAccount (\s a -> s { _apvirOwnerAccount = a })
{-# INLINE apvirOwnerAccount #-}

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
apvirNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface (NewPrivateVirtualInterfaceAllocation)
apvirNewPrivateVirtualInterfaceAllocation = lens _apvirNewPrivateVirtualInterfaceAllocation (\s a -> s { _apvirNewPrivateVirtualInterfaceAllocation = a })
{-# INLINE apvirNewPrivateVirtualInterfaceAllocation #-}

instance ToPath AllocatePrivateVirtualInterface

instance ToQuery AllocatePrivateVirtualInterface

instance ToHeaders AllocatePrivateVirtualInterface

instance ToJSON AllocatePrivateVirtualInterface

data AllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _viOwnerAccount :: Maybe Text
    , _viVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _viLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _viConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _viVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _viVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _viVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _viAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _viAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _viAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _viCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
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
    , _viCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _viVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _viRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

viOwnerAccount :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viOwnerAccount = lens _viOwnerAccount (\s a -> s { _viOwnerAccount = a })
{-# INLINE viOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
viVirtualInterfaceId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceId = lens _viVirtualInterfaceId (\s a -> s { _viVirtualInterfaceId = a })
{-# INLINE viVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
viLocation :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viLocation = lens _viLocation (\s a -> s { _viLocation = a })
{-# INLINE viLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
viConnectionId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viConnectionId = lens _viConnectionId (\s a -> s { _viConnectionId = a })
{-# INLINE viConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
viVirtualInterfaceType :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceType = lens _viVirtualInterfaceType (\s a -> s { _viVirtualInterfaceType = a })
{-# INLINE viVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
viVirtualInterfaceName :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualInterfaceName = lens _viVirtualInterfaceName (\s a -> s { _viVirtualInterfaceName = a })
{-# INLINE viVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
viVlan :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
viVlan = lens _viVlan (\s a -> s { _viVlan = a })
{-# INLINE viVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
viAsn :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
viAsn = lens _viAsn (\s a -> s { _viAsn = a })
{-# INLINE viAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
viAuthKey :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viAuthKey = lens _viAuthKey (\s a -> s { _viAuthKey = a })
{-# INLINE viAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
viAmazonAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viAmazonAddress = lens _viAmazonAddress (\s a -> s { _viAmazonAddress = a })
{-# INLINE viAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
viCustomerAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viCustomerAddress = lens _viCustomerAddress (\s a -> s { _viCustomerAddress = a })
{-# INLINE viCustomerAddress #-}

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
viVirtualInterfaceState = lens _viVirtualInterfaceState (\s a -> s { _viVirtualInterfaceState = a })
{-# INLINE viVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
viCustomerRouterConfig :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viCustomerRouterConfig = lens _viCustomerRouterConfig (\s a -> s { _viCustomerRouterConfig = a })
{-# INLINE viCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
viVirtualGatewayId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
viVirtualGatewayId = lens _viVirtualGatewayId (\s a -> s { _viVirtualGatewayId = a })
{-# INLINE viVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
viRouteFilterPrefixes :: Lens' AllocatePrivateVirtualInterfaceResponse ([RouteFilterPrefix])
viRouteFilterPrefixes = lens _viRouteFilterPrefixes (\s a -> s { _viRouteFilterPrefixes = a })
{-# INLINE viRouteFilterPrefixes #-}

instance FromJSON AllocatePrivateVirtualInterfaceResponse

instance AWSRequest AllocatePrivateVirtualInterface where
    type Sv AllocatePrivateVirtualInterface = DirectConnect
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

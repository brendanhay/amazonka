{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a public virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a public virtual
-- interface which will be owned by another AWS customer. Virtual interfaces
-- created using this function must be confirmed by the virtual interface
-- owner by calling ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface will be in 'Confirming' state, and will
-- not be available for handling traffic.
module Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface
    (
    -- * Request
      AllocatePublicVirtualInterface
    -- ** Request constructor
    , mkAllocatePublicVirtualInterfaceRequest
    -- ** Request lenses
    , apvisConnectionId
    , apvisOwnerAccount
    , apvisNewPublicVirtualInterfaceAllocation

    -- * Response
    , AllocatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , vjOwnerAccount
    , vjVirtualInterfaceId
    , vjLocation
    , vjConnectionId
    , vjVirtualInterfaceType
    , vjVirtualInterfaceName
    , vjVlan
    , vjAsn
    , vjAuthKey
    , vjAmazonAddress
    , vjCustomerAddress
    , vjVirtualInterfaceState
    , vjCustomerRouterConfig
    , vjVirtualGatewayId
    , vjRouteFilterPrefixes
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePublicVirtualInterface' request.
mkAllocatePublicVirtualInterfaceRequest :: Text -- ^ 'apvisConnectionId'
                                        -> Text -- ^ 'apvisOwnerAccount'
                                        -> NewPublicVirtualInterfaceAllocation -- ^ 'apvisNewPublicVirtualInterfaceAllocation'
                                        -> AllocatePublicVirtualInterface
mkAllocatePublicVirtualInterfaceRequest p1 p2 p3 = AllocatePublicVirtualInterface
    { _apvisConnectionId = p1
    , _apvisOwnerAccount = p2
    , _apvisNewPublicVirtualInterfaceAllocation = p3
    }
{-# INLINE mkAllocatePublicVirtualInterfaceRequest #-}

data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface
    { _apvisConnectionId :: Text
      -- ^ The connection ID on which the public virtual interface is
      -- provisioned. Default: None.
    , _apvisOwnerAccount :: Text
      -- ^ The AWS account that will own the new public virtual interface.
      -- Default: None.
    , _apvisNewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
      -- ^ Detailed information for the public virtual interface to be
      -- provisioned. Default: None.
    } deriving (Show, Generic)

-- | The connection ID on which the public virtual interface is provisioned.
-- Default: None.
apvisConnectionId :: Lens' AllocatePublicVirtualInterface (Text)
apvisConnectionId = lens _apvisConnectionId (\s a -> s { _apvisConnectionId = a })
{-# INLINE apvisConnectionId #-}

-- | The AWS account that will own the new public virtual interface. Default:
-- None.
apvisOwnerAccount :: Lens' AllocatePublicVirtualInterface (Text)
apvisOwnerAccount = lens _apvisOwnerAccount (\s a -> s { _apvisOwnerAccount = a })
{-# INLINE apvisOwnerAccount #-}

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
apvisNewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface (NewPublicVirtualInterfaceAllocation)
apvisNewPublicVirtualInterfaceAllocation = lens _apvisNewPublicVirtualInterfaceAllocation (\s a -> s { _apvisNewPublicVirtualInterfaceAllocation = a })
{-# INLINE apvisNewPublicVirtualInterfaceAllocation #-}

instance ToPath AllocatePublicVirtualInterface

instance ToQuery AllocatePublicVirtualInterface

instance ToHeaders AllocatePublicVirtualInterface

instance ToJSON AllocatePublicVirtualInterface

data AllocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _vjOwnerAccount :: Maybe Text
    , _vjVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vjLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vjConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vjVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    , _vjVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vjVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vjAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vjAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vjAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vjCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vjVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , _vjCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vjVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vjRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    } deriving (Show, Generic)

vjOwnerAccount :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjOwnerAccount = lens _vjOwnerAccount (\s a -> s { _vjOwnerAccount = a })
{-# INLINE vjOwnerAccount #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vjVirtualInterfaceId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjVirtualInterfaceId = lens _vjVirtualInterfaceId (\s a -> s { _vjVirtualInterfaceId = a })
{-# INLINE vjVirtualInterfaceId #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vjLocation :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjLocation = lens _vjLocation (\s a -> s { _vjLocation = a })
{-# INLINE vjLocation #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vjConnectionId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjConnectionId = lens _vjConnectionId (\s a -> s { _vjConnectionId = a })
{-# INLINE vjConnectionId #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vjVirtualInterfaceType :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjVirtualInterfaceType = lens _vjVirtualInterfaceType (\s a -> s { _vjVirtualInterfaceType = a })
{-# INLINE vjVirtualInterfaceType #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vjVirtualInterfaceName :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjVirtualInterfaceName = lens _vjVirtualInterfaceName (\s a -> s { _vjVirtualInterfaceName = a })
{-# INLINE vjVirtualInterfaceName #-}

-- | The VLAN ID. Example: 101.
vjVlan :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
vjVlan = lens _vjVlan (\s a -> s { _vjVlan = a })
{-# INLINE vjVlan #-}

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vjAsn :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
vjAsn = lens _vjAsn (\s a -> s { _vjAsn = a })
{-# INLINE vjAsn #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vjAuthKey :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjAuthKey = lens _vjAuthKey (\s a -> s { _vjAuthKey = a })
{-# INLINE vjAuthKey #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vjAmazonAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjAmazonAddress = lens _vjAmazonAddress (\s a -> s { _vjAmazonAddress = a })
{-# INLINE vjAmazonAddress #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vjCustomerAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjCustomerAddress = lens _vjCustomerAddress (\s a -> s { _vjCustomerAddress = a })
{-# INLINE vjCustomerAddress #-}

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
vjVirtualInterfaceState :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
vjVirtualInterfaceState = lens _vjVirtualInterfaceState (\s a -> s { _vjVirtualInterfaceState = a })
{-# INLINE vjVirtualInterfaceState #-}

-- | Information for generating the customer router configuration.
vjCustomerRouterConfig :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjCustomerRouterConfig = lens _vjCustomerRouterConfig (\s a -> s { _vjCustomerRouterConfig = a })
{-# INLINE vjCustomerRouterConfig #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vjVirtualGatewayId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
vjVirtualGatewayId = lens _vjVirtualGatewayId (\s a -> s { _vjVirtualGatewayId = a })
{-# INLINE vjVirtualGatewayId #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vjRouteFilterPrefixes :: Lens' AllocatePublicVirtualInterfaceResponse ([RouteFilterPrefix])
vjRouteFilterPrefixes = lens _vjRouteFilterPrefixes (\s a -> s { _vjRouteFilterPrefixes = a })
{-# INLINE vjRouteFilterPrefixes #-}

instance FromJSON AllocatePublicVirtualInterfaceResponse

instance AWSRequest AllocatePublicVirtualInterface where
    type Sv AllocatePublicVirtualInterface = DirectConnect
    type Rs AllocatePublicVirtualInterface = AllocatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

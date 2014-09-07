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
    , mkAllocatePrivateVirtualInterface
    -- ** Request lenses
    , apviConnectionId
    , apviOwnerAccount
    , apviNewPrivateVirtualInterfaceAllocation

    -- * Response
    , AllocatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , apvirsOwnerAccount
    , apvirsVirtualInterfaceId
    , apvirsLocation
    , apvirsConnectionId
    , apvirsVirtualInterfaceType
    , apvirsVirtualInterfaceName
    , apvirsVlan
    , apvirsAsn
    , apvirsAuthKey
    , apvirsAmazonAddress
    , apvirsCustomerAddress
    , apvirsVirtualInterfaceState
    , apvirsCustomerRouterConfig
    , apvirsVirtualGatewayId
    , apvirsRouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the AllocatePrivateVirtualInterface
-- operation.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { _apviConnectionId :: Text
    , _apviOwnerAccount :: Text
    , _apviNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePrivateVirtualInterface' request.
mkAllocatePrivateVirtualInterface :: Text -- ^ 'apviConnectionId'
                                  -> Text -- ^ 'apviOwnerAccount'
                                  -> NewPrivateVirtualInterfaceAllocation -- ^ 'apviNewPrivateVirtualInterfaceAllocation'
                                  -> AllocatePrivateVirtualInterface
mkAllocatePrivateVirtualInterface p1 p2 p3 = AllocatePrivateVirtualInterface
    { _apviConnectionId = p1
    , _apviOwnerAccount = p2
    , _apviNewPrivateVirtualInterfaceAllocation = p3
    }

-- | The connection ID on which the private virtual interface is provisioned.
-- Default: None.
apviConnectionId :: Lens' AllocatePrivateVirtualInterface Text
apviConnectionId =
    lens _apviConnectionId (\s a -> s { _apviConnectionId = a })

-- | The AWS account that will own the new private virtual interface. Default:
-- None.
apviOwnerAccount :: Lens' AllocatePrivateVirtualInterface Text
apviOwnerAccount =
    lens _apviOwnerAccount (\s a -> s { _apviOwnerAccount = a })

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
apviNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation =
    lens _apviNewPrivateVirtualInterfaceAllocation
         (\s a -> s { _apviNewPrivateVirtualInterfaceAllocation = a })

instance ToPath AllocatePrivateVirtualInterface

instance ToQuery AllocatePrivateVirtualInterface

instance ToHeaders AllocatePrivateVirtualInterface

instance ToJSON AllocatePrivateVirtualInterface

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data AllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _apvirsOwnerAccount :: Maybe Text
    , _apvirsVirtualInterfaceId :: Maybe Text
    , _apvirsLocation :: Maybe Text
    , _apvirsConnectionId :: Maybe Text
    , _apvirsVirtualInterfaceType :: Maybe Text
    , _apvirsVirtualInterfaceName :: Maybe Text
    , _apvirsVlan :: Maybe Integer
    , _apvirsAsn :: Maybe Integer
    , _apvirsAuthKey :: Maybe Text
    , _apvirsAmazonAddress :: Maybe Text
    , _apvirsCustomerAddress :: Maybe Text
    , _apvirsVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _apvirsCustomerRouterConfig :: Maybe Text
    , _apvirsVirtualGatewayId :: Maybe Text
    , _apvirsRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

apvirsOwnerAccount :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsOwnerAccount =
    lens _apvirsOwnerAccount (\s a -> s { _apvirsOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
apvirsVirtualInterfaceId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsVirtualInterfaceId =
    lens _apvirsVirtualInterfaceId
         (\s a -> s { _apvirsVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
apvirsLocation :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsLocation = lens _apvirsLocation (\s a -> s { _apvirsLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
apvirsConnectionId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsConnectionId =
    lens _apvirsConnectionId (\s a -> s { _apvirsConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
apvirsVirtualInterfaceType :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsVirtualInterfaceType =
    lens _apvirsVirtualInterfaceType
         (\s a -> s { _apvirsVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
apvirsVirtualInterfaceName :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsVirtualInterfaceName =
    lens _apvirsVirtualInterfaceName
         (\s a -> s { _apvirsVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
apvirsVlan :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
apvirsVlan = lens _apvirsVlan (\s a -> s { _apvirsVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
apvirsAsn :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
apvirsAsn = lens _apvirsAsn (\s a -> s { _apvirsAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
apvirsAuthKey :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsAuthKey = lens _apvirsAuthKey (\s a -> s { _apvirsAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
apvirsAmazonAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsAmazonAddress =
    lens _apvirsAmazonAddress (\s a -> s { _apvirsAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
apvirsCustomerAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsCustomerAddress =
    lens _apvirsCustomerAddress (\s a -> s { _apvirsCustomerAddress = a })

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
apvirsVirtualInterfaceState :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
apvirsVirtualInterfaceState =
    lens _apvirsVirtualInterfaceState
         (\s a -> s { _apvirsVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
apvirsCustomerRouterConfig :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsCustomerRouterConfig =
    lens _apvirsCustomerRouterConfig
         (\s a -> s { _apvirsCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
apvirsVirtualGatewayId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirsVirtualGatewayId =
    lens _apvirsVirtualGatewayId (\s a -> s { _apvirsVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
apvirsRouteFilterPrefixes :: Lens' AllocatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
apvirsRouteFilterPrefixes =
    lens _apvirsRouteFilterPrefixes
         (\s a -> s { _apvirsRouteFilterPrefixes = a })

instance FromJSON AllocatePrivateVirtualInterfaceResponse

instance AWSRequest AllocatePrivateVirtualInterface where
    type Sv AllocatePrivateVirtualInterface = DirectConnect
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

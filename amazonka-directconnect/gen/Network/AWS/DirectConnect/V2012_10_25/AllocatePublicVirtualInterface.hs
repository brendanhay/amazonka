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
    , mkAllocatePublicVirtualInterface
    -- ** Request lenses
    , apvi1ConnectionId
    , apvi1OwnerAccount
    , apvi1NewPublicVirtualInterfaceAllocation

    -- * Response
    , AllocatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , apvirsrsOwnerAccount
    , apvirsrsVirtualInterfaceId
    , apvirsrsLocation
    , apvirsrsConnectionId
    , apvirsrsVirtualInterfaceType
    , apvirsrsVirtualInterfaceName
    , apvirsrsVlan
    , apvirsrsAsn
    , apvirsrsAuthKey
    , apvirsrsAmazonAddress
    , apvirsrsCustomerAddress
    , apvirsrsVirtualInterfaceState
    , apvirsrsCustomerRouterConfig
    , apvirsrsVirtualGatewayId
    , apvirsrsRouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the AllocatePublicVirtualInterface
-- operation.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface
    { _apvi1ConnectionId :: Text
    , _apvi1OwnerAccount :: Text
    , _apvi1NewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePublicVirtualInterface' request.
mkAllocatePublicVirtualInterface :: Text -- ^ 'apvi1ConnectionId'
                                 -> Text -- ^ 'apvi1OwnerAccount'
                                 -> NewPublicVirtualInterfaceAllocation -- ^ 'apvi1NewPublicVirtualInterfaceAllocation'
                                 -> AllocatePublicVirtualInterface
mkAllocatePublicVirtualInterface p1 p2 p3 = AllocatePublicVirtualInterface
    { _apvi1ConnectionId = p1
    , _apvi1OwnerAccount = p2
    , _apvi1NewPublicVirtualInterfaceAllocation = p3
    }

-- | The connection ID on which the public virtual interface is provisioned.
-- Default: None.
apvi1ConnectionId :: Lens' AllocatePublicVirtualInterface Text
apvi1ConnectionId =
    lens _apvi1ConnectionId (\s a -> s { _apvi1ConnectionId = a })

-- | The AWS account that will own the new public virtual interface. Default:
-- None.
apvi1OwnerAccount :: Lens' AllocatePublicVirtualInterface Text
apvi1OwnerAccount =
    lens _apvi1OwnerAccount (\s a -> s { _apvi1OwnerAccount = a })

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
apvi1NewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
apvi1NewPublicVirtualInterfaceAllocation =
    lens _apvi1NewPublicVirtualInterfaceAllocation
         (\s a -> s { _apvi1NewPublicVirtualInterfaceAllocation = a })

instance ToPath AllocatePublicVirtualInterface

instance ToQuery AllocatePublicVirtualInterface

instance ToHeaders AllocatePublicVirtualInterface

instance ToJSON AllocatePublicVirtualInterface

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data AllocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _apvirsrsOwnerAccount :: Maybe Text
    , _apvirsrsVirtualInterfaceId :: Maybe Text
    , _apvirsrsLocation :: Maybe Text
    , _apvirsrsConnectionId :: Maybe Text
    , _apvirsrsVirtualInterfaceType :: Maybe Text
    , _apvirsrsVirtualInterfaceName :: Maybe Text
    , _apvirsrsVlan :: Maybe Integer
    , _apvirsrsAsn :: Maybe Integer
    , _apvirsrsAuthKey :: Maybe Text
    , _apvirsrsAmazonAddress :: Maybe Text
    , _apvirsrsCustomerAddress :: Maybe Text
    , _apvirsrsVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _apvirsrsCustomerRouterConfig :: Maybe Text
    , _apvirsrsVirtualGatewayId :: Maybe Text
    , _apvirsrsRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

apvirsrsOwnerAccount :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsOwnerAccount =
    lens _apvirsrsOwnerAccount (\s a -> s { _apvirsrsOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
apvirsrsVirtualInterfaceId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsVirtualInterfaceId =
    lens _apvirsrsVirtualInterfaceId
         (\s a -> s { _apvirsrsVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
apvirsrsLocation :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsLocation =
    lens _apvirsrsLocation (\s a -> s { _apvirsrsLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
apvirsrsConnectionId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsConnectionId =
    lens _apvirsrsConnectionId (\s a -> s { _apvirsrsConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
apvirsrsVirtualInterfaceType :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsVirtualInterfaceType =
    lens _apvirsrsVirtualInterfaceType
         (\s a -> s { _apvirsrsVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
apvirsrsVirtualInterfaceName :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsVirtualInterfaceName =
    lens _apvirsrsVirtualInterfaceName
         (\s a -> s { _apvirsrsVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
apvirsrsVlan :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
apvirsrsVlan = lens _apvirsrsVlan (\s a -> s { _apvirsrsVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
apvirsrsAsn :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
apvirsrsAsn = lens _apvirsrsAsn (\s a -> s { _apvirsrsAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
apvirsrsAuthKey :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsAuthKey = lens _apvirsrsAuthKey (\s a -> s { _apvirsrsAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
apvirsrsAmazonAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsAmazonAddress =
    lens _apvirsrsAmazonAddress (\s a -> s { _apvirsrsAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
apvirsrsCustomerAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsCustomerAddress =
    lens _apvirsrsCustomerAddress
         (\s a -> s { _apvirsrsCustomerAddress = a })

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
apvirsrsVirtualInterfaceState :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
apvirsrsVirtualInterfaceState =
    lens _apvirsrsVirtualInterfaceState
         (\s a -> s { _apvirsrsVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
apvirsrsCustomerRouterConfig :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsCustomerRouterConfig =
    lens _apvirsrsCustomerRouterConfig
         (\s a -> s { _apvirsrsCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
apvirsrsVirtualGatewayId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirsrsVirtualGatewayId =
    lens _apvirsrsVirtualGatewayId
         (\s a -> s { _apvirsrsVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
apvirsrsRouteFilterPrefixes :: Lens' AllocatePublicVirtualInterfaceResponse [RouteFilterPrefix]
apvirsrsRouteFilterPrefixes =
    lens _apvirsrsRouteFilterPrefixes
         (\s a -> s { _apvirsrsRouteFilterPrefixes = a })

instance FromJSON AllocatePublicVirtualInterfaceResponse

instance AWSRequest AllocatePublicVirtualInterface where
    type Sv AllocatePublicVirtualInterface = DirectConnect
    type Rs AllocatePublicVirtualInterface = AllocatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

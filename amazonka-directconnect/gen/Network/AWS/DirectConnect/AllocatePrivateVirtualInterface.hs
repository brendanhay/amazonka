{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
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
module Network.AWS.DirectConnect
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
    -- ** Response constructor
    , mkAllocatePrivateVirtualInterfaceResponse
    -- ** Response lenses
    , apvirOwnerAccount
    , apvirVirtualInterfaceId
    , apvirLocation
    , apvirConnectionId
    , apvirVirtualInterfaceType
    , apvirVirtualInterfaceName
    , apvirVlan
    , apvirAsn
    , apvirAuthKey
    , apvirAmazonAddress
    , apvirCustomerAddress
    , apvirVirtualInterfaceState
    , apvirCustomerRouterConfig
    , apvirVirtualGatewayId
    , apvirRouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the AllocatePrivateVirtualInterface
-- operation.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { _apviConnectionId :: !Text
    , _apviOwnerAccount :: !Text
    , _apviNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePrivateVirtualInterface' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Text@
--
-- * @OwnerAccount ::@ @Text@
--
-- * @NewPrivateVirtualInterfaceAllocation ::@ @NewPrivateVirtualInterfaceAllocation@
--
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
    { _apvirOwnerAccount :: !(Maybe Text)
    , _apvirVirtualInterfaceId :: !(Maybe Text)
    , _apvirLocation :: !(Maybe Text)
    , _apvirConnectionId :: !(Maybe Text)
    , _apvirVirtualInterfaceType :: !(Maybe Text)
    , _apvirVirtualInterfaceName :: !(Maybe Text)
    , _apvirVlan :: !(Maybe Integer)
    , _apvirAsn :: !(Maybe Integer)
    , _apvirAuthKey :: !(Maybe Text)
    , _apvirAmazonAddress :: !(Maybe Text)
    , _apvirCustomerAddress :: !(Maybe Text)
    , _apvirVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _apvirCustomerRouterConfig :: !(Maybe Text)
    , _apvirVirtualGatewayId :: !(Maybe Text)
    , _apvirRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePrivateVirtualInterfaceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @VirtualInterfaceId ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @VirtualInterfaceType ::@ @Maybe Text@
--
-- * @VirtualInterfaceName ::@ @Maybe Text@
--
-- * @Vlan ::@ @Maybe Integer@
--
-- * @Asn ::@ @Maybe Integer@
--
-- * @AuthKey ::@ @Maybe Text@
--
-- * @AmazonAddress ::@ @Maybe Text@
--
-- * @CustomerAddress ::@ @Maybe Text@
--
-- * @VirtualInterfaceState ::@ @Maybe VirtualInterfaceState@
--
-- * @CustomerRouterConfig ::@ @Maybe Text@
--
-- * @VirtualGatewayId ::@ @Maybe Text@
--
-- * @RouteFilterPrefixes ::@ @[RouteFilterPrefix]@
--
mkAllocatePrivateVirtualInterfaceResponse :: AllocatePrivateVirtualInterfaceResponse
mkAllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { _apvirOwnerAccount = Nothing
    , _apvirVirtualInterfaceId = Nothing
    , _apvirLocation = Nothing
    , _apvirConnectionId = Nothing
    , _apvirVirtualInterfaceType = Nothing
    , _apvirVirtualInterfaceName = Nothing
    , _apvirVlan = Nothing
    , _apvirAsn = Nothing
    , _apvirAuthKey = Nothing
    , _apvirAmazonAddress = Nothing
    , _apvirCustomerAddress = Nothing
    , _apvirVirtualInterfaceState = Nothing
    , _apvirCustomerRouterConfig = Nothing
    , _apvirVirtualGatewayId = Nothing
    , _apvirRouteFilterPrefixes = mempty
    }

apvirOwnerAccount :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirOwnerAccount =
    lens _apvirOwnerAccount (\s a -> s { _apvirOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
apvirVirtualInterfaceId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceId =
    lens _apvirVirtualInterfaceId
         (\s a -> s { _apvirVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
apvirLocation :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirLocation = lens _apvirLocation (\s a -> s { _apvirLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
apvirConnectionId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirConnectionId =
    lens _apvirConnectionId (\s a -> s { _apvirConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
apvirVirtualInterfaceType :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceType =
    lens _apvirVirtualInterfaceType
         (\s a -> s { _apvirVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
apvirVirtualInterfaceName :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualInterfaceName =
    lens _apvirVirtualInterfaceName
         (\s a -> s { _apvirVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
apvirVlan :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
apvirVlan = lens _apvirVlan (\s a -> s { _apvirVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
apvirAsn :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Integer)
apvirAsn = lens _apvirAsn (\s a -> s { _apvirAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
apvirAuthKey :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirAuthKey = lens _apvirAuthKey (\s a -> s { _apvirAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
apvirAmazonAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirAmazonAddress =
    lens _apvirAmazonAddress (\s a -> s { _apvirAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
apvirCustomerAddress :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirCustomerAddress =
    lens _apvirCustomerAddress (\s a -> s { _apvirCustomerAddress = a })

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
apvirVirtualInterfaceState :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
apvirVirtualInterfaceState =
    lens _apvirVirtualInterfaceState
         (\s a -> s { _apvirVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
apvirCustomerRouterConfig :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirCustomerRouterConfig =
    lens _apvirCustomerRouterConfig
         (\s a -> s { _apvirCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
apvirVirtualGatewayId :: Lens' AllocatePrivateVirtualInterfaceResponse (Maybe Text)
apvirVirtualGatewayId =
    lens _apvirVirtualGatewayId (\s a -> s { _apvirVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
apvirRouteFilterPrefixes :: Lens' AllocatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
apvirRouteFilterPrefixes =
    lens _apvirRouteFilterPrefixes
         (\s a -> s { _apvirRouteFilterPrefixes = a })

instance FromJSON AllocatePrivateVirtualInterfaceResponse

instance AWSRequest AllocatePrivateVirtualInterface where
    type Sv AllocatePrivateVirtualInterface = DirectConnect
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

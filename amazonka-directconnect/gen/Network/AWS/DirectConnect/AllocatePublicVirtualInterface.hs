{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
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
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
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
    -- ** Response constructor
    , mkAllocatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , apvirrOwnerAccount
    , apvirrVirtualInterfaceId
    , apvirrLocation
    , apvirrConnectionId
    , apvirrVirtualInterfaceType
    , apvirrVirtualInterfaceName
    , apvirrVlan
    , apvirrAsn
    , apvirrAuthKey
    , apvirrAmazonAddress
    , apvirrCustomerAddress
    , apvirrVirtualInterfaceState
    , apvirrCustomerRouterConfig
    , apvirrVirtualGatewayId
    , apvirrRouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the AllocatePublicVirtualInterface
-- operation.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface
    { _apvi1ConnectionId :: !Text
    , _apvi1OwnerAccount :: !Text
    , _apvi1NewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePublicVirtualInterface' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Text@
--
-- * @OwnerAccount ::@ @Text@
--
-- * @NewPublicVirtualInterfaceAllocation ::@ @NewPublicVirtualInterfaceAllocation@
--
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
    { _apvirrOwnerAccount :: !(Maybe Text)
    , _apvirrVirtualInterfaceId :: !(Maybe Text)
    , _apvirrLocation :: !(Maybe Text)
    , _apvirrConnectionId :: !(Maybe Text)
    , _apvirrVirtualInterfaceType :: !(Maybe Text)
    , _apvirrVirtualInterfaceName :: !(Maybe Text)
    , _apvirrVlan :: !(Maybe Integer)
    , _apvirrAsn :: !(Maybe Integer)
    , _apvirrAuthKey :: !(Maybe Text)
    , _apvirrAmazonAddress :: !(Maybe Text)
    , _apvirrCustomerAddress :: !(Maybe Text)
    , _apvirrVirtualInterfaceState :: Maybe VirtualInterfaceState
    , _apvirrCustomerRouterConfig :: !(Maybe Text)
    , _apvirrVirtualGatewayId :: !(Maybe Text)
    , _apvirrRouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocatePublicVirtualInterfaceResponse' response.
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
mkAllocatePublicVirtualInterfaceResponse :: AllocatePublicVirtualInterfaceResponse
mkAllocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _apvirrOwnerAccount = Nothing
    , _apvirrVirtualInterfaceId = Nothing
    , _apvirrLocation = Nothing
    , _apvirrConnectionId = Nothing
    , _apvirrVirtualInterfaceType = Nothing
    , _apvirrVirtualInterfaceName = Nothing
    , _apvirrVlan = Nothing
    , _apvirrAsn = Nothing
    , _apvirrAuthKey = Nothing
    , _apvirrAmazonAddress = Nothing
    , _apvirrCustomerAddress = Nothing
    , _apvirrVirtualInterfaceState = Nothing
    , _apvirrCustomerRouterConfig = Nothing
    , _apvirrVirtualGatewayId = Nothing
    , _apvirrRouteFilterPrefixes = mempty
    }

apvirrOwnerAccount :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrOwnerAccount =
    lens _apvirrOwnerAccount (\s a -> s { _apvirrOwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
apvirrVirtualInterfaceId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrVirtualInterfaceId =
    lens _apvirrVirtualInterfaceId
         (\s a -> s { _apvirrVirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
apvirrLocation :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrLocation = lens _apvirrLocation (\s a -> s { _apvirrLocation = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
apvirrConnectionId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrConnectionId =
    lens _apvirrConnectionId (\s a -> s { _apvirrConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
apvirrVirtualInterfaceType :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrVirtualInterfaceType =
    lens _apvirrVirtualInterfaceType
         (\s a -> s { _apvirrVirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
apvirrVirtualInterfaceName :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrVirtualInterfaceName =
    lens _apvirrVirtualInterfaceName
         (\s a -> s { _apvirrVirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
apvirrVlan :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
apvirrVlan = lens _apvirrVlan (\s a -> s { _apvirrVlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
apvirrAsn :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Integer)
apvirrAsn = lens _apvirrAsn (\s a -> s { _apvirrAsn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
apvirrAuthKey :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrAuthKey = lens _apvirrAuthKey (\s a -> s { _apvirrAuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
apvirrAmazonAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrAmazonAddress =
    lens _apvirrAmazonAddress (\s a -> s { _apvirrAmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
apvirrCustomerAddress :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrCustomerAddress =
    lens _apvirrCustomerAddress (\s a -> s { _apvirrCustomerAddress = a })

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
apvirrVirtualInterfaceState :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
apvirrVirtualInterfaceState =
    lens _apvirrVirtualInterfaceState
         (\s a -> s { _apvirrVirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
apvirrCustomerRouterConfig :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrCustomerRouterConfig =
    lens _apvirrCustomerRouterConfig
         (\s a -> s { _apvirrCustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
apvirrVirtualGatewayId :: Lens' AllocatePublicVirtualInterfaceResponse (Maybe Text)
apvirrVirtualGatewayId =
    lens _apvirrVirtualGatewayId (\s a -> s { _apvirrVirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
apvirrRouteFilterPrefixes :: Lens' AllocatePublicVirtualInterfaceResponse [RouteFilterPrefix]
apvirrRouteFilterPrefixes =
    lens _apvirrRouteFilterPrefixes
         (\s a -> s { _apvirrRouteFilterPrefixes = a })

instance FromJSON AllocatePublicVirtualInterfaceResponse

instance AWSRequest AllocatePublicVirtualInterface where
    type Sv AllocatePublicVirtualInterface = DirectConnect
    type Rs AllocatePublicVirtualInterface = AllocatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

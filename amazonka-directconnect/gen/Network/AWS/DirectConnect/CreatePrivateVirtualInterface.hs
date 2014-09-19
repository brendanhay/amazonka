{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
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
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    (
    -- * Request
      CreatePrivateVirtualInterface
    -- ** Request constructor
    , createPrivateVirtualInterface
    -- ** Request lenses
    , cpvi2ConnectionId
    , cpvi2NewPrivateVirtualInterface

    -- * Response
    , CreatePrivateVirtualInterfaceResponse
    -- ** Response constructor
    , createPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvir1OwnerAccount
    , cpvir1VirtualInterfaceId
    , cpvir1Location
    , cpvir1ConnectionId
    , cpvir1VirtualInterfaceType
    , cpvir1VirtualInterfaceName
    , cpvir1Vlan
    , cpvir1Asn
    , cpvir1AuthKey
    , cpvir1AmazonAddress
    , cpvir1CustomerAddress
    , cpvir1VirtualInterfaceState
    , cpvir1CustomerRouterConfig
    , cpvir1VirtualGatewayId
    , cpvir1RouteFilterPrefixes
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the CreatePrivateVirtualInterface
-- operation.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { _cpvi2ConnectionId :: Text
    , _cpvi2NewPrivateVirtualInterface :: NewPrivateVirtualInterface
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePrivateVirtualInterface' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Text@
--
-- * @NewPrivateVirtualInterface ::@ @NewPrivateVirtualInterface@
--
createPrivateVirtualInterface :: Text -- ^ 'cpvi2ConnectionId'
                              -> NewPrivateVirtualInterface -- ^ 'cpvi2NewPrivateVirtualInterface'
                              -> CreatePrivateVirtualInterface
createPrivateVirtualInterface p1 p2 = CreatePrivateVirtualInterface
    { _cpvi2ConnectionId = p1
    , _cpvi2NewPrivateVirtualInterface = p2
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvi2ConnectionId :: Lens' CreatePrivateVirtualInterface Text
cpvi2ConnectionId =
    lens _cpvi2ConnectionId (\s a -> s { _cpvi2ConnectionId = a })

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
cpvi2NewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
cpvi2NewPrivateVirtualInterface =
    lens _cpvi2NewPrivateVirtualInterface
         (\s a -> s { _cpvi2NewPrivateVirtualInterface = a })

instance ToPath CreatePrivateVirtualInterface

instance ToQuery CreatePrivateVirtualInterface

instance ToHeaders CreatePrivateVirtualInterface

instance ToJSON CreatePrivateVirtualInterface

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _cpvir1OwnerAccount :: Maybe Text
    , _cpvir1VirtualInterfaceId :: Maybe Text
    , _cpvir1Location :: Maybe Text
    , _cpvir1ConnectionId :: Maybe Text
    , _cpvir1VirtualInterfaceType :: Maybe Text
    , _cpvir1VirtualInterfaceName :: Maybe Text
    , _cpvir1Vlan :: Maybe Integer
    , _cpvir1Asn :: Maybe Integer
    , _cpvir1AuthKey :: Maybe Text
    , _cpvir1AmazonAddress :: Maybe Text
    , _cpvir1CustomerAddress :: Maybe Text
    , _cpvir1VirtualInterfaceState :: Maybe VirtualInterfaceState
    , _cpvir1CustomerRouterConfig :: Maybe Text
    , _cpvir1VirtualGatewayId :: Maybe Text
    , _cpvir1RouteFilterPrefixes :: [RouteFilterPrefix]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePrivateVirtualInterfaceResponse' response.
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
createPrivateVirtualInterfaceResponse :: CreatePrivateVirtualInterfaceResponse
createPrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { _cpvir1OwnerAccount = Nothing
    , _cpvir1VirtualInterfaceId = Nothing
    , _cpvir1Location = Nothing
    , _cpvir1ConnectionId = Nothing
    , _cpvir1VirtualInterfaceType = Nothing
    , _cpvir1VirtualInterfaceName = Nothing
    , _cpvir1Vlan = Nothing
    , _cpvir1Asn = Nothing
    , _cpvir1AuthKey = Nothing
    , _cpvir1AmazonAddress = Nothing
    , _cpvir1CustomerAddress = Nothing
    , _cpvir1VirtualInterfaceState = Nothing
    , _cpvir1CustomerRouterConfig = Nothing
    , _cpvir1VirtualGatewayId = Nothing
    , _cpvir1RouteFilterPrefixes = mempty
    }

cpvir1OwnerAccount :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1OwnerAccount =
    lens _cpvir1OwnerAccount (\s a -> s { _cpvir1OwnerAccount = a })

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpvir1VirtualInterfaceId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1VirtualInterfaceId =
    lens _cpvir1VirtualInterfaceId
         (\s a -> s { _cpvir1VirtualInterfaceId = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cpvir1Location :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1Location = lens _cpvir1Location (\s a -> s { _cpvir1Location = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cpvir1ConnectionId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1ConnectionId =
    lens _cpvir1ConnectionId (\s a -> s { _cpvir1ConnectionId = a })

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
cpvir1VirtualInterfaceType :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1VirtualInterfaceType =
    lens _cpvir1VirtualInterfaceType
         (\s a -> s { _cpvir1VirtualInterfaceType = a })

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
cpvir1VirtualInterfaceName :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1VirtualInterfaceName =
    lens _cpvir1VirtualInterfaceName
         (\s a -> s { _cpvir1VirtualInterfaceName = a })

-- | The VLAN ID. Example: 101.
cpvir1Vlan :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
cpvir1Vlan = lens _cpvir1Vlan (\s a -> s { _cpvir1Vlan = a })

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
cpvir1Asn :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Integer)
cpvir1Asn = lens _cpvir1Asn (\s a -> s { _cpvir1Asn = a })

-- | Authentication key for BGP configuration. Example: asdf34example.
cpvir1AuthKey :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1AuthKey = lens _cpvir1AuthKey (\s a -> s { _cpvir1AuthKey = a })

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
cpvir1AmazonAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1AmazonAddress =
    lens _cpvir1AmazonAddress (\s a -> s { _cpvir1AmazonAddress = a })

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
cpvir1CustomerAddress :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1CustomerAddress =
    lens _cpvir1CustomerAddress (\s a -> s { _cpvir1CustomerAddress = a })

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
cpvir1VirtualInterfaceState :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvir1VirtualInterfaceState =
    lens _cpvir1VirtualInterfaceState
         (\s a -> s { _cpvir1VirtualInterfaceState = a })

-- | Information for generating the customer router configuration.
cpvir1CustomerRouterConfig :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1CustomerRouterConfig =
    lens _cpvir1CustomerRouterConfig
         (\s a -> s { _cpvir1CustomerRouterConfig = a })

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
cpvir1VirtualGatewayId :: Lens' CreatePrivateVirtualInterfaceResponse (Maybe Text)
cpvir1VirtualGatewayId =
    lens _cpvir1VirtualGatewayId (\s a -> s { _cpvir1VirtualGatewayId = a })

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
cpvir1RouteFilterPrefixes :: Lens' CreatePrivateVirtualInterfaceResponse [RouteFilterPrefix]
cpvir1RouteFilterPrefixes =
    lens _cpvir1RouteFilterPrefixes
         (\s a -> s { _cpvir1RouteFilterPrefixes = a })

instance FromJSON CreatePrivateVirtualInterfaceResponse

instance AWSRequest CreatePrivateVirtualInterface where
    type Sv CreatePrivateVirtualInterface = DirectConnect
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse

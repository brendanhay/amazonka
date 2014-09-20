{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new connection between the customer network and a specific AWS
-- Direct Connect location. A connection links your internal network to an AWS
-- Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet
-- fiber-optic cable. One end of the cable is connected to your router, the
-- other to an AWS Direct Connect router. An AWS Direct Connect location
-- provides access to Amazon Web Services in the region it is associated with.
-- You can establish connections with AWS Direct Connect locations in multiple
-- regions, but a connection in one region does not provide connectivity to
-- other regions.
module Network.AWS.DirectConnect.CreateConnection
    (
    -- * Request
      CreateConnection
    -- ** Request constructor
    , createConnection
    -- ** Request lenses
    , cc1Location
    , cc1Bandwidth
    , cc1ConnectionName

    -- * Response
    , CreateConnectionResponse
    -- ** Response constructor
    , createConnectionResponse
    -- ** Response lenses
    , ccrrOwnerAccount
    , ccrrConnectionId
    , ccrrConnectionName
    , ccrrConnectionState
    , ccrrRegion
    , ccrrLocation
    , ccrrBandwidth
    , ccrrVlan
    , ccrrPartnerName
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the CreateConnection operation.
data CreateConnection = CreateConnection
    { _cc1Location :: Text
    , _cc1Bandwidth :: Text
    , _cc1ConnectionName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Location ::@ @Text@
--
-- * @Bandwidth ::@ @Text@
--
-- * @ConnectionName ::@ @Text@
--
createConnection :: Text -- ^ 'cc1Location'
                 -> Text -- ^ 'cc1Bandwidth'
                 -> Text -- ^ 'cc1ConnectionName'
                 -> CreateConnection
createConnection p1 p2 p3 = CreateConnection
    { _cc1Location = p1
    , _cc1Bandwidth = p2
    , _cc1ConnectionName = p3
    }

-- | Where the connection is located. Example: EqSV5 Default: None.
cc1Location :: Lens' CreateConnection Text
cc1Location = lens _cc1Location (\s a -> s { _cc1Location = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cc1Bandwidth :: Lens' CreateConnection Text
cc1Bandwidth = lens _cc1Bandwidth (\s a -> s { _cc1Bandwidth = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cc1ConnectionName :: Lens' CreateConnection Text
cc1ConnectionName =
    lens _cc1ConnectionName (\s a -> s { _cc1ConnectionName = a })

instance ToPath CreateConnection

instance ToQuery CreateConnection

instance ToHeaders CreateConnection

instance ToJSON CreateConnection

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data CreateConnectionResponse = CreateConnectionResponse
    { _ccrrOwnerAccount :: Maybe Text
    , _ccrrConnectionId :: Maybe Text
    , _ccrrConnectionName :: Maybe Text
    , _ccrrConnectionState :: Maybe ConnectionState
    , _ccrrRegion :: Maybe Text
    , _ccrrLocation :: Maybe Text
    , _ccrrBandwidth :: Maybe Text
    , _ccrrVlan :: Maybe Integer
    , _ccrrPartnerName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @ConnectionName ::@ @Maybe Text@
--
-- * @ConnectionState ::@ @Maybe ConnectionState@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bandwidth ::@ @Maybe Text@
--
-- * @Vlan ::@ @Maybe Integer@
--
-- * @PartnerName ::@ @Maybe Text@
--
createConnectionResponse :: CreateConnectionResponse
createConnectionResponse = CreateConnectionResponse
    { _ccrrOwnerAccount = Nothing
    , _ccrrConnectionId = Nothing
    , _ccrrConnectionName = Nothing
    , _ccrrConnectionState = Nothing
    , _ccrrRegion = Nothing
    , _ccrrLocation = Nothing
    , _ccrrBandwidth = Nothing
    , _ccrrVlan = Nothing
    , _ccrrPartnerName = Nothing
    }

ccrrOwnerAccount :: Lens' CreateConnectionResponse (Maybe Text)
ccrrOwnerAccount =
    lens _ccrrOwnerAccount (\s a -> s { _ccrrOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
ccrrConnectionId :: Lens' CreateConnectionResponse (Maybe Text)
ccrrConnectionId =
    lens _ccrrConnectionId (\s a -> s { _ccrrConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
ccrrConnectionName :: Lens' CreateConnectionResponse (Maybe Text)
ccrrConnectionName =
    lens _ccrrConnectionName (\s a -> s { _ccrrConnectionName = a })

-- | State of the connection. Ordering: The initial state of a hosted connection
-- provisioned on an interconnect. The connection stays in the ordering state
-- until the owner of the hosted connection confirms or declines the
-- connection order. Requested: The initial state of a standard connection.
-- The connection stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The connection has
-- been approved, and is being initialized. Available: The network link is up,
-- and the connection is ready for use. Down: The network link is down.
-- Deleted: The connection has been deleted. Rejected: A hosted connection in
-- the 'Ordering' state will enter the 'Rejected' state if it is deleted by
-- the end customer.
ccrrConnectionState :: Lens' CreateConnectionResponse (Maybe ConnectionState)
ccrrConnectionState =
    lens _ccrrConnectionState (\s a -> s { _ccrrConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
ccrrRegion :: Lens' CreateConnectionResponse (Maybe Text)
ccrrRegion = lens _ccrrRegion (\s a -> s { _ccrrRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
ccrrLocation :: Lens' CreateConnectionResponse (Maybe Text)
ccrrLocation = lens _ccrrLocation (\s a -> s { _ccrrLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
ccrrBandwidth :: Lens' CreateConnectionResponse (Maybe Text)
ccrrBandwidth = lens _ccrrBandwidth (\s a -> s { _ccrrBandwidth = a })

-- | The VLAN ID. Example: 101.
ccrrVlan :: Lens' CreateConnectionResponse (Maybe Integer)
ccrrVlan = lens _ccrrVlan (\s a -> s { _ccrrVlan = a })

ccrrPartnerName :: Lens' CreateConnectionResponse (Maybe Text)
ccrrPartnerName = lens _ccrrPartnerName (\s a -> s { _ccrrPartnerName = a })

instance FromJSON CreateConnectionResponse

instance AWSRequest CreateConnection where
    type Sv CreateConnection = DirectConnect
    type Rs CreateConnection = CreateConnectionResponse

    request = get
    response _ = jsonResponse

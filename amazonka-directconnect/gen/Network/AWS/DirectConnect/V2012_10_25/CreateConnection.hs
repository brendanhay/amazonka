{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreateConnection
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
module Network.AWS.DirectConnect.V2012_10_25.CreateConnection
    (
    -- * Request
      CreateConnection
    -- ** Request constructor
    , mkCreateConnection
    -- ** Request lenses
    , cc1Location
    , cc1Bandwidth
    , cc1ConnectionName

    -- * Response
    , CreateConnectionResponse
    -- ** Response lenses
    , ccrsrsOwnerAccount
    , ccrsrsConnectionId
    , ccrsrsConnectionName
    , ccrsrsConnectionState
    , ccrsrsRegion
    , ccrsrsLocation
    , ccrsrsBandwidth
    , ccrsrsVlan
    , ccrsrsPartnerName
    ) where

import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the CreateConnection operation.
data CreateConnection = CreateConnection
    { _cc1Location :: Text
    , _cc1Bandwidth :: Text
    , _cc1ConnectionName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateConnection' request.
mkCreateConnection :: Text -- ^ 'cc1Location'
                   -> Text -- ^ 'cc1Bandwidth'
                   -> Text -- ^ 'cc1ConnectionName'
                   -> CreateConnection
mkCreateConnection p1 p2 p3 = CreateConnection
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
    { _ccrsrsOwnerAccount :: Maybe Text
    , _ccrsrsConnectionId :: Maybe Text
    , _ccrsrsConnectionName :: Maybe Text
    , _ccrsrsConnectionState :: Maybe ConnectionState
    , _ccrsrsRegion :: Maybe Text
    , _ccrsrsLocation :: Maybe Text
    , _ccrsrsBandwidth :: Maybe Text
    , _ccrsrsVlan :: Maybe Integer
    , _ccrsrsPartnerName :: Maybe Text
    } deriving (Show, Generic)

ccrsrsOwnerAccount :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsOwnerAccount =
    lens _ccrsrsOwnerAccount (\s a -> s { _ccrsrsOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
ccrsrsConnectionId :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsConnectionId =
    lens _ccrsrsConnectionId (\s a -> s { _ccrsrsConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
ccrsrsConnectionName :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsConnectionName =
    lens _ccrsrsConnectionName (\s a -> s { _ccrsrsConnectionName = a })

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
ccrsrsConnectionState :: Lens' CreateConnectionResponse (Maybe ConnectionState)
ccrsrsConnectionState =
    lens _ccrsrsConnectionState (\s a -> s { _ccrsrsConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
ccrsrsRegion :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsRegion = lens _ccrsrsRegion (\s a -> s { _ccrsrsRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
ccrsrsLocation :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsLocation = lens _ccrsrsLocation (\s a -> s { _ccrsrsLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
ccrsrsBandwidth :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsBandwidth = lens _ccrsrsBandwidth (\s a -> s { _ccrsrsBandwidth = a })

-- | The VLAN ID. Example: 101.
ccrsrsVlan :: Lens' CreateConnectionResponse (Maybe Integer)
ccrsrsVlan = lens _ccrsrsVlan (\s a -> s { _ccrsrsVlan = a })

ccrsrsPartnerName :: Lens' CreateConnectionResponse (Maybe Text)
ccrsrsPartnerName =
    lens _ccrsrsPartnerName (\s a -> s { _ccrsrsPartnerName = a })

instance FromJSON CreateConnectionResponse

instance AWSRequest CreateConnection where
    type Sv CreateConnection = DirectConnect
    type Rs CreateConnection = CreateConnectionResponse

    request = get
    response _ = jsonResponse

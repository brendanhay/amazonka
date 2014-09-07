{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.AllocateConnectionOnInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a hosted connection on an interconnect. Allocates a VLAN number and
-- a specified amount of bandwidth for use by a hosted connection on the given
-- interconnect.
module Network.AWS.DirectConnect.V2012_10_25.AllocateConnectionOnInterconnect
    (
    -- * Request
      AllocateConnectionOnInterconnect
    -- ** Request constructor
    , mkAllocateConnectionOnInterconnect
    -- ** Request lenses
    , acoiBandwidth
    , acoiConnectionName
    , acoiOwnerAccount
    , acoiInterconnectId
    , acoiVlan

    -- * Response
    , AllocateConnectionOnInterconnectResponse
    -- ** Response lenses
    , acoirsOwnerAccount
    , acoirsConnectionId
    , acoirsConnectionName
    , acoirsConnectionState
    , acoirsRegion
    , acoirsLocation
    , acoirsBandwidth
    , acoirsVlan
    , acoirsPartnerName
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Container for the parameters to the AllocateConnectionOnInterconnect
-- operation.
data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { _acoiBandwidth :: Text
    , _acoiConnectionName :: Text
    , _acoiOwnerAccount :: Text
    , _acoiInterconnectId :: Text
    , _acoiVlan :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateConnectionOnInterconnect' request.
mkAllocateConnectionOnInterconnect :: Text -- ^ 'acoiBandwidth'
                                   -> Text -- ^ 'acoiConnectionName'
                                   -> Text -- ^ 'acoiOwnerAccount'
                                   -> Text -- ^ 'acoiInterconnectId'
                                   -> Integer -- ^ 'acoiVlan'
                                   -> AllocateConnectionOnInterconnect
mkAllocateConnectionOnInterconnect p1 p2 p3 p4 p5 = AllocateConnectionOnInterconnect
    { _acoiBandwidth = p1
    , _acoiConnectionName = p2
    , _acoiOwnerAccount = p3
    , _acoiInterconnectId = p4
    , _acoiVlan = p5
    }

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoiBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoiBandwidth = lens _acoiBandwidth (\s a -> s { _acoiBandwidth = a })

-- | Name of the provisioned connection. Example: "500M Connection to AWS"
-- Default: None.
acoiConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoiConnectionName =
    lens _acoiConnectionName (\s a -> s { _acoiConnectionName = a })

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned. Example: 123443215678 Default: None.
acoiOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoiOwnerAccount =
    lens _acoiOwnerAccount (\s a -> s { _acoiOwnerAccount = a })

-- | ID of the interconnect on which the connection will be provisioned.
-- Example: dxcon-456abc78 Default: None.
acoiInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoiInterconnectId =
    lens _acoiInterconnectId (\s a -> s { _acoiInterconnectId = a })

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default:
-- None.
acoiVlan :: Lens' AllocateConnectionOnInterconnect Integer
acoiVlan = lens _acoiVlan (\s a -> s { _acoiVlan = a })

instance ToPath AllocateConnectionOnInterconnect

instance ToQuery AllocateConnectionOnInterconnect

instance ToHeaders AllocateConnectionOnInterconnect

instance ToJSON AllocateConnectionOnInterconnect

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _acoirsOwnerAccount :: Maybe Text
    , _acoirsConnectionId :: Maybe Text
    , _acoirsConnectionName :: Maybe Text
    , _acoirsConnectionState :: Maybe ConnectionState
    , _acoirsRegion :: Maybe Text
    , _acoirsLocation :: Maybe Text
    , _acoirsBandwidth :: Maybe Text
    , _acoirsVlan :: Maybe Integer
    , _acoirsPartnerName :: Maybe Text
    } deriving (Show, Generic)

acoirsOwnerAccount :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsOwnerAccount =
    lens _acoirsOwnerAccount (\s a -> s { _acoirsOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
acoirsConnectionId :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsConnectionId =
    lens _acoirsConnectionId (\s a -> s { _acoirsConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
acoirsConnectionName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsConnectionName =
    lens _acoirsConnectionName (\s a -> s { _acoirsConnectionName = a })

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
acoirsConnectionState :: Lens' AllocateConnectionOnInterconnectResponse (Maybe ConnectionState)
acoirsConnectionState =
    lens _acoirsConnectionState (\s a -> s { _acoirsConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
acoirsRegion :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsRegion = lens _acoirsRegion (\s a -> s { _acoirsRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
acoirsLocation :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsLocation = lens _acoirsLocation (\s a -> s { _acoirsLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoirsBandwidth :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsBandwidth = lens _acoirsBandwidth (\s a -> s { _acoirsBandwidth = a })

-- | The VLAN ID. Example: 101.
acoirsVlan :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Integer)
acoirsVlan = lens _acoirsVlan (\s a -> s { _acoirsVlan = a })

acoirsPartnerName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirsPartnerName =
    lens _acoirsPartnerName (\s a -> s { _acoirsPartnerName = a })

instance FromJSON AllocateConnectionOnInterconnectResponse

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request = get
    response _ = jsonResponse

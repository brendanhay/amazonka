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
    , createConnection
    -- ** Request lenses
    , cctBandwidth
    , cctConnectionName
    , cctLocation

    -- * Response
    , CreateConnectionResponse
    -- ** Response lenses
    , fBandwidth
    , fConnectionId
    , fConnectionName
    , fConnectionState
    , fLocation
    , fOwnerAccount
    , fPartnerName
    , fRegion
    , fVlan
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateConnection' request.
createConnection :: Text -- ^ 'cctBandwidth'
                 -> Text -- ^ 'cctConnectionName'
                 -> Text -- ^ 'cctLocation'
                 -> CreateConnection
createConnection p1 p2 p3 = CreateConnection
    { _cctBandwidth = p1
    , _cctConnectionName = p2
    , _cctLocation = p3
    }
{-# INLINE createConnection #-}

data CreateConnection = CreateConnection
    { _cctBandwidth :: Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _cctConnectionName :: Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _cctLocation :: Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cctBandwidth :: Lens' CreateConnection (Text)
cctBandwidth f x =
    f (_cctBandwidth x)
        <&> \y -> x { _cctBandwidth = y }
{-# INLINE cctBandwidth #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cctConnectionName :: Lens' CreateConnection (Text)
cctConnectionName f x =
    f (_cctConnectionName x)
        <&> \y -> x { _cctConnectionName = y }
{-# INLINE cctConnectionName #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
cctLocation :: Lens' CreateConnection (Text)
cctLocation f x =
    f (_cctLocation x)
        <&> \y -> x { _cctLocation = y }
{-# INLINE cctLocation #-}

instance ToPath CreateConnection

instance ToQuery CreateConnection

instance ToHeaders CreateConnection

instance ToJSON CreateConnection

data CreateConnectionResponse = CreateConnectionResponse
    { _fBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _fConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _fConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _fConnectionState :: Maybe ConnectionState
      -- ^ State of the connection. Ordering: The initial state of a hosted
      -- connection provisioned on an interconnect. The connection stays
      -- in the ordering state until the owner of the hosted connection
      -- confirms or declines the connection order. Requested: The initial
      -- state of a standard connection. The connection stays in the
      -- requested state until the Letter of Authorization (LOA) is sent
      -- to the customer. Pending: The connection has been approved, and
      -- is being initialized. Available: The network link is up, and the
      -- connection is ready for use. Down: The network link is down.
      -- Deleted: The connection has been deleted. Rejected: A hosted
      -- connection in the 'Ordering' state will enter the 'Rejected'
      -- state if it is deleted by the end customer.
    , _fLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _fOwnerAccount :: Maybe Text
    , _fPartnerName :: Maybe Text
    , _fRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _fVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
fBandwidth :: Lens' CreateConnectionResponse (Maybe Text)
fBandwidth f x =
    f (_fBandwidth x)
        <&> \y -> x { _fBandwidth = y }
{-# INLINE fBandwidth #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
fConnectionId :: Lens' CreateConnectionResponse (Maybe Text)
fConnectionId f x =
    f (_fConnectionId x)
        <&> \y -> x { _fConnectionId = y }
{-# INLINE fConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
fConnectionName :: Lens' CreateConnectionResponse (Maybe Text)
fConnectionName f x =
    f (_fConnectionName x)
        <&> \y -> x { _fConnectionName = y }
{-# INLINE fConnectionName #-}

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
fConnectionState :: Lens' CreateConnectionResponse (Maybe ConnectionState)
fConnectionState f x =
    f (_fConnectionState x)
        <&> \y -> x { _fConnectionState = y }
{-# INLINE fConnectionState #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
fLocation :: Lens' CreateConnectionResponse (Maybe Text)
fLocation f x =
    f (_fLocation x)
        <&> \y -> x { _fLocation = y }
{-# INLINE fLocation #-}

fOwnerAccount :: Lens' CreateConnectionResponse (Maybe Text)
fOwnerAccount f x =
    f (_fOwnerAccount x)
        <&> \y -> x { _fOwnerAccount = y }
{-# INLINE fOwnerAccount #-}

fPartnerName :: Lens' CreateConnectionResponse (Maybe Text)
fPartnerName f x =
    f (_fPartnerName x)
        <&> \y -> x { _fPartnerName = y }
{-# INLINE fPartnerName #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
fRegion :: Lens' CreateConnectionResponse (Maybe Text)
fRegion f x =
    f (_fRegion x)
        <&> \y -> x { _fRegion = y }
{-# INLINE fRegion #-}

-- | The VLAN ID. Example: 101.
fVlan :: Lens' CreateConnectionResponse (Maybe Integer)
fVlan f x =
    f (_fVlan x)
        <&> \y -> x { _fVlan = y }
{-# INLINE fVlan #-}

instance FromJSON CreateConnectionResponse

instance AWSRequest CreateConnection where
    type Sv CreateConnection = DirectConnect
    type Rs CreateConnection = CreateConnectionResponse

    request = get
    response _ = jsonResponse

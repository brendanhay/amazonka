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
    , mkCreateConnectionRequest
    -- ** Request lenses
    , cctLocation
    , cctBandwidth
    , cctConnectionName

    -- * Response
    , CreateConnectionResponse
    -- ** Response lenses
    , fOwnerAccount
    , fConnectionId
    , fConnectionName
    , fConnectionState
    , fRegion
    , fLocation
    , fBandwidth
    , fVlan
    , fPartnerName
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateConnection' request.
mkCreateConnectionRequest :: Text -- ^ 'cctLocation'
                          -> Text -- ^ 'cctBandwidth'
                          -> Text -- ^ 'cctConnectionName'
                          -> CreateConnection
mkCreateConnectionRequest p1 p2 p3 = CreateConnection
    { _cctLocation = p1
    , _cctBandwidth = p2
    , _cctConnectionName = p3
    }
{-# INLINE mkCreateConnectionRequest #-}

data CreateConnection = CreateConnection
    { _cctLocation :: Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _cctBandwidth :: Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _cctConnectionName :: Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    } deriving (Show, Generic)

-- | Where the connection is located. Example: EqSV5 Default: None.
cctLocation :: Lens' CreateConnection (Text)
cctLocation = lens _cctLocation (\s a -> s { _cctLocation = a })
{-# INLINE cctLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cctBandwidth :: Lens' CreateConnection (Text)
cctBandwidth = lens _cctBandwidth (\s a -> s { _cctBandwidth = a })
{-# INLINE cctBandwidth #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cctConnectionName :: Lens' CreateConnection (Text)
cctConnectionName = lens _cctConnectionName (\s a -> s { _cctConnectionName = a })
{-# INLINE cctConnectionName #-}

instance ToPath CreateConnection

instance ToQuery CreateConnection

instance ToHeaders CreateConnection

instance ToJSON CreateConnection

data CreateConnectionResponse = CreateConnectionResponse
    { _fOwnerAccount :: Maybe Text
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
    , _fRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _fLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _fBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _fVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _fPartnerName :: Maybe Text
    } deriving (Show, Generic)

fOwnerAccount :: Lens' CreateConnectionResponse (Maybe Text)
fOwnerAccount = lens _fOwnerAccount (\s a -> s { _fOwnerAccount = a })
{-# INLINE fOwnerAccount #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
fConnectionId :: Lens' CreateConnectionResponse (Maybe Text)
fConnectionId = lens _fConnectionId (\s a -> s { _fConnectionId = a })
{-# INLINE fConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
fConnectionName :: Lens' CreateConnectionResponse (Maybe Text)
fConnectionName = lens _fConnectionName (\s a -> s { _fConnectionName = a })
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
fConnectionState = lens _fConnectionState (\s a -> s { _fConnectionState = a })
{-# INLINE fConnectionState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
fRegion :: Lens' CreateConnectionResponse (Maybe Text)
fRegion = lens _fRegion (\s a -> s { _fRegion = a })
{-# INLINE fRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
fLocation :: Lens' CreateConnectionResponse (Maybe Text)
fLocation = lens _fLocation (\s a -> s { _fLocation = a })
{-# INLINE fLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
fBandwidth :: Lens' CreateConnectionResponse (Maybe Text)
fBandwidth = lens _fBandwidth (\s a -> s { _fBandwidth = a })
{-# INLINE fBandwidth #-}

-- | The VLAN ID. Example: 101.
fVlan :: Lens' CreateConnectionResponse (Maybe Integer)
fVlan = lens _fVlan (\s a -> s { _fVlan = a })
{-# INLINE fVlan #-}

fPartnerName :: Lens' CreateConnectionResponse (Maybe Text)
fPartnerName = lens _fPartnerName (\s a -> s { _fPartnerName = a })
{-# INLINE fPartnerName #-}

instance FromJSON CreateConnectionResponse

instance AWSRequest CreateConnection where
    type Sv CreateConnection = DirectConnect
    type Rs CreateConnection = CreateConnectionResponse

    request = get
    response _ = jsonResponse

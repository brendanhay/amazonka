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
    , allocateConnectionOnInterconnect
    -- ** Request lenses
    , acoirBandwidth
    , acoirConnectionName
    , acoirInterconnectId
    , acoirOwnerAccount
    , acoirVlan

    -- * Response
    , AllocateConnectionOnInterconnectResponse
    -- ** Response lenses
    , cBandwidth
    , cConnectionId
    , cConnectionName
    , cConnectionState
    , cLocation
    , cOwnerAccount
    , cPartnerName
    , cRegion
    , cVlan
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AllocateConnectionOnInterconnect' request.
allocateConnectionOnInterconnect :: Text -- ^ 'acoirBandwidth'
                                 -> Text -- ^ 'acoirConnectionName'
                                 -> Text -- ^ 'acoirInterconnectId'
                                 -> Text -- ^ 'acoirOwnerAccount'
                                 -> Integer -- ^ 'acoirVlan'
                                 -> AllocateConnectionOnInterconnect
allocateConnectionOnInterconnect p1 p2 p3 p4 p5 = AllocateConnectionOnInterconnect
    { _acoirBandwidth = p1
    , _acoirConnectionName = p2
    , _acoirInterconnectId = p3
    , _acoirOwnerAccount = p4
    , _acoirVlan = p5
    }
{-# INLINE allocateConnectionOnInterconnect #-}

data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { _acoirBandwidth :: Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _acoirConnectionName :: Text
      -- ^ Name of the provisioned connection. Example: "500M Connection to
      -- AWS" Default: None.
    , _acoirInterconnectId :: Text
      -- ^ ID of the interconnect on which the connection will be
      -- provisioned. Example: dxcon-456abc78 Default: None.
    , _acoirOwnerAccount :: Text
      -- ^ Numeric account Id of the customer for whom the connection will
      -- be provisioned. Example: 123443215678 Default: None.
    , _acoirVlan :: Integer
      -- ^ The dedicated VLAN provisioned to the connection. Example: 101
      -- Default: None.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoirBandwidth :: Lens' AllocateConnectionOnInterconnect (Text)
acoirBandwidth f x =
    f (_acoirBandwidth x)
        <&> \y -> x { _acoirBandwidth = y }
{-# INLINE acoirBandwidth #-}

-- | Name of the provisioned connection. Example: "500M Connection to AWS"
-- Default: None.
acoirConnectionName :: Lens' AllocateConnectionOnInterconnect (Text)
acoirConnectionName f x =
    f (_acoirConnectionName x)
        <&> \y -> x { _acoirConnectionName = y }
{-# INLINE acoirConnectionName #-}

-- | ID of the interconnect on which the connection will be provisioned.
-- Example: dxcon-456abc78 Default: None.
acoirInterconnectId :: Lens' AllocateConnectionOnInterconnect (Text)
acoirInterconnectId f x =
    f (_acoirInterconnectId x)
        <&> \y -> x { _acoirInterconnectId = y }
{-# INLINE acoirInterconnectId #-}

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned. Example: 123443215678 Default: None.
acoirOwnerAccount :: Lens' AllocateConnectionOnInterconnect (Text)
acoirOwnerAccount f x =
    f (_acoirOwnerAccount x)
        <&> \y -> x { _acoirOwnerAccount = y }
{-# INLINE acoirOwnerAccount #-}

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default:
-- None.
acoirVlan :: Lens' AllocateConnectionOnInterconnect (Integer)
acoirVlan f x =
    f (_acoirVlan x)
        <&> \y -> x { _acoirVlan = y }
{-# INLINE acoirVlan #-}

instance ToPath AllocateConnectionOnInterconnect

instance ToQuery AllocateConnectionOnInterconnect

instance ToHeaders AllocateConnectionOnInterconnect

instance ToJSON AllocateConnectionOnInterconnect

data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _cBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _cConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _cConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _cConnectionState :: Maybe ConnectionState
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
    , _cLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _cOwnerAccount :: Maybe Text
    , _cPartnerName :: Maybe Text
    , _cRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _cVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cBandwidth :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cBandwidth f x =
    f (_cBandwidth x)
        <&> \y -> x { _cBandwidth = y }
{-# INLINE cBandwidth #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cConnectionId :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cConnectionId f x =
    f (_cConnectionId x)
        <&> \y -> x { _cConnectionId = y }
{-# INLINE cConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cConnectionName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cConnectionName f x =
    f (_cConnectionName x)
        <&> \y -> x { _cConnectionName = y }
{-# INLINE cConnectionName #-}

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
cConnectionState :: Lens' AllocateConnectionOnInterconnectResponse (Maybe ConnectionState)
cConnectionState f x =
    f (_cConnectionState x)
        <&> \y -> x { _cConnectionState = y }
{-# INLINE cConnectionState #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
cLocation :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cLocation f x =
    f (_cLocation x)
        <&> \y -> x { _cLocation = y }
{-# INLINE cLocation #-}

cOwnerAccount :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cOwnerAccount f x =
    f (_cOwnerAccount x)
        <&> \y -> x { _cOwnerAccount = y }
{-# INLINE cOwnerAccount #-}

cPartnerName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cPartnerName f x =
    f (_cPartnerName x)
        <&> \y -> x { _cPartnerName = y }
{-# INLINE cPartnerName #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
cRegion :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cRegion f x =
    f (_cRegion x)
        <&> \y -> x { _cRegion = y }
{-# INLINE cRegion #-}

-- | The VLAN ID. Example: 101.
cVlan :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Integer)
cVlan f x =
    f (_cVlan x)
        <&> \y -> x { _cVlan = y }
{-# INLINE cVlan #-}

instance FromJSON AllocateConnectionOnInterconnectResponse

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request = get
    response _ = jsonResponse

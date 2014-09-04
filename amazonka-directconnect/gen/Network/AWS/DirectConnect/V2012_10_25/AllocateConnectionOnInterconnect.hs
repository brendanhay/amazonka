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
    , mkAllocateConnectionOnInterconnectRequest
    -- ** Request lenses
    , acoirBandwidth
    , acoirConnectionName
    , acoirOwnerAccount
    , acoirInterconnectId
    , acoirVlan

    -- * Response
    , AllocateConnectionOnInterconnectResponse
    -- ** Response lenses
    , cOwnerAccount
    , cConnectionId
    , cConnectionName
    , cConnectionState
    , cRegion
    , cLocation
    , cBandwidth
    , cVlan
    , cPartnerName
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateConnectionOnInterconnect' request.
mkAllocateConnectionOnInterconnectRequest :: Text -- ^ 'acoirBandwidth'
                                          -> Text -- ^ 'acoirConnectionName'
                                          -> Text -- ^ 'acoirOwnerAccount'
                                          -> Text -- ^ 'acoirInterconnectId'
                                          -> Integer -- ^ 'acoirVlan'
                                          -> AllocateConnectionOnInterconnect
mkAllocateConnectionOnInterconnectRequest p1 p2 p3 p4 p5 = AllocateConnectionOnInterconnect
    { _acoirBandwidth = p1
    , _acoirConnectionName = p2
    , _acoirOwnerAccount = p3
    , _acoirInterconnectId = p4
    , _acoirVlan = p5
    }
{-# INLINE mkAllocateConnectionOnInterconnectRequest #-}

data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { _acoirBandwidth :: Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _acoirConnectionName :: Text
      -- ^ Name of the provisioned connection. Example: "500M Connection to
      -- AWS" Default: None.
    , _acoirOwnerAccount :: Text
      -- ^ Numeric account Id of the customer for whom the connection will
      -- be provisioned. Example: 123443215678 Default: None.
    , _acoirInterconnectId :: Text
      -- ^ ID of the interconnect on which the connection will be
      -- provisioned. Example: dxcon-456abc78 Default: None.
    , _acoirVlan :: Integer
      -- ^ The dedicated VLAN provisioned to the connection. Example: 101
      -- Default: None.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoirBandwidth :: Lens' AllocateConnectionOnInterconnect (Text)
acoirBandwidth = lens _acoirBandwidth (\s a -> s { _acoirBandwidth = a })
{-# INLINE acoirBandwidth #-}

-- | Name of the provisioned connection. Example: "500M Connection to AWS"
-- Default: None.
acoirConnectionName :: Lens' AllocateConnectionOnInterconnect (Text)
acoirConnectionName = lens _acoirConnectionName (\s a -> s { _acoirConnectionName = a })
{-# INLINE acoirConnectionName #-}

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned. Example: 123443215678 Default: None.
acoirOwnerAccount :: Lens' AllocateConnectionOnInterconnect (Text)
acoirOwnerAccount = lens _acoirOwnerAccount (\s a -> s { _acoirOwnerAccount = a })
{-# INLINE acoirOwnerAccount #-}

-- | ID of the interconnect on which the connection will be provisioned.
-- Example: dxcon-456abc78 Default: None.
acoirInterconnectId :: Lens' AllocateConnectionOnInterconnect (Text)
acoirInterconnectId = lens _acoirInterconnectId (\s a -> s { _acoirInterconnectId = a })
{-# INLINE acoirInterconnectId #-}

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default:
-- None.
acoirVlan :: Lens' AllocateConnectionOnInterconnect (Integer)
acoirVlan = lens _acoirVlan (\s a -> s { _acoirVlan = a })
{-# INLINE acoirVlan #-}

instance ToPath AllocateConnectionOnInterconnect

instance ToQuery AllocateConnectionOnInterconnect

instance ToHeaders AllocateConnectionOnInterconnect

instance ToJSON AllocateConnectionOnInterconnect

data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _cOwnerAccount :: Maybe Text
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
    , _cRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _cLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _cBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _cVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _cPartnerName :: Maybe Text
    } deriving (Show, Generic)

cOwnerAccount :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cOwnerAccount = lens _cOwnerAccount (\s a -> s { _cOwnerAccount = a })
{-# INLINE cOwnerAccount #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
cConnectionId :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cConnectionId = lens _cConnectionId (\s a -> s { _cConnectionId = a })
{-# INLINE cConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
cConnectionName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cConnectionName = lens _cConnectionName (\s a -> s { _cConnectionName = a })
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
cConnectionState = lens _cConnectionState (\s a -> s { _cConnectionState = a })
{-# INLINE cConnectionState #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
cRegion :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cRegion = lens _cRegion (\s a -> s { _cRegion = a })
{-# INLINE cRegion #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
cLocation :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cLocation = lens _cLocation (\s a -> s { _cLocation = a })
{-# INLINE cLocation #-}

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cBandwidth :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cBandwidth = lens _cBandwidth (\s a -> s { _cBandwidth = a })
{-# INLINE cBandwidth #-}

-- | The VLAN ID. Example: 101.
cVlan :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Integer)
cVlan = lens _cVlan (\s a -> s { _cVlan = a })
{-# INLINE cVlan #-}

cPartnerName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
cPartnerName = lens _cPartnerName (\s a -> s { _cPartnerName = a })
{-# INLINE cPartnerName #-}

instance FromJSON AllocateConnectionOnInterconnectResponse

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request = get
    response _ = jsonResponse

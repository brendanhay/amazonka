{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DeleteConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the connection. Deleting a connection only stops the AWS Direct
-- Connect port hour and data transfer charges. You need to cancel separately
-- with the providers any services or charges for cross-connects or network
-- circuits that connect you to the AWS Direct Connect location.
module Network.AWS.DirectConnect.V2012_10_25.DeleteConnection
    (
    -- * Request
      DeleteConnection
    -- ** Request constructor
    , deleteConnection
    -- ** Request lenses
    , dcrConnectionId

    -- * Response
    , DeleteConnectionResponse
    -- ** Response lenses
    , kBandwidth
    , kConnectionId
    , kConnectionName
    , kConnectionState
    , kLocation
    , kOwnerAccount
    , kPartnerName
    , kRegion
    , kVlan
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteConnection' request.
deleteConnection :: Text -- ^ 'dcrConnectionId'
                 -> DeleteConnection
deleteConnection p1 = DeleteConnection
    { _dcrConnectionId = p1
    }
{-# INLINE deleteConnection #-}

data DeleteConnection = DeleteConnection
    { _dcrConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcrConnectionId :: Lens' DeleteConnection (Text)
dcrConnectionId f x =
    f (_dcrConnectionId x)
        <&> \y -> x { _dcrConnectionId = y }
{-# INLINE dcrConnectionId #-}

instance ToPath DeleteConnection

instance ToQuery DeleteConnection

instance ToHeaders DeleteConnection

instance ToJSON DeleteConnection

data DeleteConnectionResponse = DeleteConnectionResponse
    { _kBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _kConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _kConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _kConnectionState :: Maybe ConnectionState
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
    , _kLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _kOwnerAccount :: Maybe Text
    , _kPartnerName :: Maybe Text
    , _kRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _kVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
kBandwidth :: Lens' DeleteConnectionResponse (Maybe Text)
kBandwidth f x =
    f (_kBandwidth x)
        <&> \y -> x { _kBandwidth = y }
{-# INLINE kBandwidth #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
kConnectionId :: Lens' DeleteConnectionResponse (Maybe Text)
kConnectionId f x =
    f (_kConnectionId x)
        <&> \y -> x { _kConnectionId = y }
{-# INLINE kConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
kConnectionName :: Lens' DeleteConnectionResponse (Maybe Text)
kConnectionName f x =
    f (_kConnectionName x)
        <&> \y -> x { _kConnectionName = y }
{-# INLINE kConnectionName #-}

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
kConnectionState :: Lens' DeleteConnectionResponse (Maybe ConnectionState)
kConnectionState f x =
    f (_kConnectionState x)
        <&> \y -> x { _kConnectionState = y }
{-# INLINE kConnectionState #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
kLocation :: Lens' DeleteConnectionResponse (Maybe Text)
kLocation f x =
    f (_kLocation x)
        <&> \y -> x { _kLocation = y }
{-# INLINE kLocation #-}

kOwnerAccount :: Lens' DeleteConnectionResponse (Maybe Text)
kOwnerAccount f x =
    f (_kOwnerAccount x)
        <&> \y -> x { _kOwnerAccount = y }
{-# INLINE kOwnerAccount #-}

kPartnerName :: Lens' DeleteConnectionResponse (Maybe Text)
kPartnerName f x =
    f (_kPartnerName x)
        <&> \y -> x { _kPartnerName = y }
{-# INLINE kPartnerName #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
kRegion :: Lens' DeleteConnectionResponse (Maybe Text)
kRegion f x =
    f (_kRegion x)
        <&> \y -> x { _kRegion = y }
{-# INLINE kRegion #-}

-- | The VLAN ID. Example: 101.
kVlan :: Lens' DeleteConnectionResponse (Maybe Integer)
kVlan f x =
    f (_kVlan x)
        <&> \y -> x { _kVlan = y }
{-# INLINE kVlan #-}

instance FromJSON DeleteConnectionResponse

instance AWSRequest DeleteConnection where
    type Sv DeleteConnection = DirectConnect
    type Rs DeleteConnection = DeleteConnectionResponse

    request = get
    response _ = jsonResponse

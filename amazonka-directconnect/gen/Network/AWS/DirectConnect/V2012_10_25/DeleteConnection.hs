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

data DeleteConnection = DeleteConnection
    { _dcrConnectionId :: Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcrConnectionId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteConnection
    -> f DeleteConnection
dcrConnectionId f x =
    (\y -> x { _dcrConnectionId = y })
       <$> f (_dcrConnectionId x)
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
kBandwidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kBandwidth f x =
    (\y -> x { _kBandwidth = y })
       <$> f (_kBandwidth x)
{-# INLINE kBandwidth #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
kConnectionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kConnectionId f x =
    (\y -> x { _kConnectionId = y })
       <$> f (_kConnectionId x)
{-# INLINE kConnectionId #-}

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
kConnectionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kConnectionName f x =
    (\y -> x { _kConnectionName = y })
       <$> f (_kConnectionName x)
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
kConnectionState
    :: Functor f
    => (Maybe ConnectionState
    -> f (Maybe ConnectionState))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kConnectionState f x =
    (\y -> x { _kConnectionState = y })
       <$> f (_kConnectionState x)
{-# INLINE kConnectionState #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
kLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kLocation f x =
    (\y -> x { _kLocation = y })
       <$> f (_kLocation x)
{-# INLINE kLocation #-}

kOwnerAccount
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kOwnerAccount f x =
    (\y -> x { _kOwnerAccount = y })
       <$> f (_kOwnerAccount x)
{-# INLINE kOwnerAccount #-}

kPartnerName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kPartnerName f x =
    (\y -> x { _kPartnerName = y })
       <$> f (_kPartnerName x)
{-# INLINE kPartnerName #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
kRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kRegion f x =
    (\y -> x { _kRegion = y })
       <$> f (_kRegion x)
{-# INLINE kRegion #-}

-- | The VLAN ID. Example: 101.
kVlan
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DeleteConnectionResponse
    -> f DeleteConnectionResponse
kVlan f x =
    (\y -> x { _kVlan = y })
       <$> f (_kVlan x)
{-# INLINE kVlan #-}

instance FromJSON DeleteConnectionResponse

instance AWSRequest DeleteConnection where
    type Sv DeleteConnection = DirectConnect
    type Rs DeleteConnection = DeleteConnectionResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.CreateInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new interconnect between a AWS Direct Connect partner's network
-- and a specific AWS Direct Connect location. An interconnect is a connection
-- which is capable of hosting other connections. The AWS Direct Connect
-- partner can use an interconnect to provide sub-1Gbps AWS Direct Connect
-- service to tier 2 customers who do not have their own connections. Like a
-- standard connection, an interconnect links the AWS Direct Connect partner's
-- network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps
-- Ethernet fiber-optic cable. One end is connected to the partner's router,
-- the other to an AWS Direct Connect router. For each end customer, the AWS
-- Direct Connect partner provisions a connection on their interconnect by
-- calling AllocateConnectionOnInterconnect. The end customer can then connect
-- to AWS resources by creating a virtual interface on their connection, using
-- the VLAN assigned to them by the AWS Direct Connect partner.
module Network.AWS.DirectConnect.V2012_10_25.CreateInterconnect
    (
    -- * Request
      CreateInterconnect
    -- ** Request constructor
    , createInterconnect
    -- ** Request lenses
    , cirBandwidth
    , cirInterconnectName
    , cirLocation

    -- * Response
    , CreateInterconnectResponse
    -- ** Response lenses
    , iBandwidth
    , iInterconnectId
    , iInterconnectName
    , iInterconnectState
    , iLocation
    , iRegion
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateInterconnect' request.
createInterconnect :: Text -- ^ 'cirBandwidth'
                   -> Text -- ^ 'cirInterconnectName'
                   -> Text -- ^ 'cirLocation'
                   -> CreateInterconnect
createInterconnect p1 p2 p3 = CreateInterconnect
    { _cirBandwidth = p1
    , _cirInterconnectName = p2
    , _cirLocation = p3
    }

data CreateInterconnect = CreateInterconnect
    { _cirBandwidth :: Text
      -- ^ The port bandwidth Example: 1Gbps Default: None Available values:
      -- 1Gbps,10Gbps.
    , _cirInterconnectName :: Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS"
      -- Default: None.
    , _cirLocation :: Text
      -- ^ Where the interconnect is located Example: EqSV5 Default: None.
    } deriving (Show, Generic)

-- | The port bandwidth Example: 1Gbps Default: None Available values:
-- 1Gbps,10Gbps.
cirBandwidth
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInterconnect
    -> f CreateInterconnect
cirBandwidth f x =
    (\y -> x { _cirBandwidth = y })
       <$> f (_cirBandwidth x)
{-# INLINE cirBandwidth #-}

-- | The name of the interconnect. Example: "1G Interconnect to AWS" Default:
-- None.
cirInterconnectName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInterconnect
    -> f CreateInterconnect
cirInterconnectName f x =
    (\y -> x { _cirInterconnectName = y })
       <$> f (_cirInterconnectName x)
{-# INLINE cirInterconnectName #-}

-- | Where the interconnect is located Example: EqSV5 Default: None.
cirLocation
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInterconnect
    -> f CreateInterconnect
cirLocation f x =
    (\y -> x { _cirLocation = y })
       <$> f (_cirLocation x)
{-# INLINE cirLocation #-}

instance ToPath CreateInterconnect

instance ToQuery CreateInterconnect

instance ToHeaders CreateInterconnect

instance ToJSON CreateInterconnect

data CreateInterconnectResponse = CreateInterconnectResponse
    { _iBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _iInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , _iInterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , _iInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an
      -- interconnect. The interconnect stays in the requested state until
      -- the Letter of Authorization (LOA) is sent to the customer.
      -- Pending: The interconnect has been approved, and is being
      -- initialized. Available: The network link is up, and the
      -- interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , _iLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _iRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    } deriving (Show, Generic)

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
iBandwidth
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iBandwidth f x =
    (\y -> x { _iBandwidth = y })
       <$> f (_iBandwidth x)
{-# INLINE iBandwidth #-}

-- | The ID of the interconnect. Example: dxcon-abc123.
iInterconnectId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iInterconnectId f x =
    (\y -> x { _iInterconnectId = y })
       <$> f (_iInterconnectId x)
{-# INLINE iInterconnectId #-}

-- | The name of the interconnect. Example: "1G Interconnect to AWS".
iInterconnectName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iInterconnectName f x =
    (\y -> x { _iInterconnectName = y })
       <$> f (_iInterconnectName x)
{-# INLINE iInterconnectName #-}

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
iInterconnectState
    :: Functor f
    => (Maybe InterconnectState
    -> f (Maybe InterconnectState))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iInterconnectState f x =
    (\y -> x { _iInterconnectState = y })
       <$> f (_iInterconnectState x)
{-# INLINE iInterconnectState #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
iLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iLocation f x =
    (\y -> x { _iLocation = y })
       <$> f (_iLocation x)
{-# INLINE iLocation #-}

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
iRegion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInterconnectResponse
    -> f CreateInterconnectResponse
iRegion f x =
    (\y -> x { _iRegion = y })
       <$> f (_iRegion x)
{-# INLINE iRegion #-}

instance FromJSON CreateInterconnectResponse

instance AWSRequest CreateInterconnect where
    type Sv CreateInterconnect = DirectConnect
    type Rs CreateInterconnect = CreateInterconnectResponse

    request = get
    response _ = jsonResponse

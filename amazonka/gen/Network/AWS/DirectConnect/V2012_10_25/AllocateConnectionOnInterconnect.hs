{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.DirectConnect.V2012_10_25.AllocateConnectionOnInterconnect where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

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

makeLenses ''AllocateConnectionOnInterconnect

instance ToPath AllocateConnectionOnInterconnect

instance ToQuery AllocateConnectionOnInterconnect

instance ToHeaders AllocateConnectionOnInterconnect

instance ToJSON AllocateConnectionOnInterconnect

data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _xBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _xConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _xConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS"
      -- Default: None.
    , _xConnectionState :: Maybe ConnectionState
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
    , _xLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _xOwnerAccount :: Maybe Text
    , _xPartnerName :: Maybe Text
    , _xRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    , _xVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    } deriving (Show, Generic)

makeLenses ''AllocateConnectionOnInterconnectResponse

instance FromJSON AllocateConnectionOnInterconnectResponse

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request = get
    response _ = undefined

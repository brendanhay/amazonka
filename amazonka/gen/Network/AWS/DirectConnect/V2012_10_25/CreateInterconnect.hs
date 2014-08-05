{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.DirectConnect.V2012_10_25.CreateInterconnect where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

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

makeLenses ''CreateInterconnect

instance ToPath CreateInterconnect

instance ToQuery CreateInterconnect

instance ToHeaders CreateInterconnect

instance ToJSON CreateInterconnect

data CreateInterconnectResponse = CreateInterconnectResponse
    { _iuBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , _iuInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , _iuInterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , _iuInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an
      -- interconnect. The interconnect stays in the requested state until
      -- the Letter of Authorization (LOA) is sent to the customer.
      -- Pending: The interconnect has been approved, and is being
      -- initialized. Available: The network link is up, and the
      -- interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , _iuLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _iuRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example:
      -- us-east-1 Default: None.
    } deriving (Show, Generic)

makeLenses ''CreateInterconnectResponse

instance FromJSON CreateInterconnectResponse

instance AWSRequest CreateInterconnect where
    type Sv CreateInterconnect = DirectConnect
    type Rs CreateInterconnect = CreateInterconnectResponse

    request = get
    response _ = undefined

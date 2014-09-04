{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeConnectionsOnInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return a list of connections that have been provisioned on the given
-- interconnect.
module Network.AWS.DirectConnect.V2012_10_25.DescribeConnectionsOnInterconnect
    (
    -- * Request
      DescribeConnectionsOnInterconnect
    -- ** Request constructor
    , mkDescribeConnectionsOnInterconnectRequest
    -- ** Request lenses
    , dcoirInterconnectId

    -- * Response
    , DescribeConnectionsOnInterconnectResponse
    -- ** Response lenses
    , pConnections
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnectionsOnInterconnect' request.
mkDescribeConnectionsOnInterconnectRequest :: Text -- ^ 'dcoirInterconnectId'
                                           -> DescribeConnectionsOnInterconnect
mkDescribeConnectionsOnInterconnectRequest p1 = DescribeConnectionsOnInterconnect
    { _dcoirInterconnectId = p1
    }
{-# INLINE mkDescribeConnectionsOnInterconnectRequest #-}

newtype DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect
    { _dcoirInterconnectId :: Text
      -- ^ ID of the interconnect on which a list of connection is
      -- provisioned. Example: dxcon-abc123 Default: None.
    } deriving (Show, Generic)

-- | ID of the interconnect on which a list of connection is provisioned.
-- Example: dxcon-abc123 Default: None.
dcoirInterconnectId :: Lens' DescribeConnectionsOnInterconnect (Text)
dcoirInterconnectId = lens _dcoirInterconnectId (\s a -> s { _dcoirInterconnectId = a })
{-# INLINE dcoirInterconnectId #-}

instance ToPath DescribeConnectionsOnInterconnect

instance ToQuery DescribeConnectionsOnInterconnect

instance ToHeaders DescribeConnectionsOnInterconnect

instance ToJSON DescribeConnectionsOnInterconnect

newtype DescribeConnectionsOnInterconnectResponse = DescribeConnectionsOnInterconnectResponse
    { _pConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Show, Generic)

-- | A list of connections.
pConnections :: Lens' DescribeConnectionsOnInterconnectResponse ([Connection])
pConnections = lens _pConnections (\s a -> s { _pConnections = a })
{-# INLINE pConnections #-}

instance FromJSON DescribeConnectionsOnInterconnectResponse

instance AWSRequest DescribeConnectionsOnInterconnect where
    type Sv DescribeConnectionsOnInterconnect = DirectConnect
    type Rs DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnectResponse

    request = get
    response _ = jsonResponse

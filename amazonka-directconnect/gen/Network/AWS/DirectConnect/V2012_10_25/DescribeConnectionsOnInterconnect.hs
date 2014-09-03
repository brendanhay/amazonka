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
    , describeConnectionsOnInterconnect
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

-- | Minimum specification for a 'DescribeConnectionsOnInterconnect' request.
describeConnectionsOnInterconnect :: Text -- ^ 'dcoirInterconnectId'
                                  -> DescribeConnectionsOnInterconnect
describeConnectionsOnInterconnect p1 = DescribeConnectionsOnInterconnect
    { _dcoirInterconnectId = p1
    }

data DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect
    { _dcoirInterconnectId :: Text
      -- ^ ID of the interconnect on which a list of connection is
      -- provisioned. Example: dxcon-abc123 Default: None.
    } deriving (Show, Generic)

-- | ID of the interconnect on which a list of connection is provisioned.
-- Example: dxcon-abc123 Default: None.
dcoirInterconnectId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeConnectionsOnInterconnect
    -> f DescribeConnectionsOnInterconnect
dcoirInterconnectId f x =
    (\y -> x { _dcoirInterconnectId = y })
       <$> f (_dcoirInterconnectId x)
{-# INLINE dcoirInterconnectId #-}

instance ToPath DescribeConnectionsOnInterconnect

instance ToQuery DescribeConnectionsOnInterconnect

instance ToHeaders DescribeConnectionsOnInterconnect

instance ToJSON DescribeConnectionsOnInterconnect

data DescribeConnectionsOnInterconnectResponse = DescribeConnectionsOnInterconnectResponse
    { _pConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Show, Generic)

-- | A list of connections.
pConnections
    :: Functor f
    => ([Connection]
    -> f ([Connection]))
    -> DescribeConnectionsOnInterconnectResponse
    -> f DescribeConnectionsOnInterconnectResponse
pConnections f x =
    (\y -> x { _pConnections = y })
       <$> f (_pConnections x)
{-# INLINE pConnections #-}

instance FromJSON DescribeConnectionsOnInterconnectResponse

instance AWSRequest DescribeConnectionsOnInterconnect where
    type Sv DescribeConnectionsOnInterconnect = DirectConnect
    type Rs DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnectResponse

    request = get
    response _ = jsonResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays all connections in this region. If a connection ID is provided,
-- the call returns only that particular connection.
module Network.AWS.DirectConnect.V2012_10_25.DescribeConnections
    (
    -- * Request
      DescribeConnections
    -- ** Request constructor
    , describeConnections
    -- ** Request lenses
    , dcsConnectionId

    -- * Response
    , DescribeConnectionsResponse
    -- ** Response lenses
    , mConnections
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeConnections' request.
describeConnections :: DescribeConnections
describeConnections = DescribeConnections
    { _dcsConnectionId = Nothing
    }
{-# INLINE describeConnections #-}

data DescribeConnections = DescribeConnections
    { _dcsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcsConnectionId :: Lens' DescribeConnections (Maybe Text)
dcsConnectionId f x =
    f (_dcsConnectionId x)
        <&> \y -> x { _dcsConnectionId = y }
{-# INLINE dcsConnectionId #-}

instance ToPath DescribeConnections

instance ToQuery DescribeConnections

instance ToHeaders DescribeConnections

instance ToJSON DescribeConnections

data DescribeConnectionsResponse = DescribeConnectionsResponse
    { _mConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Show, Generic)

-- | A list of connections.
mConnections :: Lens' DescribeConnectionsResponse ([Connection])
mConnections f x =
    f (_mConnections x)
        <&> \y -> x { _mConnections = y }
{-# INLINE mConnections #-}

instance FromJSON DescribeConnectionsResponse

instance AWSRequest DescribeConnections where
    type Sv DescribeConnections = DirectConnect
    type Rs DescribeConnections = DescribeConnectionsResponse

    request = get
    response _ = jsonResponse

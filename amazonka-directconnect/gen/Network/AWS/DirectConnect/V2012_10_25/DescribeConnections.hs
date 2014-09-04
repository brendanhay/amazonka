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
    , mkDescribeConnectionsRequest
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnections' request.
mkDescribeConnectionsRequest :: DescribeConnections
mkDescribeConnectionsRequest = DescribeConnections
    { _dcsConnectionId = Nothing
    }
{-# INLINE mkDescribeConnectionsRequest #-}

newtype DescribeConnections = DescribeConnections
    { _dcsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Show, Generic)

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcsConnectionId :: Lens' DescribeConnections (Maybe Text)
dcsConnectionId = lens _dcsConnectionId (\s a -> s { _dcsConnectionId = a })
{-# INLINE dcsConnectionId #-}

instance ToPath DescribeConnections

instance ToQuery DescribeConnections

instance ToHeaders DescribeConnections

instance ToJSON DescribeConnections

newtype DescribeConnectionsResponse = DescribeConnectionsResponse
    { _mConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Show, Generic)

-- | A list of connections.
mConnections :: Lens' DescribeConnectionsResponse ([Connection])
mConnections = lens _mConnections (\s a -> s { _mConnections = a })
{-# INLINE mConnections #-}

instance FromJSON DescribeConnectionsResponse

instance AWSRequest DescribeConnections where
    type Sv DescribeConnections = DirectConnect
    type Rs DescribeConnections = DescribeConnectionsResponse

    request = get
    response _ = jsonResponse

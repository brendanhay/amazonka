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
    , mkDescribeConnections
    -- ** Request lenses
    , dc1ConnectionId

    -- * Response
    , DescribeConnectionsResponse
    -- ** Response constructor
    , mkDescribeConnectionsResponse
    -- ** Response lenses
    , dcrrConnections
    ) where

import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DescribeConnections operation.
newtype DescribeConnections = DescribeConnections
    { _dc1ConnectionId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnections' request.
mkDescribeConnections :: DescribeConnections
mkDescribeConnections = DescribeConnections
    { _dc1ConnectionId = Nothing
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dc1ConnectionId :: Lens' DescribeConnections (Maybe Text)
dc1ConnectionId = lens _dc1ConnectionId (\s a -> s { _dc1ConnectionId = a })

instance ToPath DescribeConnections

instance ToQuery DescribeConnections

instance ToHeaders DescribeConnections

instance ToJSON DescribeConnections

-- | A structure containing a list of connections.
newtype DescribeConnectionsResponse = DescribeConnectionsResponse
    { _dcrrConnections :: [Connection]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnectionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeConnectionsResponse :: DescribeConnectionsResponse
mkDescribeConnectionsResponse = DescribeConnectionsResponse
    { _dcrrConnections = mempty
    }

-- | A list of connections.
dcrrConnections :: Lens' DescribeConnectionsResponse [Connection]
dcrrConnections = lens _dcrrConnections (\s a -> s { _dcrrConnections = a })

instance FromJSON DescribeConnectionsResponse

instance AWSRequest DescribeConnections where
    type Sv DescribeConnections = DirectConnect
    type Rs DescribeConnections = DescribeConnectionsResponse

    request = get
    response _ = jsonResponse

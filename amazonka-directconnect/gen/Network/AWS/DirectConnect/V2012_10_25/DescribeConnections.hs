{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.DirectConnect.V2012_10_25.DescribeConnections where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeConnections' request.
describeConnections :: DescribeConnections
describeConnections = DescribeConnections
    { _dcsConnectionId = Nothing
    }

data DescribeConnections = DescribeConnections
    { _dcsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Show, Generic)

makeLenses ''DescribeConnections

instance ToPath DescribeConnections

instance ToQuery DescribeConnections

instance ToHeaders DescribeConnections

instance ToJSON DescribeConnections

data DescribeConnectionsResponse = DescribeConnectionsResponse
    { _hConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Show, Generic)

makeLenses ''DescribeConnectionsResponse

instance FromJSON DescribeConnectionsResponse

instance AWSRequest DescribeConnections where
    type Sv DescribeConnections = DirectConnect
    type Rs DescribeConnections = DescribeConnectionsResponse

    request = get
    response _ = jsonResponse

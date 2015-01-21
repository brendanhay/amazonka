{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Displays all connections in this region.
--
-- If a connection ID is provided, the call returns only that particular
-- connection.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnections.html>
module Network.AWS.DirectConnect.DescribeConnections
    (
    -- * Request
      DescribeConnections
    -- ** Request constructor
    , describeConnections
    -- ** Request lenses
    , dc1ConnectionId

    -- * Response
    , DescribeConnectionsResponse
    -- ** Response constructor
    , describeConnectionsResponse
    -- ** Response lenses
    , dcrConnections
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

newtype DescribeConnections = DescribeConnections
    { _dc1ConnectionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DescribeConnections' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dc1ConnectionId' @::@ 'Maybe' 'Text'
--
describeConnections :: DescribeConnections
describeConnections = DescribeConnections
    { _dc1ConnectionId = Nothing
    }

dc1ConnectionId :: Lens' DescribeConnections (Maybe Text)
dc1ConnectionId = lens _dc1ConnectionId (\s a -> s { _dc1ConnectionId = a })

newtype DescribeConnectionsResponse = DescribeConnectionsResponse
    { _dcrConnections :: List "connections" Connection
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeConnectionsResponse where
    type Item DescribeConnectionsResponse = Connection

    fromList = DescribeConnectionsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcrConnections

-- | 'DescribeConnectionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrConnections' @::@ ['Connection']
--
describeConnectionsResponse :: DescribeConnectionsResponse
describeConnectionsResponse = DescribeConnectionsResponse
    { _dcrConnections = mempty
    }

-- | A list of connections.
dcrConnections :: Lens' DescribeConnectionsResponse [Connection]
dcrConnections = lens _dcrConnections (\s a -> s { _dcrConnections = a }) . _List

instance ToPath DescribeConnections where
    toPath = const "/"

instance ToQuery DescribeConnections where
    toQuery = const mempty

instance ToHeaders DescribeConnections

instance ToJSON DescribeConnections where
    toJSON DescribeConnections{..} = object
        [ "connectionId" .= _dc1ConnectionId
        ]

instance AWSRequest DescribeConnections where
    type Sv DescribeConnections = DirectConnect
    type Rs DescribeConnections = DescribeConnectionsResponse

    request  = post "DescribeConnections"
    response = jsonResponse

instance FromJSON DescribeConnectionsResponse where
    parseJSON = withObject "DescribeConnectionsResponse" $ \o -> DescribeConnectionsResponse
        <$> o .:? "connections" .!= mempty

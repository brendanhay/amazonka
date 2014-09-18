{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
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
module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    (
    -- * Request
      DescribeConnectionsOnInterconnect
    -- ** Request constructor
    , describeConnectionsOnInterconnect
    -- ** Request lenses
    , dcoiInterconnectId

    -- * Response
    , DescribeConnectionsOnInterconnectResponse
    -- ** Response constructor
    , describeConnectionsOnInterconnectResponse
    -- ** Response lenses
    , dcoirConnections
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DescribeConnectionsOnInterconnect
-- operation.
newtype DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect
    { _dcoiInterconnectId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnectionsOnInterconnect' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectId ::@ @Text@
--
describeConnectionsOnInterconnect :: Text -- ^ 'dcoiInterconnectId'
                                    -> DescribeConnectionsOnInterconnect
describeConnectionsOnInterconnect p1 = DescribeConnectionsOnInterconnect
    { _dcoiInterconnectId = p1
    }

-- | ID of the interconnect on which a list of connection is provisioned.
-- Example: dxcon-abc123 Default: None.
dcoiInterconnectId :: Lens' DescribeConnectionsOnInterconnect Text
dcoiInterconnectId =
    lens _dcoiInterconnectId (\s a -> s { _dcoiInterconnectId = a })

instance ToPath DescribeConnectionsOnInterconnect

instance ToQuery DescribeConnectionsOnInterconnect

instance ToHeaders DescribeConnectionsOnInterconnect

instance ToJSON DescribeConnectionsOnInterconnect

-- | A structure containing a list of connections.
newtype DescribeConnectionsOnInterconnectResponse = DescribeConnectionsOnInterconnectResponse
    { _dcoirConnections :: [Connection]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeConnectionsOnInterconnectResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Connections ::@ @[Connection]@
--
describeConnectionsOnInterconnectResponse :: DescribeConnectionsOnInterconnectResponse
describeConnectionsOnInterconnectResponse = DescribeConnectionsOnInterconnectResponse
    { _dcoirConnections = mempty
    }

-- | A list of connections.
dcoirConnections :: Lens' DescribeConnectionsOnInterconnectResponse [Connection]
dcoirConnections =
    lens _dcoirConnections (\s a -> s { _dcoirConnections = a })

instance FromJSON DescribeConnectionsOnInterconnectResponse

instance AWSRequest DescribeConnectionsOnInterconnect where
    type Sv DescribeConnectionsOnInterconnect = DirectConnect
    type Rs DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnectResponse

    request = get
    response _ = jsonResponse

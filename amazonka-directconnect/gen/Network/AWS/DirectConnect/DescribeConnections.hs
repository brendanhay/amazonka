{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , desConnectionId

    -- * Response
    , Connections
    -- ** Response constructor
    , connections
    -- ** Response lenses
    , conConnections
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConnections' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desConnectionId'
newtype DescribeConnections = DescribeConnections'{_desConnectionId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeConnections' smart constructor.
describeConnections :: DescribeConnections
describeConnections = DescribeConnections'{_desConnectionId = Nothing};

-- | FIXME: Undocumented member.
desConnectionId :: Lens' DescribeConnections (Maybe Text)
desConnectionId = lens _desConnectionId (\ s a -> s{_desConnectionId = a});

instance AWSRequest DescribeConnections where
        type Sv DescribeConnections = DirectConnect
        type Rs DescribeConnections = Connections
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeConnections where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeConnections" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnections where
        toJSON DescribeConnections'{..}
          = object ["connectionId" .= _desConnectionId]

instance ToPath DescribeConnections where
        toPath = const "/"

instance ToQuery DescribeConnections where
        toQuery = const mempty

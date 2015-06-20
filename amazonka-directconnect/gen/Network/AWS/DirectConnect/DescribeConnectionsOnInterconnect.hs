{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
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

-- | Return a list of connections that have been provisioned on the given
-- interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeConnectionsOnInterconnect.html>
module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    (
    -- * Request
      DescribeConnectionsOnInterconnect
    -- ** Request constructor
    , describeConnectionsOnInterconnect
    -- ** Request lenses
    , dcoiInterconnectId

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

-- | /See:/ 'describeConnectionsOnInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcoiInterconnectId'
newtype DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect'{_dcoiInterconnectId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeConnectionsOnInterconnect' smart constructor.
describeConnectionsOnInterconnect :: Text -> DescribeConnectionsOnInterconnect
describeConnectionsOnInterconnect pInterconnectId = DescribeConnectionsOnInterconnect'{_dcoiInterconnectId = pInterconnectId};

-- | ID of the interconnect on which a list of connection is provisioned.
--
-- Example: dxcon-abc123
--
-- Default: None
dcoiInterconnectId :: Lens' DescribeConnectionsOnInterconnect Text
dcoiInterconnectId = lens _dcoiInterconnectId (\ s a -> s{_dcoiInterconnectId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeConnectionsOnInterconnect
         where
        type Sv DescribeConnectionsOnInterconnect =
             DirectConnect
        type Rs DescribeConnectionsOnInterconnect =
             Connections
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeConnectionsOnInterconnect
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeConnectionsOnInterconnect"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnectionsOnInterconnect
         where
        toJSON DescribeConnectionsOnInterconnect'{..}
          = object ["interconnectId" .= _dcoiInterconnectId]

instance ToPath DescribeConnectionsOnInterconnect
         where
        toPath = const "/"

instance ToQuery DescribeConnectionsOnInterconnect
         where
        toQuery = const mempty

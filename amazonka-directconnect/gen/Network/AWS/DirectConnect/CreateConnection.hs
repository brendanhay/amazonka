{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.CreateConnection
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

-- | Creates a new connection between the customer network and a specific AWS
-- Direct Connect location.
--
-- A connection links your internal network to an AWS Direct Connect
-- location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic
-- cable. One end of the cable is connected to your router, the other to an
-- AWS Direct Connect router. An AWS Direct Connect location provides
-- access to Amazon Web Services in the region it is associated with. You
-- can establish connections with AWS Direct Connect locations in multiple
-- regions, but a connection in one region does not provide connectivity to
-- other regions.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreateConnection.html>
module Network.AWS.DirectConnect.CreateConnection
    (
    -- * Request
      CreateConnection
    -- ** Request constructor
    , createConnection
    -- ** Request lenses
    , ccLocation
    , ccBandwidth
    , ccConnectionName

    -- * Response
    , Connection
    -- ** Response constructor
    , connection
    -- ** Response lenses
    , conVlan
    , conLocation
    , conConnectionId
    , conConnectionName
    , conPartnerName
    , conBandwidth
    , conRegion
    , conOwnerAccount
    , conConnectionState
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the CreateConnection operation.
--
-- /See:/ 'createConnection' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccLocation'
--
-- * 'ccBandwidth'
--
-- * 'ccConnectionName'
data CreateConnection = CreateConnection'
    { _ccLocation       :: !Text
    , _ccBandwidth      :: !Text
    , _ccConnectionName :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateConnection' smart constructor.
createConnection :: Text -> Text -> Text -> CreateConnection
createConnection pLocation pBandwidth pConnectionName =
    CreateConnection'
    { _ccLocation = pLocation
    , _ccBandwidth = pBandwidth
    , _ccConnectionName = pConnectionName
    }

-- | FIXME: Undocumented member.
ccLocation :: Lens' CreateConnection Text
ccLocation = lens _ccLocation (\ s a -> s{_ccLocation = a});

-- | FIXME: Undocumented member.
ccBandwidth :: Lens' CreateConnection Text
ccBandwidth = lens _ccBandwidth (\ s a -> s{_ccBandwidth = a});

-- | FIXME: Undocumented member.
ccConnectionName :: Lens' CreateConnection Text
ccConnectionName = lens _ccConnectionName (\ s a -> s{_ccConnectionName = a});

instance AWSRequest CreateConnection where
        type Sv CreateConnection = DirectConnect
        type Rs CreateConnection = Connection
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateConnection where
        toJSON CreateConnection'{..}
          = object
              ["location" .= _ccLocation,
               "bandwidth" .= _ccBandwidth,
               "connectionName" .= _ccConnectionName]

instance ToPath CreateConnection where
        toPath = const "/"

instance ToQuery CreateConnection where
        toQuery = const mempty

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new connection between the customer network and a specific AWS
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
    , ccrqLocation
    , ccrqBandwidth
    , ccrqConnectionName

    -- * Response
    , Connection
    -- ** Response constructor
    , connection
    -- ** Response lenses
    , cVlan
    , cLocation
    , cConnectionId
    , cConnectionName
    , cPartnerName
    , cBandwidth
    , cRegion
    , cOwnerAccount
    , cConnectionState
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
-- * 'ccrqLocation'
--
-- * 'ccrqBandwidth'
--
-- * 'ccrqConnectionName'
data CreateConnection = CreateConnection'
    { _ccrqLocation       :: !Text
    , _ccrqBandwidth      :: !Text
    , _ccrqConnectionName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateConnection' smart constructor.
createConnection :: Text -> Text -> Text -> CreateConnection
createConnection pLocation pBandwidth pConnectionName =
    CreateConnection'
    { _ccrqLocation = pLocation
    , _ccrqBandwidth = pBandwidth
    , _ccrqConnectionName = pConnectionName
    }

-- | FIXME: Undocumented member.
ccrqLocation :: Lens' CreateConnection Text
ccrqLocation = lens _ccrqLocation (\ s a -> s{_ccrqLocation = a});

-- | FIXME: Undocumented member.
ccrqBandwidth :: Lens' CreateConnection Text
ccrqBandwidth = lens _ccrqBandwidth (\ s a -> s{_ccrqBandwidth = a});

-- | FIXME: Undocumented member.
ccrqConnectionName :: Lens' CreateConnection Text
ccrqConnectionName = lens _ccrqConnectionName (\ s a -> s{_ccrqConnectionName = a});

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
              ["location" .= _ccrqLocation,
               "bandwidth" .= _ccrqBandwidth,
               "connectionName" .= _ccrqConnectionName]

instance ToPath CreateConnection where
        toPath = const "/"

instance ToQuery CreateConnection where
        toQuery = const mempty

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
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

-- | Creates a hosted connection on an interconnect.
--
-- Allocates a VLAN number and a specified amount of bandwidth for use by a
-- hosted connection on the given interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocateConnectionOnInterconnect.html>
module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
    (
    -- * Request
      AllocateConnectionOnInterconnect
    -- ** Request constructor
    , allocateConnectionOnInterconnect
    -- ** Request lenses
    , acoiBandwidth
    , acoiConnectionName
    , acoiOwnerAccount
    , acoiInterconnectId
    , acoiVlan

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

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateConnectionOnInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoiBandwidth'
--
-- * 'acoiConnectionName'
--
-- * 'acoiOwnerAccount'
--
-- * 'acoiInterconnectId'
--
-- * 'acoiVlan'
data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect'{_acoiBandwidth :: Text, _acoiConnectionName :: Text, _acoiOwnerAccount :: Text, _acoiInterconnectId :: Text, _acoiVlan :: Int} deriving (Eq, Read, Show)

-- | 'AllocateConnectionOnInterconnect' smart constructor.
allocateConnectionOnInterconnect :: Text -> Text -> Text -> Text -> Int -> AllocateConnectionOnInterconnect
allocateConnectionOnInterconnect pBandwidth pConnectionName pOwnerAccount pInterconnectId pVlan = AllocateConnectionOnInterconnect'{_acoiBandwidth = pBandwidth, _acoiConnectionName = pConnectionName, _acoiOwnerAccount = pOwnerAccount, _acoiInterconnectId = pInterconnectId, _acoiVlan = pVlan};

-- | Bandwidth of the connection.
--
-- Example: \"/500Mbps/\"
--
-- Default: None
acoiBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoiBandwidth = lens _acoiBandwidth (\ s a -> s{_acoiBandwidth = a});

-- | Name of the provisioned connection.
--
-- Example: \"/500M Connection to AWS/\"
--
-- Default: None
acoiConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoiConnectionName = lens _acoiConnectionName (\ s a -> s{_acoiConnectionName = a});

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned.
--
-- Example: 123443215678
--
-- Default: None
acoiOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoiOwnerAccount = lens _acoiOwnerAccount (\ s a -> s{_acoiOwnerAccount = a});

-- | ID of the interconnect on which the connection will be provisioned.
--
-- Example: dxcon-456abc78
--
-- Default: None
acoiInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoiInterconnectId = lens _acoiInterconnectId (\ s a -> s{_acoiInterconnectId = a});

-- | The dedicated VLAN provisioned to the connection.
--
-- Example: 101
--
-- Default: None
acoiVlan :: Lens' AllocateConnectionOnInterconnect Int
acoiVlan = lens _acoiVlan (\ s a -> s{_acoiVlan = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest AllocateConnectionOnInterconnect
         where
        type Sv AllocateConnectionOnInterconnect =
             DirectConnect
        type Rs AllocateConnectionOnInterconnect = Connection
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders AllocateConnectionOnInterconnect
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AllocateConnectionOnInterconnect"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AllocateConnectionOnInterconnect
         where
        toJSON AllocateConnectionOnInterconnect'{..}
          = object
              ["bandwidth" .= _acoiBandwidth,
               "connectionName" .= _acoiConnectionName,
               "ownerAccount" .= _acoiOwnerAccount,
               "interconnectId" .= _acoiInterconnectId,
               "vlan" .= _acoiVlan]

instance ToPath AllocateConnectionOnInterconnect
         where
        toPath = const "/"

instance ToQuery AllocateConnectionOnInterconnect
         where
        toQuery = const mempty

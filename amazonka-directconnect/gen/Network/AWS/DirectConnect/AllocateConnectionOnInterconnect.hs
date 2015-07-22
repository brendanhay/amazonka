{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a hosted connection on an interconnect.
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
    , acoirqBandwidth
    , acoirqConnectionName
    , acoirqOwnerAccount
    , acoirqInterconnectId
    , acoirqVlan

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

-- | Container for the parameters to the AllocateConnectionOnInterconnect
-- operation.
--
-- /See:/ 'allocateConnectionOnInterconnect' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acoirqBandwidth'
--
-- * 'acoirqConnectionName'
--
-- * 'acoirqOwnerAccount'
--
-- * 'acoirqInterconnectId'
--
-- * 'acoirqVlan'
data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect'
    { _acoirqBandwidth      :: !Text
    , _acoirqConnectionName :: !Text
    , _acoirqOwnerAccount   :: !Text
    , _acoirqInterconnectId :: !Text
    , _acoirqVlan           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllocateConnectionOnInterconnect' smart constructor.
allocateConnectionOnInterconnect :: Text -> Text -> Text -> Text -> Int -> AllocateConnectionOnInterconnect
allocateConnectionOnInterconnect pBandwidth pConnectionName pOwnerAccount pInterconnectId pVlan =
    AllocateConnectionOnInterconnect'
    { _acoirqBandwidth = pBandwidth
    , _acoirqConnectionName = pConnectionName
    , _acoirqOwnerAccount = pOwnerAccount
    , _acoirqInterconnectId = pInterconnectId
    , _acoirqVlan = pVlan
    }

-- | Bandwidth of the connection.
--
-- Example: \"/500Mbps/\"
--
-- Default: None
acoirqBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoirqBandwidth = lens _acoirqBandwidth (\ s a -> s{_acoirqBandwidth = a});

-- | Name of the provisioned connection.
--
-- Example: \"/500M Connection to AWS/\"
--
-- Default: None
acoirqConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoirqConnectionName = lens _acoirqConnectionName (\ s a -> s{_acoirqConnectionName = a});

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned.
--
-- Example: 123443215678
--
-- Default: None
acoirqOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoirqOwnerAccount = lens _acoirqOwnerAccount (\ s a -> s{_acoirqOwnerAccount = a});

-- | ID of the interconnect on which the connection will be provisioned.
--
-- Example: dxcon-456abc78
--
-- Default: None
acoirqInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoirqInterconnectId = lens _acoirqInterconnectId (\ s a -> s{_acoirqInterconnectId = a});

-- | The dedicated VLAN provisioned to the connection.
--
-- Example: 101
--
-- Default: None
acoirqVlan :: Lens' AllocateConnectionOnInterconnect Int
acoirqVlan = lens _acoirqVlan (\ s a -> s{_acoirqVlan = a});

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
              ["bandwidth" .= _acoirqBandwidth,
               "connectionName" .= _acoirqConnectionName,
               "ownerAccount" .= _acoirqOwnerAccount,
               "interconnectId" .= _acoirqInterconnectId,
               "vlan" .= _acoirqVlan]

instance ToPath AllocateConnectionOnInterconnect
         where
        toPath = const "/"

instance ToQuery AllocateConnectionOnInterconnect
         where
        toQuery = const mempty

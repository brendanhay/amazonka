{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated in favor of 'AllocateHostedConnection' .
--
--
-- Creates a hosted connection on an interconnect.
--
-- Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect.
--
module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
    (
    -- * Creating a Request
      allocateConnectionOnInterconnect
    , AllocateConnectionOnInterconnect
    -- * Request Lenses
    , acoiBandwidth
    , acoiConnectionName
    , acoiOwnerAccount
    , acoiInterconnectId
    , acoiVlan

    -- * Destructuring the Response
    , connection
    , Connection
    -- * Response Lenses
    , cLagId
    , cVlan
    , cLocation
    , cAwsDevice
    , cConnectionId
    , cLoaIssueTime
    , cPartnerName
    , cConnectionName
    , cBandwidth
    , cOwnerAccount
    , cRegion
    , cConnectionState
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the AllocateConnectionOnInterconnect operation.
--
--
--
-- /See:/ 'allocateConnectionOnInterconnect' smart constructor.
data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect'
    { _acoiBandwidth      :: !Text
    , _acoiConnectionName :: !Text
    , _acoiOwnerAccount   :: !Text
    , _acoiInterconnectId :: !Text
    , _acoiVlan           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateConnectionOnInterconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acoiBandwidth' - Bandwidth of the connection. Example: "/500Mbps/ " Default: None Values: 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, or 500Mbps
--
-- * 'acoiConnectionName' - Name of the provisioned connection. Example: "/500M Connection to AWS/ " Default: None
--
-- * 'acoiOwnerAccount' - Numeric account Id of the customer for whom the connection will be provisioned. Example: 123443215678 Default: None
--
-- * 'acoiInterconnectId' - ID of the interconnect on which the connection will be provisioned. Example: dxcon-456abc78 Default: None
--
-- * 'acoiVlan' - The dedicated VLAN provisioned to the connection. Example: 101 Default: None
allocateConnectionOnInterconnect
    :: Text -- ^ 'acoiBandwidth'
    -> Text -- ^ 'acoiConnectionName'
    -> Text -- ^ 'acoiOwnerAccount'
    -> Text -- ^ 'acoiInterconnectId'
    -> Int -- ^ 'acoiVlan'
    -> AllocateConnectionOnInterconnect
allocateConnectionOnInterconnect pBandwidth_ pConnectionName_ pOwnerAccount_ pInterconnectId_ pVlan_ =
    AllocateConnectionOnInterconnect'
    { _acoiBandwidth = pBandwidth_
    , _acoiConnectionName = pConnectionName_
    , _acoiOwnerAccount = pOwnerAccount_
    , _acoiInterconnectId = pInterconnectId_
    , _acoiVlan = pVlan_
    }

-- | Bandwidth of the connection. Example: "/500Mbps/ " Default: None Values: 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, or 500Mbps
acoiBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoiBandwidth = lens _acoiBandwidth (\ s a -> s{_acoiBandwidth = a});

-- | Name of the provisioned connection. Example: "/500M Connection to AWS/ " Default: None
acoiConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoiConnectionName = lens _acoiConnectionName (\ s a -> s{_acoiConnectionName = a});

-- | Numeric account Id of the customer for whom the connection will be provisioned. Example: 123443215678 Default: None
acoiOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoiOwnerAccount = lens _acoiOwnerAccount (\ s a -> s{_acoiOwnerAccount = a});

-- | ID of the interconnect on which the connection will be provisioned. Example: dxcon-456abc78 Default: None
acoiInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoiInterconnectId = lens _acoiInterconnectId (\ s a -> s{_acoiInterconnectId = a});

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default: None
acoiVlan :: Lens' AllocateConnectionOnInterconnect Int
acoiVlan = lens _acoiVlan (\ s a -> s{_acoiVlan = a});

instance AWSRequest AllocateConnectionOnInterconnect
         where
        type Rs AllocateConnectionOnInterconnect = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable AllocateConnectionOnInterconnect

instance NFData AllocateConnectionOnInterconnect

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
              (catMaybes
                 [Just ("bandwidth" .= _acoiBandwidth),
                  Just ("connectionName" .= _acoiConnectionName),
                  Just ("ownerAccount" .= _acoiOwnerAccount),
                  Just ("interconnectId" .= _acoiInterconnectId),
                  Just ("vlan" .= _acoiVlan)])

instance ToPath AllocateConnectionOnInterconnect
         where
        toPath = const "/"

instance ToQuery AllocateConnectionOnInterconnect
         where
        toQuery = const mempty

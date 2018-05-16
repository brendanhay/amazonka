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
-- Module      : Network.AWS.DirectConnect.AllocateHostedConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a hosted connection on an interconnect or a link aggregation group (LAG).
--
--
-- Allocates a VLAN number and a specified amount of bandwidth for use by a hosted connection on the given interconnect or LAG.
--
module Network.AWS.DirectConnect.AllocateHostedConnection
    (
    -- * Creating a Request
      allocateHostedConnection
    , AllocateHostedConnection
    -- * Request Lenses
    , ahcConnectionId
    , ahcOwnerAccount
    , ahcBandwidth
    , ahcConnectionName
    , ahcVlan

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

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to theHostedConnection operation.
--
--
--
-- /See:/ 'allocateHostedConnection' smart constructor.
data AllocateHostedConnection = AllocateHostedConnection'
  { _ahcConnectionId   :: !Text
  , _ahcOwnerAccount   :: !Text
  , _ahcBandwidth      :: !Text
  , _ahcConnectionName :: !Text
  , _ahcVlan           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateHostedConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahcConnectionId' - The ID of the interconnect or LAG on which the connection will be provisioned. Example: dxcon-456abc78 or dxlag-abc123 Default: None
--
-- * 'ahcOwnerAccount' - The numeric account ID of the customer for whom the connection will be provisioned. Example: 123443215678 Default: None
--
-- * 'ahcBandwidth' - The bandwidth of the connection. Example: @500Mbps@  Default: None Values: 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, or 500Mbps
--
-- * 'ahcConnectionName' - The name of the provisioned connection. Example: "@500M Connection to AWS@ " Default: None
--
-- * 'ahcVlan' - The dedicated VLAN provisioned to the hosted connection. Example: 101 Default: None
allocateHostedConnection
    :: Text -- ^ 'ahcConnectionId'
    -> Text -- ^ 'ahcOwnerAccount'
    -> Text -- ^ 'ahcBandwidth'
    -> Text -- ^ 'ahcConnectionName'
    -> Int -- ^ 'ahcVlan'
    -> AllocateHostedConnection
allocateHostedConnection pConnectionId_ pOwnerAccount_ pBandwidth_ pConnectionName_ pVlan_ =
  AllocateHostedConnection'
    { _ahcConnectionId = pConnectionId_
    , _ahcOwnerAccount = pOwnerAccount_
    , _ahcBandwidth = pBandwidth_
    , _ahcConnectionName = pConnectionName_
    , _ahcVlan = pVlan_
    }


-- | The ID of the interconnect or LAG on which the connection will be provisioned. Example: dxcon-456abc78 or dxlag-abc123 Default: None
ahcConnectionId :: Lens' AllocateHostedConnection Text
ahcConnectionId = lens _ahcConnectionId (\ s a -> s{_ahcConnectionId = a})

-- | The numeric account ID of the customer for whom the connection will be provisioned. Example: 123443215678 Default: None
ahcOwnerAccount :: Lens' AllocateHostedConnection Text
ahcOwnerAccount = lens _ahcOwnerAccount (\ s a -> s{_ahcOwnerAccount = a})

-- | The bandwidth of the connection. Example: @500Mbps@  Default: None Values: 50Mbps, 100Mbps, 200Mbps, 300Mbps, 400Mbps, or 500Mbps
ahcBandwidth :: Lens' AllocateHostedConnection Text
ahcBandwidth = lens _ahcBandwidth (\ s a -> s{_ahcBandwidth = a})

-- | The name of the provisioned connection. Example: "@500M Connection to AWS@ " Default: None
ahcConnectionName :: Lens' AllocateHostedConnection Text
ahcConnectionName = lens _ahcConnectionName (\ s a -> s{_ahcConnectionName = a})

-- | The dedicated VLAN provisioned to the hosted connection. Example: 101 Default: None
ahcVlan :: Lens' AllocateHostedConnection Int
ahcVlan = lens _ahcVlan (\ s a -> s{_ahcVlan = a})

instance AWSRequest AllocateHostedConnection where
        type Rs AllocateHostedConnection = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable AllocateHostedConnection where

instance NFData AllocateHostedConnection where

instance ToHeaders AllocateHostedConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AllocateHostedConnection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AllocateHostedConnection where
        toJSON AllocateHostedConnection'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _ahcConnectionId),
                  Just ("ownerAccount" .= _ahcOwnerAccount),
                  Just ("bandwidth" .= _ahcBandwidth),
                  Just ("connectionName" .= _ahcConnectionName),
                  Just ("vlan" .= _ahcVlan)])

instance ToPath AllocateHostedConnection where
        toPath = const "/"

instance ToQuery AllocateHostedConnection where
        toQuery = const mempty

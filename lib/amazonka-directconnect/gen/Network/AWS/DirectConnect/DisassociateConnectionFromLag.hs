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
-- Module      : Network.AWS.DirectConnect.DisassociateConnectionFromLag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection from a link aggregation group (LAG). The connection is interrupted and re-established as a standalone connection (the connection is not deleted; to delete the connection, use the 'DeleteConnection' request). If the LAG has associated virtual interfaces or hosted connections, they remain associated with the LAG. A disassociated connection owned by an AWS Direct Connect partner is automatically converted to an interconnect.
--
--
-- If disassociating the connection will cause the LAG to fall below its setting for minimum number of operational connections, the request fails, except when it's the last member of the LAG. If all connections are disassociated, the LAG continues to exist as an empty LAG with no physical connections.
--
module Network.AWS.DirectConnect.DisassociateConnectionFromLag
    (
    -- * Creating a Request
      disassociateConnectionFromLag
    , DisassociateConnectionFromLag
    -- * Request Lenses
    , dcflConnectionId
    , dcflLagId

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

-- | Container for the parameters to the DisassociateConnectionFromLag operation.
--
--
--
-- /See:/ 'disassociateConnectionFromLag' smart constructor.
data DisassociateConnectionFromLag = DisassociateConnectionFromLag'
  { _dcflConnectionId :: !Text
  , _dcflLagId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateConnectionFromLag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcflConnectionId' - The ID of the connection to disassociate from the LAG. Example: dxcon-abc123 Default: None
--
-- * 'dcflLagId' - The ID of the LAG. Example: dxlag-abc123 Default: None
disassociateConnectionFromLag
    :: Text -- ^ 'dcflConnectionId'
    -> Text -- ^ 'dcflLagId'
    -> DisassociateConnectionFromLag
disassociateConnectionFromLag pConnectionId_ pLagId_ =
  DisassociateConnectionFromLag'
    {_dcflConnectionId = pConnectionId_, _dcflLagId = pLagId_}


-- | The ID of the connection to disassociate from the LAG. Example: dxcon-abc123 Default: None
dcflConnectionId :: Lens' DisassociateConnectionFromLag Text
dcflConnectionId = lens _dcflConnectionId (\ s a -> s{_dcflConnectionId = a})

-- | The ID of the LAG. Example: dxlag-abc123 Default: None
dcflLagId :: Lens' DisassociateConnectionFromLag Text
dcflLagId = lens _dcflLagId (\ s a -> s{_dcflLagId = a})

instance AWSRequest DisassociateConnectionFromLag
         where
        type Rs DisassociateConnectionFromLag = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DisassociateConnectionFromLag where

instance NFData DisassociateConnectionFromLag where

instance ToHeaders DisassociateConnectionFromLag
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DisassociateConnectionFromLag" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateConnectionFromLag where
        toJSON DisassociateConnectionFromLag'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _dcflConnectionId),
                  Just ("lagId" .= _dcflLagId)])

instance ToPath DisassociateConnectionFromLag where
        toPath = const "/"

instance ToQuery DisassociateConnectionFromLag where
        toQuery = const mempty

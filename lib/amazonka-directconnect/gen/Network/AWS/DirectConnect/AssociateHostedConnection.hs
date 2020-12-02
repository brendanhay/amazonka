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
-- Module      : Network.AWS.DirectConnect.AssociateHostedConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a hosted connection and its virtual interfaces with a link aggregation group (LAG) or interconnect. If the target interconnect or LAG has an existing hosted connection with a conflicting VLAN number or IP address, the operation fails. This action temporarily interrupts the hosted connection's connectivity to AWS as it is being migrated.
--
--
module Network.AWS.DirectConnect.AssociateHostedConnection
    (
    -- * Creating a Request
      associateHostedConnection
    , AssociateHostedConnection
    -- * Request Lenses
    , assConnectionId
    , assParentConnectionId

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

-- | Container for the parameters to the AssociateHostedConnection operation.
--
--
--
-- /See:/ 'associateHostedConnection' smart constructor.
data AssociateHostedConnection = AssociateHostedConnection'
  { _assConnectionId       :: !Text
  , _assParentConnectionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateHostedConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assConnectionId' - The ID of the hosted connection. Example: dxcon-abc123 Default: None
--
-- * 'assParentConnectionId' - The ID of the interconnect or the LAG. Example: dxcon-abc123 or dxlag-abc123 Default: None
associateHostedConnection
    :: Text -- ^ 'assConnectionId'
    -> Text -- ^ 'assParentConnectionId'
    -> AssociateHostedConnection
associateHostedConnection pConnectionId_ pParentConnectionId_ =
  AssociateHostedConnection'
    { _assConnectionId = pConnectionId_
    , _assParentConnectionId = pParentConnectionId_
    }


-- | The ID of the hosted connection. Example: dxcon-abc123 Default: None
assConnectionId :: Lens' AssociateHostedConnection Text
assConnectionId = lens _assConnectionId (\ s a -> s{_assConnectionId = a})

-- | The ID of the interconnect or the LAG. Example: dxcon-abc123 or dxlag-abc123 Default: None
assParentConnectionId :: Lens' AssociateHostedConnection Text
assParentConnectionId = lens _assParentConnectionId (\ s a -> s{_assParentConnectionId = a})

instance AWSRequest AssociateHostedConnection where
        type Rs AssociateHostedConnection = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable AssociateHostedConnection where

instance NFData AssociateHostedConnection where

instance ToHeaders AssociateHostedConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AssociateHostedConnection" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateHostedConnection where
        toJSON AssociateHostedConnection'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _assConnectionId),
                  Just
                    ("parentConnectionId" .= _assParentConnectionId)])

instance ToPath AssociateHostedConnection where
        toPath = const "/"

instance ToQuery AssociateHostedConnection where
        toQuery = const mempty

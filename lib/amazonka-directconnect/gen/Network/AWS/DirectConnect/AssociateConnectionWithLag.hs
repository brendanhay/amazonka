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
-- Module      : Network.AWS.DirectConnect.AssociateConnectionWithLag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing connection with a link aggregation group (LAG). The connection is interrupted and re-established as a member of the LAG (connectivity to AWS will be interrupted). The connection must be hosted on the same AWS Direct Connect endpoint as the LAG, and its bandwidth must match the bandwidth for the LAG. You can reassociate a connection that's currently associated with a different LAG; however, if removing the connection will cause the original LAG to fall below its setting for minimum number of operational connections, the request fails.
--
--
-- Any virtual interfaces that are directly associated with the connection are automatically re-associated with the LAG. If the connection was originally associated with a different LAG, the virtual interfaces remain associated with the original LAG.
--
-- For interconnects, any hosted connections are automatically re-associated with the LAG. If the interconnect was originally associated with a different LAG, the hosted connections remain associated with the original LAG.
--
module Network.AWS.DirectConnect.AssociateConnectionWithLag
    (
    -- * Creating a Request
      associateConnectionWithLag
    , AssociateConnectionWithLag
    -- * Request Lenses
    , acwlConnectionId
    , acwlLagId

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

-- | Container for the parameters to the AssociateConnectionWithLag operation.
--
--
--
-- /See:/ 'associateConnectionWithLag' smart constructor.
data AssociateConnectionWithLag = AssociateConnectionWithLag'
  { _acwlConnectionId :: !Text
  , _acwlLagId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateConnectionWithLag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acwlConnectionId' - The ID of the connection. Example: dxcon-abc123 Default: None
--
-- * 'acwlLagId' - The ID of the LAG with which to associate the connection. Example: dxlag-abc123 Default: None
associateConnectionWithLag
    :: Text -- ^ 'acwlConnectionId'
    -> Text -- ^ 'acwlLagId'
    -> AssociateConnectionWithLag
associateConnectionWithLag pConnectionId_ pLagId_ =
  AssociateConnectionWithLag'
    {_acwlConnectionId = pConnectionId_, _acwlLagId = pLagId_}


-- | The ID of the connection. Example: dxcon-abc123 Default: None
acwlConnectionId :: Lens' AssociateConnectionWithLag Text
acwlConnectionId = lens _acwlConnectionId (\ s a -> s{_acwlConnectionId = a})

-- | The ID of the LAG with which to associate the connection. Example: dxlag-abc123 Default: None
acwlLagId :: Lens' AssociateConnectionWithLag Text
acwlLagId = lens _acwlLagId (\ s a -> s{_acwlLagId = a})

instance AWSRequest AssociateConnectionWithLag where
        type Rs AssociateConnectionWithLag = Connection
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable AssociateConnectionWithLag where

instance NFData AssociateConnectionWithLag where

instance ToHeaders AssociateConnectionWithLag where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AssociateConnectionWithLag" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateConnectionWithLag where
        toJSON AssociateConnectionWithLag'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _acwlConnectionId),
                  Just ("lagId" .= _acwlLagId)])

instance ToPath AssociateConnectionWithLag where
        toPath = const "/"

instance ToQuery AssociateConnectionWithLag where
        toQuery = const mempty

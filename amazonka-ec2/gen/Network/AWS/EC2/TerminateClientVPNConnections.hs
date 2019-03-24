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
-- Module      : Network.AWS.EC2.TerminateClientVPNConnections
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates active Client VPN endpoint connections. This action can be used to terminate a specific client connection, or up to five connections established by a specific user.
--
--
module Network.AWS.EC2.TerminateClientVPNConnections
    (
    -- * Creating a Request
      terminateClientVPNConnections
    , TerminateClientVPNConnections
    -- * Request Lenses
    , tcvcConnectionId
    , tcvcUsername
    , tcvcDryRun
    , tcvcClientVPNEndpointId

    -- * Destructuring the Response
    , terminateClientVPNConnectionsResponse
    , TerminateClientVPNConnectionsResponse
    -- * Response Lenses
    , tcvcrsConnectionStatuses
    , tcvcrsUsername
    , tcvcrsClientVPNEndpointId
    , tcvcrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'terminateClientVPNConnections' smart constructor.
data TerminateClientVPNConnections = TerminateClientVPNConnections'
  { _tcvcConnectionId        :: !(Maybe Text)
  , _tcvcUsername            :: !(Maybe Text)
  , _tcvcDryRun              :: !(Maybe Bool)
  , _tcvcClientVPNEndpointId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateClientVPNConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcvcConnectionId' - The ID of the client connection to be terminated.
--
-- * 'tcvcUsername' - The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
--
-- * 'tcvcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'tcvcClientVPNEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
terminateClientVPNConnections
    :: Text -- ^ 'tcvcClientVPNEndpointId'
    -> TerminateClientVPNConnections
terminateClientVPNConnections pClientVPNEndpointId_ =
  TerminateClientVPNConnections'
    { _tcvcConnectionId = Nothing
    , _tcvcUsername = Nothing
    , _tcvcDryRun = Nothing
    , _tcvcClientVPNEndpointId = pClientVPNEndpointId_
    }


-- | The ID of the client connection to be terminated.
tcvcConnectionId :: Lens' TerminateClientVPNConnections (Maybe Text)
tcvcConnectionId = lens _tcvcConnectionId (\ s a -> s{_tcvcConnectionId = a})

-- | The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
tcvcUsername :: Lens' TerminateClientVPNConnections (Maybe Text)
tcvcUsername = lens _tcvcUsername (\ s a -> s{_tcvcUsername = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
tcvcDryRun :: Lens' TerminateClientVPNConnections (Maybe Bool)
tcvcDryRun = lens _tcvcDryRun (\ s a -> s{_tcvcDryRun = a})

-- | The ID of the Client VPN endpoint to which the client is connected.
tcvcClientVPNEndpointId :: Lens' TerminateClientVPNConnections Text
tcvcClientVPNEndpointId = lens _tcvcClientVPNEndpointId (\ s a -> s{_tcvcClientVPNEndpointId = a})

instance AWSRequest TerminateClientVPNConnections
         where
        type Rs TerminateClientVPNConnections =
             TerminateClientVPNConnectionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 TerminateClientVPNConnectionsResponse' <$>
                   (x .@? "connectionStatuses" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "username")
                     <*> (x .@? "clientVpnEndpointId")
                     <*> (pure (fromEnum s)))

instance Hashable TerminateClientVPNConnections where

instance NFData TerminateClientVPNConnections where

instance ToHeaders TerminateClientVPNConnections
         where
        toHeaders = const mempty

instance ToPath TerminateClientVPNConnections where
        toPath = const "/"

instance ToQuery TerminateClientVPNConnections where
        toQuery TerminateClientVPNConnections'{..}
          = mconcat
              ["Action" =:
                 ("TerminateClientVpnConnections" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ConnectionId" =: _tcvcConnectionId,
               "Username" =: _tcvcUsername, "DryRun" =: _tcvcDryRun,
               "ClientVpnEndpointId" =: _tcvcClientVPNEndpointId]

-- | /See:/ 'terminateClientVPNConnectionsResponse' smart constructor.
data TerminateClientVPNConnectionsResponse = TerminateClientVPNConnectionsResponse'
  { _tcvcrsConnectionStatuses  :: !(Maybe [TerminateConnectionStatus])
  , _tcvcrsUsername            :: !(Maybe Text)
  , _tcvcrsClientVPNEndpointId :: !(Maybe Text)
  , _tcvcrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateClientVPNConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcvcrsConnectionStatuses' - The current state of the client connections.
--
-- * 'tcvcrsUsername' - The user who established the terminated client connections.
--
-- * 'tcvcrsClientVPNEndpointId' - The ID of the Client VPN endpoint.
--
-- * 'tcvcrsResponseStatus' - -- | The response status code.
terminateClientVPNConnectionsResponse
    :: Int -- ^ 'tcvcrsResponseStatus'
    -> TerminateClientVPNConnectionsResponse
terminateClientVPNConnectionsResponse pResponseStatus_ =
  TerminateClientVPNConnectionsResponse'
    { _tcvcrsConnectionStatuses = Nothing
    , _tcvcrsUsername = Nothing
    , _tcvcrsClientVPNEndpointId = Nothing
    , _tcvcrsResponseStatus = pResponseStatus_
    }


-- | The current state of the client connections.
tcvcrsConnectionStatuses :: Lens' TerminateClientVPNConnectionsResponse [TerminateConnectionStatus]
tcvcrsConnectionStatuses = lens _tcvcrsConnectionStatuses (\ s a -> s{_tcvcrsConnectionStatuses = a}) . _Default . _Coerce

-- | The user who established the terminated client connections.
tcvcrsUsername :: Lens' TerminateClientVPNConnectionsResponse (Maybe Text)
tcvcrsUsername = lens _tcvcrsUsername (\ s a -> s{_tcvcrsUsername = a})

-- | The ID of the Client VPN endpoint.
tcvcrsClientVPNEndpointId :: Lens' TerminateClientVPNConnectionsResponse (Maybe Text)
tcvcrsClientVPNEndpointId = lens _tcvcrsClientVPNEndpointId (\ s a -> s{_tcvcrsClientVPNEndpointId = a})

-- | -- | The response status code.
tcvcrsResponseStatus :: Lens' TerminateClientVPNConnectionsResponse Int
tcvcrsResponseStatus = lens _tcvcrsResponseStatus (\ s a -> s{_tcvcrsResponseStatus = a})

instance NFData TerminateClientVPNConnectionsResponse
         where

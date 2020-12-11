{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.TerminateClientVPNConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates active Client VPN endpoint connections. This action can be used to terminate a specific client connection, or up to five connections established by a specific user.
module Network.AWS.EC2.TerminateClientVPNConnections
  ( -- * Creating a request
    TerminateClientVPNConnections (..),
    mkTerminateClientVPNConnections,

    -- ** Request lenses
    tcvcConnectionId,
    tcvcUsername,
    tcvcDryRun,
    tcvcClientVPNEndpointId,

    -- * Destructuring the response
    TerminateClientVPNConnectionsResponse (..),
    mkTerminateClientVPNConnectionsResponse,

    -- ** Response lenses
    tcvcrsConnectionStatuses,
    tcvcrsUsername,
    tcvcrsClientVPNEndpointId,
    tcvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTerminateClientVPNConnections' smart constructor.
data TerminateClientVPNConnections = TerminateClientVPNConnections'
  { connectionId ::
      Lude.Maybe Lude.Text,
    username ::
      Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    clientVPNEndpointId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateClientVPNConnections' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
-- * 'connectionId' - The ID of the client connection to be terminated.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'username' - The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
mkTerminateClientVPNConnections ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  TerminateClientVPNConnections
mkTerminateClientVPNConnections pClientVPNEndpointId_ =
  TerminateClientVPNConnections'
    { connectionId = Lude.Nothing,
      username = Lude.Nothing,
      dryRun = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | The ID of the client connection to be terminated.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcConnectionId :: Lens.Lens' TerminateClientVPNConnections (Lude.Maybe Lude.Text)
tcvcConnectionId = Lens.lens (connectionId :: TerminateClientVPNConnections -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: TerminateClientVPNConnections)
{-# DEPRECATED tcvcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The name of the user who initiated the connection. Use this option to terminate all active connections for the specified user. This option can only be used if the user has established up to five connections.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcUsername :: Lens.Lens' TerminateClientVPNConnections (Lude.Maybe Lude.Text)
tcvcUsername = Lens.lens (username :: TerminateClientVPNConnections -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: TerminateClientVPNConnections)
{-# DEPRECATED tcvcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcDryRun :: Lens.Lens' TerminateClientVPNConnections (Lude.Maybe Lude.Bool)
tcvcDryRun = Lens.lens (dryRun :: TerminateClientVPNConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: TerminateClientVPNConnections)
{-# DEPRECATED tcvcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint to which the client is connected.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcClientVPNEndpointId :: Lens.Lens' TerminateClientVPNConnections Lude.Text
tcvcClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: TerminateClientVPNConnections -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: TerminateClientVPNConnections)
{-# DEPRECATED tcvcClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Lude.AWSRequest TerminateClientVPNConnections where
  type
    Rs TerminateClientVPNConnections =
      TerminateClientVPNConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          TerminateClientVPNConnectionsResponse'
            Lude.<$> ( x Lude..@? "connectionStatuses" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "username")
            Lude.<*> (x Lude..@? "clientVpnEndpointId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateClientVPNConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TerminateClientVPNConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateClientVPNConnections where
  toQuery TerminateClientVPNConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("TerminateClientVpnConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ConnectionId" Lude.=: connectionId,
        "Username" Lude.=: username,
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkTerminateClientVPNConnectionsResponse' smart constructor.
data TerminateClientVPNConnectionsResponse = TerminateClientVPNConnectionsResponse'
  { connectionStatuses ::
      Lude.Maybe
        [TerminateConnectionStatus],
    username ::
      Lude.Maybe
        Lude.Text,
    clientVPNEndpointId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateClientVPNConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'connectionStatuses' - The current state of the client connections.
-- * 'responseStatus' - The response status code.
-- * 'username' - The user who established the terminated client connections.
mkTerminateClientVPNConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateClientVPNConnectionsResponse
mkTerminateClientVPNConnectionsResponse pResponseStatus_ =
  TerminateClientVPNConnectionsResponse'
    { connectionStatuses =
        Lude.Nothing,
      username = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the client connections.
--
-- /Note:/ Consider using 'connectionStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrsConnectionStatuses :: Lens.Lens' TerminateClientVPNConnectionsResponse (Lude.Maybe [TerminateConnectionStatus])
tcvcrsConnectionStatuses = Lens.lens (connectionStatuses :: TerminateClientVPNConnectionsResponse -> Lude.Maybe [TerminateConnectionStatus]) (\s a -> s {connectionStatuses = a} :: TerminateClientVPNConnectionsResponse)
{-# DEPRECATED tcvcrsConnectionStatuses "Use generic-lens or generic-optics with 'connectionStatuses' instead." #-}

-- | The user who established the terminated client connections.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrsUsername :: Lens.Lens' TerminateClientVPNConnectionsResponse (Lude.Maybe Lude.Text)
tcvcrsUsername = Lens.lens (username :: TerminateClientVPNConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: TerminateClientVPNConnectionsResponse)
{-# DEPRECATED tcvcrsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrsClientVPNEndpointId :: Lens.Lens' TerminateClientVPNConnectionsResponse (Lude.Maybe Lude.Text)
tcvcrsClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: TerminateClientVPNConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: TerminateClientVPNConnectionsResponse)
{-# DEPRECATED tcvcrsClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcvcrsResponseStatus :: Lens.Lens' TerminateClientVPNConnectionsResponse Lude.Int
tcvcrsResponseStatus = Lens.lens (responseStatus :: TerminateClientVPNConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateClientVPNConnectionsResponse)
{-# DEPRECATED tcvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

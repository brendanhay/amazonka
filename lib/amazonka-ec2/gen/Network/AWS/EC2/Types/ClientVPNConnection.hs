{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNConnection
  ( ClientVPNConnection (..),

    -- * Smart constructor
    mkClientVPNConnection,

    -- * Lenses
    cvcIngressPackets,
    cvcStatus,
    cvcConnectionEndTime,
    cvcCommonName,
    cvcPostureComplianceStatuses,
    cvcConnectionEstablishedTime,
    cvcConnectionId,
    cvcIngressBytes,
    cvcUsername,
    cvcEgressBytes,
    cvcClientVPNEndpointId,
    cvcClientIP,
    cvcEgressPackets,
    cvcTimestamp,
  )
where

import Network.AWS.EC2.Types.ClientVPNConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a client connection.
--
-- /See:/ 'mkClientVPNConnection' smart constructor.
data ClientVPNConnection = ClientVPNConnection'
  { ingressPackets ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe ClientVPNConnectionStatus,
    connectionEndTime :: Lude.Maybe Lude.Text,
    commonName :: Lude.Maybe Lude.Text,
    postureComplianceStatuses :: Lude.Maybe [Lude.Text],
    connectionEstablishedTime :: Lude.Maybe Lude.Text,
    connectionId :: Lude.Maybe Lude.Text,
    ingressBytes :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    egressBytes :: Lude.Maybe Lude.Text,
    clientVPNEndpointId :: Lude.Maybe Lude.Text,
    clientIP :: Lude.Maybe Lude.Text,
    egressPackets :: Lude.Maybe Lude.Text,
    timestamp :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNConnection' with the minimum fields required to make a request.
--
-- * 'clientIP' - The IP address of the client.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
-- * 'commonName' - The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
-- * 'connectionEndTime' - The date and time the client connection was terminated.
-- * 'connectionEstablishedTime' - The date and time the client connection was established.
-- * 'connectionId' - The ID of the client connection.
-- * 'egressBytes' - The number of bytes received by the client.
-- * 'egressPackets' - The number of packets received by the client.
-- * 'ingressBytes' - The number of bytes sent by the client.
-- * 'ingressPackets' - The number of packets sent by the client.
-- * 'postureComplianceStatuses' - The statuses returned by the client connect handler for posture compliance, if applicable.
-- * 'status' - The current state of the client connection.
-- * 'timestamp' - The current date and time.
-- * 'username' - The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
mkClientVPNConnection ::
  ClientVPNConnection
mkClientVPNConnection =
  ClientVPNConnection'
    { ingressPackets = Lude.Nothing,
      status = Lude.Nothing,
      connectionEndTime = Lude.Nothing,
      commonName = Lude.Nothing,
      postureComplianceStatuses = Lude.Nothing,
      connectionEstablishedTime = Lude.Nothing,
      connectionId = Lude.Nothing,
      ingressBytes = Lude.Nothing,
      username = Lude.Nothing,
      egressBytes = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      clientIP = Lude.Nothing,
      egressPackets = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The number of packets sent by the client.
--
-- /Note:/ Consider using 'ingressPackets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcIngressPackets :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcIngressPackets = Lens.lens (ingressPackets :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {ingressPackets = a} :: ClientVPNConnection)
{-# DEPRECATED cvcIngressPackets "Use generic-lens or generic-optics with 'ingressPackets' instead." #-}

-- | The current state of the client connection.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcStatus :: Lens.Lens' ClientVPNConnection (Lude.Maybe ClientVPNConnectionStatus)
cvcStatus = Lens.lens (status :: ClientVPNConnection -> Lude.Maybe ClientVPNConnectionStatus) (\s a -> s {status = a} :: ClientVPNConnection)
{-# DEPRECATED cvcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the client connection was terminated.
--
-- /Note:/ Consider using 'connectionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionEndTime :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcConnectionEndTime = Lens.lens (connectionEndTime :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {connectionEndTime = a} :: ClientVPNConnection)
{-# DEPRECATED cvcConnectionEndTime "Use generic-lens or generic-optics with 'connectionEndTime' instead." #-}

-- | The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcCommonName :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcCommonName = Lens.lens (commonName :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {commonName = a} :: ClientVPNConnection)
{-# DEPRECATED cvcCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

-- | The statuses returned by the client connect handler for posture compliance, if applicable.
--
-- /Note:/ Consider using 'postureComplianceStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcPostureComplianceStatuses :: Lens.Lens' ClientVPNConnection (Lude.Maybe [Lude.Text])
cvcPostureComplianceStatuses = Lens.lens (postureComplianceStatuses :: ClientVPNConnection -> Lude.Maybe [Lude.Text]) (\s a -> s {postureComplianceStatuses = a} :: ClientVPNConnection)
{-# DEPRECATED cvcPostureComplianceStatuses "Use generic-lens or generic-optics with 'postureComplianceStatuses' instead." #-}

-- | The date and time the client connection was established.
--
-- /Note:/ Consider using 'connectionEstablishedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionEstablishedTime :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcConnectionEstablishedTime = Lens.lens (connectionEstablishedTime :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {connectionEstablishedTime = a} :: ClientVPNConnection)
{-# DEPRECATED cvcConnectionEstablishedTime "Use generic-lens or generic-optics with 'connectionEstablishedTime' instead." #-}

-- | The ID of the client connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionId :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcConnectionId = Lens.lens (connectionId :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: ClientVPNConnection)
{-# DEPRECATED cvcConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The number of bytes sent by the client.
--
-- /Note:/ Consider using 'ingressBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcIngressBytes :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcIngressBytes = Lens.lens (ingressBytes :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {ingressBytes = a} :: ClientVPNConnection)
{-# DEPRECATED cvcIngressBytes "Use generic-lens or generic-optics with 'ingressBytes' instead." #-}

-- | The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcUsername :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcUsername = Lens.lens (username :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: ClientVPNConnection)
{-# DEPRECATED cvcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The number of bytes received by the client.
--
-- /Note:/ Consider using 'egressBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcEgressBytes :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcEgressBytes = Lens.lens (egressBytes :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {egressBytes = a} :: ClientVPNConnection)
{-# DEPRECATED cvcEgressBytes "Use generic-lens or generic-optics with 'egressBytes' instead." #-}

-- | The ID of the Client VPN endpoint to which the client is connected.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcClientVPNEndpointId :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ClientVPNConnection)
{-# DEPRECATED cvcClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The IP address of the client.
--
-- /Note:/ Consider using 'clientIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcClientIP :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcClientIP = Lens.lens (clientIP :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {clientIP = a} :: ClientVPNConnection)
{-# DEPRECATED cvcClientIP "Use generic-lens or generic-optics with 'clientIP' instead." #-}

-- | The number of packets received by the client.
--
-- /Note:/ Consider using 'egressPackets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcEgressPackets :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcEgressPackets = Lens.lens (egressPackets :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {egressPackets = a} :: ClientVPNConnection)
{-# DEPRECATED cvcEgressPackets "Use generic-lens or generic-optics with 'egressPackets' instead." #-}

-- | The current date and time.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTimestamp :: Lens.Lens' ClientVPNConnection (Lude.Maybe Lude.Text)
cvcTimestamp = Lens.lens (timestamp :: ClientVPNConnection -> Lude.Maybe Lude.Text) (\s a -> s {timestamp = a} :: ClientVPNConnection)
{-# DEPRECATED cvcTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML ClientVPNConnection where
  parseXML x =
    ClientVPNConnection'
      Lude.<$> (x Lude..@? "ingressPackets")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "connectionEndTime")
      Lude.<*> (x Lude..@? "commonName")
      Lude.<*> ( x Lude..@? "postureComplianceStatusSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "connectionEstablishedTime")
      Lude.<*> (x Lude..@? "connectionId")
      Lude.<*> (x Lude..@? "ingressBytes")
      Lude.<*> (x Lude..@? "username")
      Lude.<*> (x Lude..@? "egressBytes")
      Lude.<*> (x Lude..@? "clientVpnEndpointId")
      Lude.<*> (x Lude..@? "clientIp")
      Lude.<*> (x Lude..@? "egressPackets")
      Lude.<*> (x Lude..@? "timestamp")

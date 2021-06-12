{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnConnection where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnConnectionStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a client connection.
--
-- /See:/ 'newClientVpnConnection' smart constructor.
data ClientVpnConnection = ClientVpnConnection'
  { -- | The ID of the Client VPN endpoint to which the client is connected.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the client connection.
    status :: Core.Maybe ClientVpnConnectionStatus,
    -- | The number of packets sent by the client.
    ingressPackets :: Core.Maybe Core.Text,
    -- | The number of bytes received by the client.
    egressBytes :: Core.Maybe Core.Text,
    -- | The ID of the client connection.
    connectionId :: Core.Maybe Core.Text,
    -- | The date and time the client connection was established.
    connectionEstablishedTime :: Core.Maybe Core.Text,
    -- | The statuses returned by the client connect handler for posture
    -- compliance, if applicable.
    postureComplianceStatuses :: Core.Maybe [Core.Text],
    -- | The common name associated with the client. This is either the name of
    -- the client certificate, or the Active Directory user name.
    commonName :: Core.Maybe Core.Text,
    -- | The date and time the client connection was terminated.
    connectionEndTime :: Core.Maybe Core.Text,
    -- | The number of bytes sent by the client.
    ingressBytes :: Core.Maybe Core.Text,
    -- | The number of packets received by the client.
    egressPackets :: Core.Maybe Core.Text,
    -- | The current date and time.
    timestamp :: Core.Maybe Core.Text,
    -- | The username of the client who established the client connection. This
    -- information is only provided if Active Directory client authentication
    -- is used.
    username :: Core.Maybe Core.Text,
    -- | The IP address of the client.
    clientIp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'clientVpnConnection_clientVpnEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
--
-- 'status', 'clientVpnConnection_status' - The current state of the client connection.
--
-- 'ingressPackets', 'clientVpnConnection_ingressPackets' - The number of packets sent by the client.
--
-- 'egressBytes', 'clientVpnConnection_egressBytes' - The number of bytes received by the client.
--
-- 'connectionId', 'clientVpnConnection_connectionId' - The ID of the client connection.
--
-- 'connectionEstablishedTime', 'clientVpnConnection_connectionEstablishedTime' - The date and time the client connection was established.
--
-- 'postureComplianceStatuses', 'clientVpnConnection_postureComplianceStatuses' - The statuses returned by the client connect handler for posture
-- compliance, if applicable.
--
-- 'commonName', 'clientVpnConnection_commonName' - The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
--
-- 'connectionEndTime', 'clientVpnConnection_connectionEndTime' - The date and time the client connection was terminated.
--
-- 'ingressBytes', 'clientVpnConnection_ingressBytes' - The number of bytes sent by the client.
--
-- 'egressPackets', 'clientVpnConnection_egressPackets' - The number of packets received by the client.
--
-- 'timestamp', 'clientVpnConnection_timestamp' - The current date and time.
--
-- 'username', 'clientVpnConnection_username' - The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
--
-- 'clientIp', 'clientVpnConnection_clientIp' - The IP address of the client.
newClientVpnConnection ::
  ClientVpnConnection
newClientVpnConnection =
  ClientVpnConnection'
    { clientVpnEndpointId =
        Core.Nothing,
      status = Core.Nothing,
      ingressPackets = Core.Nothing,
      egressBytes = Core.Nothing,
      connectionId = Core.Nothing,
      connectionEstablishedTime = Core.Nothing,
      postureComplianceStatuses = Core.Nothing,
      commonName = Core.Nothing,
      connectionEndTime = Core.Nothing,
      ingressBytes = Core.Nothing,
      egressPackets = Core.Nothing,
      timestamp = Core.Nothing,
      username = Core.Nothing,
      clientIp = Core.Nothing
    }

-- | The ID of the Client VPN endpoint to which the client is connected.
clientVpnConnection_clientVpnEndpointId :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_clientVpnEndpointId = Lens.lens (\ClientVpnConnection' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnConnection' {} a -> s {clientVpnEndpointId = a} :: ClientVpnConnection)

-- | The current state of the client connection.
clientVpnConnection_status :: Lens.Lens' ClientVpnConnection (Core.Maybe ClientVpnConnectionStatus)
clientVpnConnection_status = Lens.lens (\ClientVpnConnection' {status} -> status) (\s@ClientVpnConnection' {} a -> s {status = a} :: ClientVpnConnection)

-- | The number of packets sent by the client.
clientVpnConnection_ingressPackets :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_ingressPackets = Lens.lens (\ClientVpnConnection' {ingressPackets} -> ingressPackets) (\s@ClientVpnConnection' {} a -> s {ingressPackets = a} :: ClientVpnConnection)

-- | The number of bytes received by the client.
clientVpnConnection_egressBytes :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_egressBytes = Lens.lens (\ClientVpnConnection' {egressBytes} -> egressBytes) (\s@ClientVpnConnection' {} a -> s {egressBytes = a} :: ClientVpnConnection)

-- | The ID of the client connection.
clientVpnConnection_connectionId :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_connectionId = Lens.lens (\ClientVpnConnection' {connectionId} -> connectionId) (\s@ClientVpnConnection' {} a -> s {connectionId = a} :: ClientVpnConnection)

-- | The date and time the client connection was established.
clientVpnConnection_connectionEstablishedTime :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_connectionEstablishedTime = Lens.lens (\ClientVpnConnection' {connectionEstablishedTime} -> connectionEstablishedTime) (\s@ClientVpnConnection' {} a -> s {connectionEstablishedTime = a} :: ClientVpnConnection)

-- | The statuses returned by the client connect handler for posture
-- compliance, if applicable.
clientVpnConnection_postureComplianceStatuses :: Lens.Lens' ClientVpnConnection (Core.Maybe [Core.Text])
clientVpnConnection_postureComplianceStatuses = Lens.lens (\ClientVpnConnection' {postureComplianceStatuses} -> postureComplianceStatuses) (\s@ClientVpnConnection' {} a -> s {postureComplianceStatuses = a} :: ClientVpnConnection) Core.. Lens.mapping Lens._Coerce

-- | The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
clientVpnConnection_commonName :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_commonName = Lens.lens (\ClientVpnConnection' {commonName} -> commonName) (\s@ClientVpnConnection' {} a -> s {commonName = a} :: ClientVpnConnection)

-- | The date and time the client connection was terminated.
clientVpnConnection_connectionEndTime :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_connectionEndTime = Lens.lens (\ClientVpnConnection' {connectionEndTime} -> connectionEndTime) (\s@ClientVpnConnection' {} a -> s {connectionEndTime = a} :: ClientVpnConnection)

-- | The number of bytes sent by the client.
clientVpnConnection_ingressBytes :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_ingressBytes = Lens.lens (\ClientVpnConnection' {ingressBytes} -> ingressBytes) (\s@ClientVpnConnection' {} a -> s {ingressBytes = a} :: ClientVpnConnection)

-- | The number of packets received by the client.
clientVpnConnection_egressPackets :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_egressPackets = Lens.lens (\ClientVpnConnection' {egressPackets} -> egressPackets) (\s@ClientVpnConnection' {} a -> s {egressPackets = a} :: ClientVpnConnection)

-- | The current date and time.
clientVpnConnection_timestamp :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_timestamp = Lens.lens (\ClientVpnConnection' {timestamp} -> timestamp) (\s@ClientVpnConnection' {} a -> s {timestamp = a} :: ClientVpnConnection)

-- | The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
clientVpnConnection_username :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_username = Lens.lens (\ClientVpnConnection' {username} -> username) (\s@ClientVpnConnection' {} a -> s {username = a} :: ClientVpnConnection)

-- | The IP address of the client.
clientVpnConnection_clientIp :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
clientVpnConnection_clientIp = Lens.lens (\ClientVpnConnection' {clientIp} -> clientIp) (\s@ClientVpnConnection' {} a -> s {clientIp = a} :: ClientVpnConnection)

instance Core.FromXML ClientVpnConnection where
  parseXML x =
    ClientVpnConnection'
      Core.<$> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "ingressPackets")
      Core.<*> (x Core..@? "egressBytes")
      Core.<*> (x Core..@? "connectionId")
      Core.<*> (x Core..@? "connectionEstablishedTime")
      Core.<*> ( x Core..@? "postureComplianceStatusSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "commonName")
      Core.<*> (x Core..@? "connectionEndTime")
      Core.<*> (x Core..@? "ingressBytes")
      Core.<*> (x Core..@? "egressPackets")
      Core.<*> (x Core..@? "timestamp")
      Core.<*> (x Core..@? "username")
      Core.<*> (x Core..@? "clientIp")

instance Core.Hashable ClientVpnConnection

instance Core.NFData ClientVpnConnection

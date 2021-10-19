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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a client connection.
--
-- /See:/ 'newClientVpnConnection' smart constructor.
data ClientVpnConnection = ClientVpnConnection'
  { -- | The number of packets sent by the client.
    ingressPackets :: Prelude.Maybe Prelude.Text,
    -- | The current state of the client connection.
    status :: Prelude.Maybe ClientVpnConnectionStatus,
    -- | The date and time the client connection was terminated.
    connectionEndTime :: Prelude.Maybe Prelude.Text,
    -- | The common name associated with the client. This is either the name of
    -- the client certificate, or the Active Directory user name.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | The statuses returned by the client connect handler for posture
    -- compliance, if applicable.
    postureComplianceStatuses :: Prelude.Maybe [Prelude.Text],
    -- | The date and time the client connection was established.
    connectionEstablishedTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the client connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes sent by the client.
    ingressBytes :: Prelude.Maybe Prelude.Text,
    -- | The username of the client who established the client connection. This
    -- information is only provided if Active Directory client authentication
    -- is used.
    username :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes received by the client.
    egressBytes :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint to which the client is connected.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the client.
    clientIp :: Prelude.Maybe Prelude.Text,
    -- | The number of packets received by the client.
    egressPackets :: Prelude.Maybe Prelude.Text,
    -- | The current date and time.
    timestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingressPackets', 'clientVpnConnection_ingressPackets' - The number of packets sent by the client.
--
-- 'status', 'clientVpnConnection_status' - The current state of the client connection.
--
-- 'connectionEndTime', 'clientVpnConnection_connectionEndTime' - The date and time the client connection was terminated.
--
-- 'commonName', 'clientVpnConnection_commonName' - The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
--
-- 'postureComplianceStatuses', 'clientVpnConnection_postureComplianceStatuses' - The statuses returned by the client connect handler for posture
-- compliance, if applicable.
--
-- 'connectionEstablishedTime', 'clientVpnConnection_connectionEstablishedTime' - The date and time the client connection was established.
--
-- 'connectionId', 'clientVpnConnection_connectionId' - The ID of the client connection.
--
-- 'ingressBytes', 'clientVpnConnection_ingressBytes' - The number of bytes sent by the client.
--
-- 'username', 'clientVpnConnection_username' - The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
--
-- 'egressBytes', 'clientVpnConnection_egressBytes' - The number of bytes received by the client.
--
-- 'clientVpnEndpointId', 'clientVpnConnection_clientVpnEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
--
-- 'clientIp', 'clientVpnConnection_clientIp' - The IP address of the client.
--
-- 'egressPackets', 'clientVpnConnection_egressPackets' - The number of packets received by the client.
--
-- 'timestamp', 'clientVpnConnection_timestamp' - The current date and time.
newClientVpnConnection ::
  ClientVpnConnection
newClientVpnConnection =
  ClientVpnConnection'
    { ingressPackets =
        Prelude.Nothing,
      status = Prelude.Nothing,
      connectionEndTime = Prelude.Nothing,
      commonName = Prelude.Nothing,
      postureComplianceStatuses = Prelude.Nothing,
      connectionEstablishedTime = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      ingressBytes = Prelude.Nothing,
      username = Prelude.Nothing,
      egressBytes = Prelude.Nothing,
      clientVpnEndpointId = Prelude.Nothing,
      clientIp = Prelude.Nothing,
      egressPackets = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The number of packets sent by the client.
clientVpnConnection_ingressPackets :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_ingressPackets = Lens.lens (\ClientVpnConnection' {ingressPackets} -> ingressPackets) (\s@ClientVpnConnection' {} a -> s {ingressPackets = a} :: ClientVpnConnection)

-- | The current state of the client connection.
clientVpnConnection_status :: Lens.Lens' ClientVpnConnection (Prelude.Maybe ClientVpnConnectionStatus)
clientVpnConnection_status = Lens.lens (\ClientVpnConnection' {status} -> status) (\s@ClientVpnConnection' {} a -> s {status = a} :: ClientVpnConnection)

-- | The date and time the client connection was terminated.
clientVpnConnection_connectionEndTime :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionEndTime = Lens.lens (\ClientVpnConnection' {connectionEndTime} -> connectionEndTime) (\s@ClientVpnConnection' {} a -> s {connectionEndTime = a} :: ClientVpnConnection)

-- | The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
clientVpnConnection_commonName :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_commonName = Lens.lens (\ClientVpnConnection' {commonName} -> commonName) (\s@ClientVpnConnection' {} a -> s {commonName = a} :: ClientVpnConnection)

-- | The statuses returned by the client connect handler for posture
-- compliance, if applicable.
clientVpnConnection_postureComplianceStatuses :: Lens.Lens' ClientVpnConnection (Prelude.Maybe [Prelude.Text])
clientVpnConnection_postureComplianceStatuses = Lens.lens (\ClientVpnConnection' {postureComplianceStatuses} -> postureComplianceStatuses) (\s@ClientVpnConnection' {} a -> s {postureComplianceStatuses = a} :: ClientVpnConnection) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the client connection was established.
clientVpnConnection_connectionEstablishedTime :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionEstablishedTime = Lens.lens (\ClientVpnConnection' {connectionEstablishedTime} -> connectionEstablishedTime) (\s@ClientVpnConnection' {} a -> s {connectionEstablishedTime = a} :: ClientVpnConnection)

-- | The ID of the client connection.
clientVpnConnection_connectionId :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionId = Lens.lens (\ClientVpnConnection' {connectionId} -> connectionId) (\s@ClientVpnConnection' {} a -> s {connectionId = a} :: ClientVpnConnection)

-- | The number of bytes sent by the client.
clientVpnConnection_ingressBytes :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_ingressBytes = Lens.lens (\ClientVpnConnection' {ingressBytes} -> ingressBytes) (\s@ClientVpnConnection' {} a -> s {ingressBytes = a} :: ClientVpnConnection)

-- | The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
clientVpnConnection_username :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_username = Lens.lens (\ClientVpnConnection' {username} -> username) (\s@ClientVpnConnection' {} a -> s {username = a} :: ClientVpnConnection)

-- | The number of bytes received by the client.
clientVpnConnection_egressBytes :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_egressBytes = Lens.lens (\ClientVpnConnection' {egressBytes} -> egressBytes) (\s@ClientVpnConnection' {} a -> s {egressBytes = a} :: ClientVpnConnection)

-- | The ID of the Client VPN endpoint to which the client is connected.
clientVpnConnection_clientVpnEndpointId :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_clientVpnEndpointId = Lens.lens (\ClientVpnConnection' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnConnection' {} a -> s {clientVpnEndpointId = a} :: ClientVpnConnection)

-- | The IP address of the client.
clientVpnConnection_clientIp :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_clientIp = Lens.lens (\ClientVpnConnection' {clientIp} -> clientIp) (\s@ClientVpnConnection' {} a -> s {clientIp = a} :: ClientVpnConnection)

-- | The number of packets received by the client.
clientVpnConnection_egressPackets :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_egressPackets = Lens.lens (\ClientVpnConnection' {egressPackets} -> egressPackets) (\s@ClientVpnConnection' {} a -> s {egressPackets = a} :: ClientVpnConnection)

-- | The current date and time.
clientVpnConnection_timestamp :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_timestamp = Lens.lens (\ClientVpnConnection' {timestamp} -> timestamp) (\s@ClientVpnConnection' {} a -> s {timestamp = a} :: ClientVpnConnection)

instance Core.FromXML ClientVpnConnection where
  parseXML x =
    ClientVpnConnection'
      Prelude.<$> (x Core..@? "ingressPackets")
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "connectionEndTime")
      Prelude.<*> (x Core..@? "commonName")
      Prelude.<*> ( x Core..@? "postureComplianceStatusSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "connectionEstablishedTime")
      Prelude.<*> (x Core..@? "connectionId")
      Prelude.<*> (x Core..@? "ingressBytes")
      Prelude.<*> (x Core..@? "username")
      Prelude.<*> (x Core..@? "egressBytes")
      Prelude.<*> (x Core..@? "clientVpnEndpointId")
      Prelude.<*> (x Core..@? "clientIp")
      Prelude.<*> (x Core..@? "egressPackets")
      Prelude.<*> (x Core..@? "timestamp")

instance Prelude.Hashable ClientVpnConnection

instance Prelude.NFData ClientVpnConnection

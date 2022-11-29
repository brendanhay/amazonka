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
-- Module      : Amazonka.EC2.Types.ClientVpnConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a client connection.
--
-- /See:/ 'newClientVpnConnection' smart constructor.
data ClientVpnConnection = ClientVpnConnection'
  { -- | The number of bytes received by the client.
    egressBytes :: Prelude.Maybe Prelude.Text,
    -- | The username of the client who established the client connection. This
    -- information is only provided if Active Directory client authentication
    -- is used.
    username :: Prelude.Maybe Prelude.Text,
    -- | The current date and time.
    timestamp :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes sent by the client.
    ingressBytes :: Prelude.Maybe Prelude.Text,
    -- | The statuses returned by the client connect handler for posture
    -- compliance, if applicable.
    postureComplianceStatuses :: Prelude.Maybe [Prelude.Text],
    -- | The current state of the client connection.
    status :: Prelude.Maybe ClientVpnConnectionStatus,
    -- | The ID of the client connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The number of packets received by the client.
    egressPackets :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint to which the client is connected.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the client connection was terminated.
    connectionEndTime :: Prelude.Maybe Prelude.Text,
    -- | The number of packets sent by the client.
    ingressPackets :: Prelude.Maybe Prelude.Text,
    -- | The common name associated with the client. This is either the name of
    -- the client certificate, or the Active Directory user name.
    commonName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the client connection was established.
    connectionEstablishedTime :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the client.
    clientIp :: Prelude.Maybe Prelude.Text
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
-- 'egressBytes', 'clientVpnConnection_egressBytes' - The number of bytes received by the client.
--
-- 'username', 'clientVpnConnection_username' - The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
--
-- 'timestamp', 'clientVpnConnection_timestamp' - The current date and time.
--
-- 'ingressBytes', 'clientVpnConnection_ingressBytes' - The number of bytes sent by the client.
--
-- 'postureComplianceStatuses', 'clientVpnConnection_postureComplianceStatuses' - The statuses returned by the client connect handler for posture
-- compliance, if applicable.
--
-- 'status', 'clientVpnConnection_status' - The current state of the client connection.
--
-- 'connectionId', 'clientVpnConnection_connectionId' - The ID of the client connection.
--
-- 'egressPackets', 'clientVpnConnection_egressPackets' - The number of packets received by the client.
--
-- 'clientVpnEndpointId', 'clientVpnConnection_clientVpnEndpointId' - The ID of the Client VPN endpoint to which the client is connected.
--
-- 'connectionEndTime', 'clientVpnConnection_connectionEndTime' - The date and time the client connection was terminated.
--
-- 'ingressPackets', 'clientVpnConnection_ingressPackets' - The number of packets sent by the client.
--
-- 'commonName', 'clientVpnConnection_commonName' - The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
--
-- 'connectionEstablishedTime', 'clientVpnConnection_connectionEstablishedTime' - The date and time the client connection was established.
--
-- 'clientIp', 'clientVpnConnection_clientIp' - The IP address of the client.
newClientVpnConnection ::
  ClientVpnConnection
newClientVpnConnection =
  ClientVpnConnection'
    { egressBytes = Prelude.Nothing,
      username = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      ingressBytes = Prelude.Nothing,
      postureComplianceStatuses = Prelude.Nothing,
      status = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      egressPackets = Prelude.Nothing,
      clientVpnEndpointId = Prelude.Nothing,
      connectionEndTime = Prelude.Nothing,
      ingressPackets = Prelude.Nothing,
      commonName = Prelude.Nothing,
      connectionEstablishedTime = Prelude.Nothing,
      clientIp = Prelude.Nothing
    }

-- | The number of bytes received by the client.
clientVpnConnection_egressBytes :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_egressBytes = Lens.lens (\ClientVpnConnection' {egressBytes} -> egressBytes) (\s@ClientVpnConnection' {} a -> s {egressBytes = a} :: ClientVpnConnection)

-- | The username of the client who established the client connection. This
-- information is only provided if Active Directory client authentication
-- is used.
clientVpnConnection_username :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_username = Lens.lens (\ClientVpnConnection' {username} -> username) (\s@ClientVpnConnection' {} a -> s {username = a} :: ClientVpnConnection)

-- | The current date and time.
clientVpnConnection_timestamp :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_timestamp = Lens.lens (\ClientVpnConnection' {timestamp} -> timestamp) (\s@ClientVpnConnection' {} a -> s {timestamp = a} :: ClientVpnConnection)

-- | The number of bytes sent by the client.
clientVpnConnection_ingressBytes :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_ingressBytes = Lens.lens (\ClientVpnConnection' {ingressBytes} -> ingressBytes) (\s@ClientVpnConnection' {} a -> s {ingressBytes = a} :: ClientVpnConnection)

-- | The statuses returned by the client connect handler for posture
-- compliance, if applicable.
clientVpnConnection_postureComplianceStatuses :: Lens.Lens' ClientVpnConnection (Prelude.Maybe [Prelude.Text])
clientVpnConnection_postureComplianceStatuses = Lens.lens (\ClientVpnConnection' {postureComplianceStatuses} -> postureComplianceStatuses) (\s@ClientVpnConnection' {} a -> s {postureComplianceStatuses = a} :: ClientVpnConnection) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the client connection.
clientVpnConnection_status :: Lens.Lens' ClientVpnConnection (Prelude.Maybe ClientVpnConnectionStatus)
clientVpnConnection_status = Lens.lens (\ClientVpnConnection' {status} -> status) (\s@ClientVpnConnection' {} a -> s {status = a} :: ClientVpnConnection)

-- | The ID of the client connection.
clientVpnConnection_connectionId :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionId = Lens.lens (\ClientVpnConnection' {connectionId} -> connectionId) (\s@ClientVpnConnection' {} a -> s {connectionId = a} :: ClientVpnConnection)

-- | The number of packets received by the client.
clientVpnConnection_egressPackets :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_egressPackets = Lens.lens (\ClientVpnConnection' {egressPackets} -> egressPackets) (\s@ClientVpnConnection' {} a -> s {egressPackets = a} :: ClientVpnConnection)

-- | The ID of the Client VPN endpoint to which the client is connected.
clientVpnConnection_clientVpnEndpointId :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_clientVpnEndpointId = Lens.lens (\ClientVpnConnection' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnConnection' {} a -> s {clientVpnEndpointId = a} :: ClientVpnConnection)

-- | The date and time the client connection was terminated.
clientVpnConnection_connectionEndTime :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionEndTime = Lens.lens (\ClientVpnConnection' {connectionEndTime} -> connectionEndTime) (\s@ClientVpnConnection' {} a -> s {connectionEndTime = a} :: ClientVpnConnection)

-- | The number of packets sent by the client.
clientVpnConnection_ingressPackets :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_ingressPackets = Lens.lens (\ClientVpnConnection' {ingressPackets} -> ingressPackets) (\s@ClientVpnConnection' {} a -> s {ingressPackets = a} :: ClientVpnConnection)

-- | The common name associated with the client. This is either the name of
-- the client certificate, or the Active Directory user name.
clientVpnConnection_commonName :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_commonName = Lens.lens (\ClientVpnConnection' {commonName} -> commonName) (\s@ClientVpnConnection' {} a -> s {commonName = a} :: ClientVpnConnection)

-- | The date and time the client connection was established.
clientVpnConnection_connectionEstablishedTime :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_connectionEstablishedTime = Lens.lens (\ClientVpnConnection' {connectionEstablishedTime} -> connectionEstablishedTime) (\s@ClientVpnConnection' {} a -> s {connectionEstablishedTime = a} :: ClientVpnConnection)

-- | The IP address of the client.
clientVpnConnection_clientIp :: Lens.Lens' ClientVpnConnection (Prelude.Maybe Prelude.Text)
clientVpnConnection_clientIp = Lens.lens (\ClientVpnConnection' {clientIp} -> clientIp) (\s@ClientVpnConnection' {} a -> s {clientIp = a} :: ClientVpnConnection)

instance Core.FromXML ClientVpnConnection where
  parseXML x =
    ClientVpnConnection'
      Prelude.<$> (x Core..@? "egressBytes")
      Prelude.<*> (x Core..@? "username")
      Prelude.<*> (x Core..@? "timestamp")
      Prelude.<*> (x Core..@? "ingressBytes")
      Prelude.<*> ( x Core..@? "postureComplianceStatusSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "connectionId")
      Prelude.<*> (x Core..@? "egressPackets")
      Prelude.<*> (x Core..@? "clientVpnEndpointId")
      Prelude.<*> (x Core..@? "connectionEndTime")
      Prelude.<*> (x Core..@? "ingressPackets")
      Prelude.<*> (x Core..@? "commonName")
      Prelude.<*> (x Core..@? "connectionEstablishedTime")
      Prelude.<*> (x Core..@? "clientIp")

instance Prelude.Hashable ClientVpnConnection where
  hashWithSalt _salt ClientVpnConnection' {..} =
    _salt `Prelude.hashWithSalt` egressBytes
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` ingressBytes
      `Prelude.hashWithSalt` postureComplianceStatuses
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` egressPackets
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` connectionEndTime
      `Prelude.hashWithSalt` ingressPackets
      `Prelude.hashWithSalt` commonName
      `Prelude.hashWithSalt` connectionEstablishedTime
      `Prelude.hashWithSalt` clientIp

instance Prelude.NFData ClientVpnConnection where
  rnf ClientVpnConnection' {..} =
    Prelude.rnf egressBytes
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf ingressBytes
      `Prelude.seq` Prelude.rnf postureComplianceStatuses
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf egressPackets
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf connectionEndTime
      `Prelude.seq` Prelude.rnf ingressPackets
      `Prelude.seq` Prelude.rnf commonName
      `Prelude.seq` Prelude.rnf connectionEstablishedTime
      `Prelude.seq` Prelude.rnf clientIp

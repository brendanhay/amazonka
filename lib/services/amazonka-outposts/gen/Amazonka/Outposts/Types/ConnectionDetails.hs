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
-- Module      : Amazonka.Outposts.Types.ConnectionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a connection.
--
-- /See:/ 'newConnectionDetails' smart constructor.
data ConnectionDetails = ConnectionDetails'
  { -- | The endpoint for the server.
    serverEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The public key of the server.
    serverPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The public key of the client.
    clientPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The client tunnel address.
    clientTunnelAddress :: Prelude.Maybe Prelude.Text,
    -- | The server tunnel address.
    serverTunnelAddress :: Prelude.Maybe Prelude.Text,
    -- | The allowed IP addresses.
    allowedIps :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverEndpoint', 'connectionDetails_serverEndpoint' - The endpoint for the server.
--
-- 'serverPublicKey', 'connectionDetails_serverPublicKey' - The public key of the server.
--
-- 'clientPublicKey', 'connectionDetails_clientPublicKey' - The public key of the client.
--
-- 'clientTunnelAddress', 'connectionDetails_clientTunnelAddress' - The client tunnel address.
--
-- 'serverTunnelAddress', 'connectionDetails_serverTunnelAddress' - The server tunnel address.
--
-- 'allowedIps', 'connectionDetails_allowedIps' - The allowed IP addresses.
newConnectionDetails ::
  ConnectionDetails
newConnectionDetails =
  ConnectionDetails'
    { serverEndpoint =
        Prelude.Nothing,
      serverPublicKey = Prelude.Nothing,
      clientPublicKey = Prelude.Nothing,
      clientTunnelAddress = Prelude.Nothing,
      serverTunnelAddress = Prelude.Nothing,
      allowedIps = Prelude.Nothing
    }

-- | The endpoint for the server.
connectionDetails_serverEndpoint :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Text)
connectionDetails_serverEndpoint = Lens.lens (\ConnectionDetails' {serverEndpoint} -> serverEndpoint) (\s@ConnectionDetails' {} a -> s {serverEndpoint = a} :: ConnectionDetails)

-- | The public key of the server.
connectionDetails_serverPublicKey :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Text)
connectionDetails_serverPublicKey = Lens.lens (\ConnectionDetails' {serverPublicKey} -> serverPublicKey) (\s@ConnectionDetails' {} a -> s {serverPublicKey = a} :: ConnectionDetails)

-- | The public key of the client.
connectionDetails_clientPublicKey :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Text)
connectionDetails_clientPublicKey = Lens.lens (\ConnectionDetails' {clientPublicKey} -> clientPublicKey) (\s@ConnectionDetails' {} a -> s {clientPublicKey = a} :: ConnectionDetails)

-- | The client tunnel address.
connectionDetails_clientTunnelAddress :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Text)
connectionDetails_clientTunnelAddress = Lens.lens (\ConnectionDetails' {clientTunnelAddress} -> clientTunnelAddress) (\s@ConnectionDetails' {} a -> s {clientTunnelAddress = a} :: ConnectionDetails)

-- | The server tunnel address.
connectionDetails_serverTunnelAddress :: Lens.Lens' ConnectionDetails (Prelude.Maybe Prelude.Text)
connectionDetails_serverTunnelAddress = Lens.lens (\ConnectionDetails' {serverTunnelAddress} -> serverTunnelAddress) (\s@ConnectionDetails' {} a -> s {serverTunnelAddress = a} :: ConnectionDetails)

-- | The allowed IP addresses.
connectionDetails_allowedIps :: Lens.Lens' ConnectionDetails (Prelude.Maybe [Prelude.Text])
connectionDetails_allowedIps = Lens.lens (\ConnectionDetails' {allowedIps} -> allowedIps) (\s@ConnectionDetails' {} a -> s {allowedIps = a} :: ConnectionDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConnectionDetails where
  parseJSON =
    Data.withObject
      "ConnectionDetails"
      ( \x ->
          ConnectionDetails'
            Prelude.<$> (x Data..:? "ServerEndpoint")
            Prelude.<*> (x Data..:? "ServerPublicKey")
            Prelude.<*> (x Data..:? "ClientPublicKey")
            Prelude.<*> (x Data..:? "ClientTunnelAddress")
            Prelude.<*> (x Data..:? "ServerTunnelAddress")
            Prelude.<*> (x Data..:? "AllowedIps" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ConnectionDetails where
  hashWithSalt _salt ConnectionDetails' {..} =
    _salt `Prelude.hashWithSalt` serverEndpoint
      `Prelude.hashWithSalt` serverPublicKey
      `Prelude.hashWithSalt` clientPublicKey
      `Prelude.hashWithSalt` clientTunnelAddress
      `Prelude.hashWithSalt` serverTunnelAddress
      `Prelude.hashWithSalt` allowedIps

instance Prelude.NFData ConnectionDetails where
  rnf ConnectionDetails' {..} =
    Prelude.rnf serverEndpoint
      `Prelude.seq` Prelude.rnf serverPublicKey
      `Prelude.seq` Prelude.rnf clientPublicKey
      `Prelude.seq` Prelude.rnf clientTunnelAddress
      `Prelude.seq` Prelude.rnf serverTunnelAddress
      `Prelude.seq` Prelude.rnf allowedIps

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
-- Module      : Amazonka.SMS.Types.ServerReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ServerReplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.ServerReplicationParameters

-- | Replication configuration of a server.
--
-- /See:/ 'newServerReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { -- | The parameters for replicating the server.
    serverReplicationParameters :: Prelude.Maybe ServerReplicationParameters,
    -- | The ID of the server with which this replication configuration is
    -- associated.
    server :: Prelude.Maybe Server
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverReplicationParameters', 'serverReplicationConfiguration_serverReplicationParameters' - The parameters for replicating the server.
--
-- 'server', 'serverReplicationConfiguration_server' - The ID of the server with which this replication configuration is
-- associated.
newServerReplicationConfiguration ::
  ServerReplicationConfiguration
newServerReplicationConfiguration =
  ServerReplicationConfiguration'
    { serverReplicationParameters =
        Prelude.Nothing,
      server = Prelude.Nothing
    }

-- | The parameters for replicating the server.
serverReplicationConfiguration_serverReplicationParameters :: Lens.Lens' ServerReplicationConfiguration (Prelude.Maybe ServerReplicationParameters)
serverReplicationConfiguration_serverReplicationParameters = Lens.lens (\ServerReplicationConfiguration' {serverReplicationParameters} -> serverReplicationParameters) (\s@ServerReplicationConfiguration' {} a -> s {serverReplicationParameters = a} :: ServerReplicationConfiguration)

-- | The ID of the server with which this replication configuration is
-- associated.
serverReplicationConfiguration_server :: Lens.Lens' ServerReplicationConfiguration (Prelude.Maybe Server)
serverReplicationConfiguration_server = Lens.lens (\ServerReplicationConfiguration' {server} -> server) (\s@ServerReplicationConfiguration' {} a -> s {server = a} :: ServerReplicationConfiguration)

instance Data.FromJSON ServerReplicationConfiguration where
  parseJSON =
    Data.withObject
      "ServerReplicationConfiguration"
      ( \x ->
          ServerReplicationConfiguration'
            Prelude.<$> (x Data..:? "serverReplicationParameters")
            Prelude.<*> (x Data..:? "server")
      )

instance
  Prelude.Hashable
    ServerReplicationConfiguration
  where
  hashWithSalt
    _salt
    ServerReplicationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` serverReplicationParameters
        `Prelude.hashWithSalt` server

instance
  Prelude.NFData
    ServerReplicationConfiguration
  where
  rnf ServerReplicationConfiguration' {..} =
    Prelude.rnf serverReplicationParameters
      `Prelude.seq` Prelude.rnf server

instance Data.ToJSON ServerReplicationConfiguration where
  toJSON ServerReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serverReplicationParameters" Data..=)
              Prelude.<$> serverReplicationParameters,
            ("server" Data..=) Prelude.<$> server
          ]
      )

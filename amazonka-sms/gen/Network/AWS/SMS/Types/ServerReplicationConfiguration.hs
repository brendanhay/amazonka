{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SMS.Types.ServerReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerReplicationConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerReplicationParameters

-- | Replication configuration of a server.
--
-- /See:/ 'newServerReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { -- | The ID of the server with which this replication configuration is
    -- associated.
    server :: Prelude.Maybe Server,
    -- | The parameters for replicating the server.
    serverReplicationParameters :: Prelude.Maybe ServerReplicationParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServerReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'server', 'serverReplicationConfiguration_server' - The ID of the server with which this replication configuration is
-- associated.
--
-- 'serverReplicationParameters', 'serverReplicationConfiguration_serverReplicationParameters' - The parameters for replicating the server.
newServerReplicationConfiguration ::
  ServerReplicationConfiguration
newServerReplicationConfiguration =
  ServerReplicationConfiguration'
    { server =
        Prelude.Nothing,
      serverReplicationParameters =
        Prelude.Nothing
    }

-- | The ID of the server with which this replication configuration is
-- associated.
serverReplicationConfiguration_server :: Lens.Lens' ServerReplicationConfiguration (Prelude.Maybe Server)
serverReplicationConfiguration_server = Lens.lens (\ServerReplicationConfiguration' {server} -> server) (\s@ServerReplicationConfiguration' {} a -> s {server = a} :: ServerReplicationConfiguration)

-- | The parameters for replicating the server.
serverReplicationConfiguration_serverReplicationParameters :: Lens.Lens' ServerReplicationConfiguration (Prelude.Maybe ServerReplicationParameters)
serverReplicationConfiguration_serverReplicationParameters = Lens.lens (\ServerReplicationConfiguration' {serverReplicationParameters} -> serverReplicationParameters) (\s@ServerReplicationConfiguration' {} a -> s {serverReplicationParameters = a} :: ServerReplicationConfiguration)

instance
  Prelude.FromJSON
    ServerReplicationConfiguration
  where
  parseJSON =
    Prelude.withObject
      "ServerReplicationConfiguration"
      ( \x ->
          ServerReplicationConfiguration'
            Prelude.<$> (x Prelude..:? "server")
            Prelude.<*> (x Prelude..:? "serverReplicationParameters")
      )

instance
  Prelude.Hashable
    ServerReplicationConfiguration

instance
  Prelude.NFData
    ServerReplicationConfiguration

instance
  Prelude.ToJSON
    ServerReplicationConfiguration
  where
  toJSON ServerReplicationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("server" Prelude..=) Prelude.<$> server,
            ("serverReplicationParameters" Prelude..=)
              Prelude.<$> serverReplicationParameters
          ]
      )

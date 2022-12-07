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
-- Module      : Amazonka.SMS.Types.Server
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.Server where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ServerType
import Amazonka.SMS.Types.VmServer

-- | Represents a server.
--
-- /See:/ 'newServer' smart constructor.
data Server = Server'
  { -- | The type of server.
    serverType :: Prelude.Maybe ServerType,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VM server.
    vmServer :: Prelude.Maybe VmServer,
    -- | Indicates whether the replication job is deleted or failed.
    replicationJobTerminated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Server' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverType', 'server_serverType' - The type of server.
--
-- 'replicationJobId', 'server_replicationJobId' - The ID of the replication job.
--
-- 'serverId', 'server_serverId' - The ID of the server.
--
-- 'vmServer', 'server_vmServer' - Information about the VM server.
--
-- 'replicationJobTerminated', 'server_replicationJobTerminated' - Indicates whether the replication job is deleted or failed.
newServer ::
  Server
newServer =
  Server'
    { serverType = Prelude.Nothing,
      replicationJobId = Prelude.Nothing,
      serverId = Prelude.Nothing,
      vmServer = Prelude.Nothing,
      replicationJobTerminated = Prelude.Nothing
    }

-- | The type of server.
server_serverType :: Lens.Lens' Server (Prelude.Maybe ServerType)
server_serverType = Lens.lens (\Server' {serverType} -> serverType) (\s@Server' {} a -> s {serverType = a} :: Server)

-- | The ID of the replication job.
server_replicationJobId :: Lens.Lens' Server (Prelude.Maybe Prelude.Text)
server_replicationJobId = Lens.lens (\Server' {replicationJobId} -> replicationJobId) (\s@Server' {} a -> s {replicationJobId = a} :: Server)

-- | The ID of the server.
server_serverId :: Lens.Lens' Server (Prelude.Maybe Prelude.Text)
server_serverId = Lens.lens (\Server' {serverId} -> serverId) (\s@Server' {} a -> s {serverId = a} :: Server)

-- | Information about the VM server.
server_vmServer :: Lens.Lens' Server (Prelude.Maybe VmServer)
server_vmServer = Lens.lens (\Server' {vmServer} -> vmServer) (\s@Server' {} a -> s {vmServer = a} :: Server)

-- | Indicates whether the replication job is deleted or failed.
server_replicationJobTerminated :: Lens.Lens' Server (Prelude.Maybe Prelude.Bool)
server_replicationJobTerminated = Lens.lens (\Server' {replicationJobTerminated} -> replicationJobTerminated) (\s@Server' {} a -> s {replicationJobTerminated = a} :: Server)

instance Data.FromJSON Server where
  parseJSON =
    Data.withObject
      "Server"
      ( \x ->
          Server'
            Prelude.<$> (x Data..:? "serverType")
            Prelude.<*> (x Data..:? "replicationJobId")
            Prelude.<*> (x Data..:? "serverId")
            Prelude.<*> (x Data..:? "vmServer")
            Prelude.<*> (x Data..:? "replicationJobTerminated")
      )

instance Prelude.Hashable Server where
  hashWithSalt _salt Server' {..} =
    _salt `Prelude.hashWithSalt` serverType
      `Prelude.hashWithSalt` replicationJobId
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` vmServer
      `Prelude.hashWithSalt` replicationJobTerminated

instance Prelude.NFData Server where
  rnf Server' {..} =
    Prelude.rnf serverType
      `Prelude.seq` Prelude.rnf replicationJobId
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf vmServer
      `Prelude.seq` Prelude.rnf replicationJobTerminated

instance Data.ToJSON Server where
  toJSON Server' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serverType" Data..=) Prelude.<$> serverType,
            ("replicationJobId" Data..=)
              Prelude.<$> replicationJobId,
            ("serverId" Data..=) Prelude.<$> serverId,
            ("vmServer" Data..=) Prelude.<$> vmServer,
            ("replicationJobTerminated" Data..=)
              Prelude.<$> replicationJobTerminated
          ]
      )

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The ID of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the replication job is deleted or failed.
    replicationJobTerminated :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the server.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | The type of server.
    serverType :: Prelude.Maybe ServerType,
    -- | Information about the VM server.
    vmServer :: Prelude.Maybe VmServer
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
-- 'replicationJobId', 'server_replicationJobId' - The ID of the replication job.
--
-- 'replicationJobTerminated', 'server_replicationJobTerminated' - Indicates whether the replication job is deleted or failed.
--
-- 'serverId', 'server_serverId' - The ID of the server.
--
-- 'serverType', 'server_serverType' - The type of server.
--
-- 'vmServer', 'server_vmServer' - Information about the VM server.
newServer ::
  Server
newServer =
  Server'
    { replicationJobId = Prelude.Nothing,
      replicationJobTerminated = Prelude.Nothing,
      serverId = Prelude.Nothing,
      serverType = Prelude.Nothing,
      vmServer = Prelude.Nothing
    }

-- | The ID of the replication job.
server_replicationJobId :: Lens.Lens' Server (Prelude.Maybe Prelude.Text)
server_replicationJobId = Lens.lens (\Server' {replicationJobId} -> replicationJobId) (\s@Server' {} a -> s {replicationJobId = a} :: Server)

-- | Indicates whether the replication job is deleted or failed.
server_replicationJobTerminated :: Lens.Lens' Server (Prelude.Maybe Prelude.Bool)
server_replicationJobTerminated = Lens.lens (\Server' {replicationJobTerminated} -> replicationJobTerminated) (\s@Server' {} a -> s {replicationJobTerminated = a} :: Server)

-- | The ID of the server.
server_serverId :: Lens.Lens' Server (Prelude.Maybe Prelude.Text)
server_serverId = Lens.lens (\Server' {serverId} -> serverId) (\s@Server' {} a -> s {serverId = a} :: Server)

-- | The type of server.
server_serverType :: Lens.Lens' Server (Prelude.Maybe ServerType)
server_serverType = Lens.lens (\Server' {serverType} -> serverType) (\s@Server' {} a -> s {serverType = a} :: Server)

-- | Information about the VM server.
server_vmServer :: Lens.Lens' Server (Prelude.Maybe VmServer)
server_vmServer = Lens.lens (\Server' {vmServer} -> vmServer) (\s@Server' {} a -> s {vmServer = a} :: Server)

instance Data.FromJSON Server where
  parseJSON =
    Data.withObject
      "Server"
      ( \x ->
          Server'
            Prelude.<$> (x Data..:? "replicationJobId")
            Prelude.<*> (x Data..:? "replicationJobTerminated")
            Prelude.<*> (x Data..:? "serverId")
            Prelude.<*> (x Data..:? "serverType")
            Prelude.<*> (x Data..:? "vmServer")
      )

instance Prelude.Hashable Server where
  hashWithSalt _salt Server' {..} =
    _salt
      `Prelude.hashWithSalt` replicationJobId
      `Prelude.hashWithSalt` replicationJobTerminated
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` serverType
      `Prelude.hashWithSalt` vmServer

instance Prelude.NFData Server where
  rnf Server' {..} =
    Prelude.rnf replicationJobId `Prelude.seq`
      Prelude.rnf replicationJobTerminated `Prelude.seq`
        Prelude.rnf serverId `Prelude.seq`
          Prelude.rnf serverType `Prelude.seq`
            Prelude.rnf vmServer

instance Data.ToJSON Server where
  toJSON Server' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("replicationJobId" Data..=)
              Prelude.<$> replicationJobId,
            ("replicationJobTerminated" Data..=)
              Prelude.<$> replicationJobTerminated,
            ("serverId" Data..=) Prelude.<$> serverId,
            ("serverType" Data..=) Prelude.<$> serverType,
            ("vmServer" Data..=) Prelude.<$> vmServer
          ]
      )

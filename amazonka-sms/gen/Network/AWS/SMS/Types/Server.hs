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
-- Module      : Network.AWS.SMS.Types.Server
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Server where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.VmServer

-- | Represents a server.
--
-- /See:/ 'newServer' smart constructor.
data Server = Server'
  { -- | The ID of the server.
    serverId :: Core.Maybe Core.Text,
    -- | The ID of the replication job.
    replicationJobId :: Core.Maybe Core.Text,
    -- | Indicates whether the replication job is deleted or failed.
    replicationJobTerminated :: Core.Maybe Core.Bool,
    -- | Information about the VM server.
    vmServer :: Core.Maybe VmServer,
    -- | The type of server.
    serverType :: Core.Maybe ServerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Server' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'server_serverId' - The ID of the server.
--
-- 'replicationJobId', 'server_replicationJobId' - The ID of the replication job.
--
-- 'replicationJobTerminated', 'server_replicationJobTerminated' - Indicates whether the replication job is deleted or failed.
--
-- 'vmServer', 'server_vmServer' - Information about the VM server.
--
-- 'serverType', 'server_serverType' - The type of server.
newServer ::
  Server
newServer =
  Server'
    { serverId = Core.Nothing,
      replicationJobId = Core.Nothing,
      replicationJobTerminated = Core.Nothing,
      vmServer = Core.Nothing,
      serverType = Core.Nothing
    }

-- | The ID of the server.
server_serverId :: Lens.Lens' Server (Core.Maybe Core.Text)
server_serverId = Lens.lens (\Server' {serverId} -> serverId) (\s@Server' {} a -> s {serverId = a} :: Server)

-- | The ID of the replication job.
server_replicationJobId :: Lens.Lens' Server (Core.Maybe Core.Text)
server_replicationJobId = Lens.lens (\Server' {replicationJobId} -> replicationJobId) (\s@Server' {} a -> s {replicationJobId = a} :: Server)

-- | Indicates whether the replication job is deleted or failed.
server_replicationJobTerminated :: Lens.Lens' Server (Core.Maybe Core.Bool)
server_replicationJobTerminated = Lens.lens (\Server' {replicationJobTerminated} -> replicationJobTerminated) (\s@Server' {} a -> s {replicationJobTerminated = a} :: Server)

-- | Information about the VM server.
server_vmServer :: Lens.Lens' Server (Core.Maybe VmServer)
server_vmServer = Lens.lens (\Server' {vmServer} -> vmServer) (\s@Server' {} a -> s {vmServer = a} :: Server)

-- | The type of server.
server_serverType :: Lens.Lens' Server (Core.Maybe ServerType)
server_serverType = Lens.lens (\Server' {serverType} -> serverType) (\s@Server' {} a -> s {serverType = a} :: Server)

instance Core.FromJSON Server where
  parseJSON =
    Core.withObject
      "Server"
      ( \x ->
          Server'
            Core.<$> (x Core..:? "serverId")
            Core.<*> (x Core..:? "replicationJobId")
            Core.<*> (x Core..:? "replicationJobTerminated")
            Core.<*> (x Core..:? "vmServer")
            Core.<*> (x Core..:? "serverType")
      )

instance Core.Hashable Server

instance Core.NFData Server

instance Core.ToJSON Server where
  toJSON Server' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serverId" Core..=) Core.<$> serverId,
            ("replicationJobId" Core..=)
              Core.<$> replicationJobId,
            ("replicationJobTerminated" Core..=)
              Core.<$> replicationJobTerminated,
            ("vmServer" Core..=) Core.<$> vmServer,
            ("serverType" Core..=) Core.<$> serverType
          ]
      )

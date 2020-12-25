{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Server
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Server
  ( Server (..),

    -- * Smart constructor
    mkServer,

    -- * Lenses
    sReplicationJobId,
    sReplicationJobTerminated,
    sServerId,
    sServerType,
    sVmServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ReplicationJobId as Types
import qualified Network.AWS.SMS.Types.ServerId as Types
import qualified Network.AWS.SMS.Types.ServerType as Types
import qualified Network.AWS.SMS.Types.VmServer as Types

-- | Represents a server.
--
-- /See:/ 'mkServer' smart constructor.
data Server = Server'
  { -- | The ID of the replication job.
    replicationJobId :: Core.Maybe Types.ReplicationJobId,
    -- | Indicates whether the replication job is deleted or failed.
    replicationJobTerminated :: Core.Maybe Core.Bool,
    -- | The ID of the server.
    serverId :: Core.Maybe Types.ServerId,
    -- | The type of server.
    serverType :: Core.Maybe Types.ServerType,
    -- | Information about the VM server.
    vmServer :: Core.Maybe Types.VmServer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Server' value with any optional fields omitted.
mkServer ::
  Server
mkServer =
  Server'
    { replicationJobId = Core.Nothing,
      replicationJobTerminated = Core.Nothing,
      serverId = Core.Nothing,
      serverType = Core.Nothing,
      vmServer = Core.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationJobId :: Lens.Lens' Server (Core.Maybe Types.ReplicationJobId)
sReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED sReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | Indicates whether the replication job is deleted or failed.
--
-- /Note:/ Consider using 'replicationJobTerminated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationJobTerminated :: Lens.Lens' Server (Core.Maybe Core.Bool)
sReplicationJobTerminated = Lens.field @"replicationJobTerminated"
{-# DEPRECATED sReplicationJobTerminated "Use generic-lens or generic-optics with 'replicationJobTerminated' instead." #-}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerId :: Lens.Lens' Server (Core.Maybe Types.ServerId)
sServerId = Lens.field @"serverId"
{-# DEPRECATED sServerId "Use generic-lens or generic-optics with 'serverId' instead." #-}

-- | The type of server.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerType :: Lens.Lens' Server (Core.Maybe Types.ServerType)
sServerType = Lens.field @"serverType"
{-# DEPRECATED sServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | Information about the VM server.
--
-- /Note:/ Consider using 'vmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVmServer :: Lens.Lens' Server (Core.Maybe Types.VmServer)
sVmServer = Lens.field @"vmServer"
{-# DEPRECATED sVmServer "Use generic-lens or generic-optics with 'vmServer' instead." #-}

instance Core.FromJSON Server where
  toJSON Server {..} =
    Core.object
      ( Core.catMaybes
          [ ("replicationJobId" Core..=) Core.<$> replicationJobId,
            ("replicationJobTerminated" Core..=)
              Core.<$> replicationJobTerminated,
            ("serverId" Core..=) Core.<$> serverId,
            ("serverType" Core..=) Core.<$> serverType,
            ("vmServer" Core..=) Core.<$> vmServer
          ]
      )

instance Core.FromJSON Server where
  parseJSON =
    Core.withObject "Server" Core.$
      \x ->
        Server'
          Core.<$> (x Core..:? "replicationJobId")
          Core.<*> (x Core..:? "replicationJobTerminated")
          Core.<*> (x Core..:? "serverId")
          Core.<*> (x Core..:? "serverType")
          Core.<*> (x Core..:? "vmServer")

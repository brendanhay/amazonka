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
    sServerType,
    sServerId,
    sReplicationJobTerminated,
    sVmServer,
    sReplicationJobId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.VMServer

-- | Represents a server.
--
-- /See:/ 'mkServer' smart constructor.
data Server = Server'
  { serverType :: Lude.Maybe ServerType,
    serverId :: Lude.Maybe Lude.Text,
    replicationJobTerminated :: Lude.Maybe Lude.Bool,
    vmServer :: Lude.Maybe VMServer,
    replicationJobId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Server' with the minimum fields required to make a request.
--
-- * 'replicationJobId' - The ID of the replication job.
-- * 'replicationJobTerminated' - Indicates whether the replication job is deleted or failed.
-- * 'serverId' - The ID of the server.
-- * 'serverType' - The type of server.
-- * 'vmServer' - Information about the VM server.
mkServer ::
  Server
mkServer =
  Server'
    { serverType = Lude.Nothing,
      serverId = Lude.Nothing,
      replicationJobTerminated = Lude.Nothing,
      vmServer = Lude.Nothing,
      replicationJobId = Lude.Nothing
    }

-- | The type of server.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerType :: Lens.Lens' Server (Lude.Maybe ServerType)
sServerType = Lens.lens (serverType :: Server -> Lude.Maybe ServerType) (\s a -> s {serverType = a} :: Server)
{-# DEPRECATED sServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | The ID of the server.
--
-- /Note:/ Consider using 'serverId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerId :: Lens.Lens' Server (Lude.Maybe Lude.Text)
sServerId = Lens.lens (serverId :: Server -> Lude.Maybe Lude.Text) (\s a -> s {serverId = a} :: Server)
{-# DEPRECATED sServerId "Use generic-lens or generic-optics with 'serverId' instead." #-}

-- | Indicates whether the replication job is deleted or failed.
--
-- /Note:/ Consider using 'replicationJobTerminated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationJobTerminated :: Lens.Lens' Server (Lude.Maybe Lude.Bool)
sReplicationJobTerminated = Lens.lens (replicationJobTerminated :: Server -> Lude.Maybe Lude.Bool) (\s a -> s {replicationJobTerminated = a} :: Server)
{-# DEPRECATED sReplicationJobTerminated "Use generic-lens or generic-optics with 'replicationJobTerminated' instead." #-}

-- | Information about the VM server.
--
-- /Note:/ Consider using 'vmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVmServer :: Lens.Lens' Server (Lude.Maybe VMServer)
sVmServer = Lens.lens (vmServer :: Server -> Lude.Maybe VMServer) (\s a -> s {vmServer = a} :: Server)
{-# DEPRECATED sVmServer "Use generic-lens or generic-optics with 'vmServer' instead." #-}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationJobId :: Lens.Lens' Server (Lude.Maybe Lude.Text)
sReplicationJobId = Lens.lens (replicationJobId :: Server -> Lude.Maybe Lude.Text) (\s a -> s {replicationJobId = a} :: Server)
{-# DEPRECATED sReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

instance Lude.FromJSON Server where
  parseJSON =
    Lude.withObject
      "Server"
      ( \x ->
          Server'
            Lude.<$> (x Lude..:? "serverType")
            Lude.<*> (x Lude..:? "serverId")
            Lude.<*> (x Lude..:? "replicationJobTerminated")
            Lude.<*> (x Lude..:? "vmServer")
            Lude.<*> (x Lude..:? "replicationJobId")
      )

instance Lude.ToJSON Server where
  toJSON Server' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverType" Lude..=) Lude.<$> serverType,
            ("serverId" Lude..=) Lude.<$> serverId,
            ("replicationJobTerminated" Lude..=)
              Lude.<$> replicationJobTerminated,
            ("vmServer" Lude..=) Lude.<$> vmServer,
            ("replicationJobId" Lude..=) Lude.<$> replicationJobId
          ]
      )

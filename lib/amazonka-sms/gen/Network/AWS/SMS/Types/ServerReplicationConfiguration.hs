{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerReplicationConfiguration
  ( ServerReplicationConfiguration (..),

    -- * Smart constructor
    mkServerReplicationConfiguration,

    -- * Lenses
    srcServer,
    srcServerReplicationParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Server as Types
import qualified Network.AWS.SMS.Types.ServerReplicationParameters as Types

-- | Replication configuration of a server.
--
-- /See:/ 'mkServerReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { -- | The ID of the server with which this replication configuration is associated.
    server :: Core.Maybe Types.Server,
    -- | The parameters for replicating the server.
    serverReplicationParameters :: Core.Maybe Types.ServerReplicationParameters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServerReplicationConfiguration' value with any optional fields omitted.
mkServerReplicationConfiguration ::
  ServerReplicationConfiguration
mkServerReplicationConfiguration =
  ServerReplicationConfiguration'
    { server = Core.Nothing,
      serverReplicationParameters = Core.Nothing
    }

-- | The ID of the server with which this replication configuration is associated.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcServer :: Lens.Lens' ServerReplicationConfiguration (Core.Maybe Types.Server)
srcServer = Lens.field @"server"
{-# DEPRECATED srcServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The parameters for replicating the server.
--
-- /Note:/ Consider using 'serverReplicationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcServerReplicationParameters :: Lens.Lens' ServerReplicationConfiguration (Core.Maybe Types.ServerReplicationParameters)
srcServerReplicationParameters = Lens.field @"serverReplicationParameters"
{-# DEPRECATED srcServerReplicationParameters "Use generic-lens or generic-optics with 'serverReplicationParameters' instead." #-}

instance Core.FromJSON ServerReplicationConfiguration where
  toJSON ServerReplicationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("server" Core..=) Core.<$> server,
            ("serverReplicationParameters" Core..=)
              Core.<$> serverReplicationParameters
          ]
      )

instance Core.FromJSON ServerReplicationConfiguration where
  parseJSON =
    Core.withObject "ServerReplicationConfiguration" Core.$
      \x ->
        ServerReplicationConfiguration'
          Core.<$> (x Core..:? "server")
          Core.<*> (x Core..:? "serverReplicationParameters")

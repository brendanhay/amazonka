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
    srcServerReplicationParameters,
    srcServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerReplicationParameters

-- | Replication configuration of a server.
--
-- /See:/ 'mkServerReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { serverReplicationParameters ::
      Lude.Maybe
        ServerReplicationParameters,
    server :: Lude.Maybe Server
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerReplicationConfiguration' with the minimum fields required to make a request.
--
-- * 'server' - The ID of the server with which this replication configuration is associated.
-- * 'serverReplicationParameters' - The parameters for replicating the server.
mkServerReplicationConfiguration ::
  ServerReplicationConfiguration
mkServerReplicationConfiguration =
  ServerReplicationConfiguration'
    { serverReplicationParameters =
        Lude.Nothing,
      server = Lude.Nothing
    }

-- | The parameters for replicating the server.
--
-- /Note:/ Consider using 'serverReplicationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcServerReplicationParameters :: Lens.Lens' ServerReplicationConfiguration (Lude.Maybe ServerReplicationParameters)
srcServerReplicationParameters = Lens.lens (serverReplicationParameters :: ServerReplicationConfiguration -> Lude.Maybe ServerReplicationParameters) (\s a -> s {serverReplicationParameters = a} :: ServerReplicationConfiguration)
{-# DEPRECATED srcServerReplicationParameters "Use generic-lens or generic-optics with 'serverReplicationParameters' instead." #-}

-- | The ID of the server with which this replication configuration is associated.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcServer :: Lens.Lens' ServerReplicationConfiguration (Lude.Maybe Server)
srcServer = Lens.lens (server :: ServerReplicationConfiguration -> Lude.Maybe Server) (\s a -> s {server = a} :: ServerReplicationConfiguration)
{-# DEPRECATED srcServer "Use generic-lens or generic-optics with 'server' instead." #-}

instance Lude.FromJSON ServerReplicationConfiguration where
  parseJSON =
    Lude.withObject
      "ServerReplicationConfiguration"
      ( \x ->
          ServerReplicationConfiguration'
            Lude.<$> (x Lude..:? "serverReplicationParameters")
            Lude.<*> (x Lude..:? "server")
      )

instance Lude.ToJSON ServerReplicationConfiguration where
  toJSON ServerReplicationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverReplicationParameters" Lude..=)
              Lude.<$> serverReplicationParameters,
            ("server" Lude..=) Lude.<$> server
          ]
      )

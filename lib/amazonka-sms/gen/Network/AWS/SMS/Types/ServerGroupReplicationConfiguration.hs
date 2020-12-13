{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
  ( ServerGroupReplicationConfiguration (..),

    -- * Smart constructor
    mkServerGroupReplicationConfiguration,

    -- * Lenses
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ServerReplicationConfiguration

-- | Replication configuration for a server group.
--
-- /See:/ 'mkServerGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { -- | The ID of the server group with which this replication configuration is associated.
    serverGroupId :: Lude.Maybe Lude.Text,
    -- | The replication configuration for servers in the server group.
    serverReplicationConfigurations :: Lude.Maybe [ServerReplicationConfiguration]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerGroupReplicationConfiguration' with the minimum fields required to make a request.
--
-- * 'serverGroupId' - The ID of the server group with which this replication configuration is associated.
-- * 'serverReplicationConfigurations' - The replication configuration for servers in the server group.
mkServerGroupReplicationConfiguration ::
  ServerGroupReplicationConfiguration
mkServerGroupReplicationConfiguration =
  ServerGroupReplicationConfiguration'
    { serverGroupId =
        Lude.Nothing,
      serverReplicationConfigurations = Lude.Nothing
    }

-- | The ID of the server group with which this replication configuration is associated.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrcServerGroupId :: Lens.Lens' ServerGroupReplicationConfiguration (Lude.Maybe Lude.Text)
sgrcServerGroupId = Lens.lens (serverGroupId :: ServerGroupReplicationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serverGroupId = a} :: ServerGroupReplicationConfiguration)
{-# DEPRECATED sgrcServerGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead." #-}

-- | The replication configuration for servers in the server group.
--
-- /Note:/ Consider using 'serverReplicationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrcServerReplicationConfigurations :: Lens.Lens' ServerGroupReplicationConfiguration (Lude.Maybe [ServerReplicationConfiguration])
sgrcServerReplicationConfigurations = Lens.lens (serverReplicationConfigurations :: ServerGroupReplicationConfiguration -> Lude.Maybe [ServerReplicationConfiguration]) (\s a -> s {serverReplicationConfigurations = a} :: ServerGroupReplicationConfiguration)
{-# DEPRECATED sgrcServerReplicationConfigurations "Use generic-lens or generic-optics with 'serverReplicationConfigurations' instead." #-}

instance Lude.FromJSON ServerGroupReplicationConfiguration where
  parseJSON =
    Lude.withObject
      "ServerGroupReplicationConfiguration"
      ( \x ->
          ServerGroupReplicationConfiguration'
            Lude.<$> (x Lude..:? "serverGroupId")
            Lude.<*> ( x Lude..:? "serverReplicationConfigurations"
                         Lude..!= Lude.mempty
                     )
      )

instance Lude.ToJSON ServerGroupReplicationConfiguration where
  toJSON ServerGroupReplicationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverGroupId" Lude..=) Lude.<$> serverGroupId,
            ("serverReplicationConfigurations" Lude..=)
              Lude.<$> serverReplicationConfigurations
          ]
      )

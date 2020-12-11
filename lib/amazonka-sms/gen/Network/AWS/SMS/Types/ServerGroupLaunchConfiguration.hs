-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
  ( ServerGroupLaunchConfiguration (..),

    -- * Smart constructor
    mkServerGroupLaunchConfiguration,

    -- * Lenses
    sglcServerGroupId,
    sglcLaunchOrder,
    sglcServerLaunchConfigurations,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ServerLaunchConfiguration

-- | Launch configuration for a server group.
--
-- /See:/ 'mkServerGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { serverGroupId ::
      Lude.Maybe Lude.Text,
    launchOrder ::
      Lude.Maybe Lude.Int,
    serverLaunchConfigurations ::
      Lude.Maybe
        [ServerLaunchConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerGroupLaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'launchOrder' - The launch order of servers in the server group.
-- * 'serverGroupId' - The ID of the server group with which the launch configuration is associated.
-- * 'serverLaunchConfigurations' - The launch configuration for servers in the server group.
mkServerGroupLaunchConfiguration ::
  ServerGroupLaunchConfiguration
mkServerGroupLaunchConfiguration =
  ServerGroupLaunchConfiguration'
    { serverGroupId = Lude.Nothing,
      launchOrder = Lude.Nothing,
      serverLaunchConfigurations = Lude.Nothing
    }

-- | The ID of the server group with which the launch configuration is associated.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcServerGroupId :: Lens.Lens' ServerGroupLaunchConfiguration (Lude.Maybe Lude.Text)
sglcServerGroupId = Lens.lens (serverGroupId :: ServerGroupLaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serverGroupId = a} :: ServerGroupLaunchConfiguration)
{-# DEPRECATED sglcServerGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead." #-}

-- | The launch order of servers in the server group.
--
-- /Note:/ Consider using 'launchOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcLaunchOrder :: Lens.Lens' ServerGroupLaunchConfiguration (Lude.Maybe Lude.Int)
sglcLaunchOrder = Lens.lens (launchOrder :: ServerGroupLaunchConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {launchOrder = a} :: ServerGroupLaunchConfiguration)
{-# DEPRECATED sglcLaunchOrder "Use generic-lens or generic-optics with 'launchOrder' instead." #-}

-- | The launch configuration for servers in the server group.
--
-- /Note:/ Consider using 'serverLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcServerLaunchConfigurations :: Lens.Lens' ServerGroupLaunchConfiguration (Lude.Maybe [ServerLaunchConfiguration])
sglcServerLaunchConfigurations = Lens.lens (serverLaunchConfigurations :: ServerGroupLaunchConfiguration -> Lude.Maybe [ServerLaunchConfiguration]) (\s a -> s {serverLaunchConfigurations = a} :: ServerGroupLaunchConfiguration)
{-# DEPRECATED sglcServerLaunchConfigurations "Use generic-lens or generic-optics with 'serverLaunchConfigurations' instead." #-}

instance Lude.FromJSON ServerGroupLaunchConfiguration where
  parseJSON =
    Lude.withObject
      "ServerGroupLaunchConfiguration"
      ( \x ->
          ServerGroupLaunchConfiguration'
            Lude.<$> (x Lude..:? "serverGroupId")
            Lude.<*> (x Lude..:? "launchOrder")
            Lude.<*> (x Lude..:? "serverLaunchConfigurations" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ServerGroupLaunchConfiguration where
  toJSON ServerGroupLaunchConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serverGroupId" Lude..=) Lude.<$> serverGroupId,
            ("launchOrder" Lude..=) Lude.<$> launchOrder,
            ("serverLaunchConfigurations" Lude..=)
              Lude.<$> serverLaunchConfigurations
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
  ( ServerGroupLaunchConfiguration (..)
  -- * Smart constructor
  , mkServerGroupLaunchConfiguration
  -- * Lenses
  , sglcLaunchOrder
  , sglcServerGroupId
  , sglcServerLaunchConfigurations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ServerGroupId as Types
import qualified Network.AWS.SMS.Types.ServerLaunchConfiguration as Types

-- | Launch configuration for a server group.
--
-- /See:/ 'mkServerGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { launchOrder :: Core.Maybe Core.Int
    -- ^ The launch order of servers in the server group.
  , serverGroupId :: Core.Maybe Types.ServerGroupId
    -- ^ The ID of the server group with which the launch configuration is associated.
  , serverLaunchConfigurations :: Core.Maybe [Types.ServerLaunchConfiguration]
    -- ^ The launch configuration for servers in the server group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerGroupLaunchConfiguration' value with any optional fields omitted.
mkServerGroupLaunchConfiguration
    :: ServerGroupLaunchConfiguration
mkServerGroupLaunchConfiguration
  = ServerGroupLaunchConfiguration'{launchOrder = Core.Nothing,
                                    serverGroupId = Core.Nothing,
                                    serverLaunchConfigurations = Core.Nothing}

-- | The launch order of servers in the server group.
--
-- /Note:/ Consider using 'launchOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcLaunchOrder :: Lens.Lens' ServerGroupLaunchConfiguration (Core.Maybe Core.Int)
sglcLaunchOrder = Lens.field @"launchOrder"
{-# INLINEABLE sglcLaunchOrder #-}
{-# DEPRECATED launchOrder "Use generic-lens or generic-optics with 'launchOrder' instead"  #-}

-- | The ID of the server group with which the launch configuration is associated.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcServerGroupId :: Lens.Lens' ServerGroupLaunchConfiguration (Core.Maybe Types.ServerGroupId)
sglcServerGroupId = Lens.field @"serverGroupId"
{-# INLINEABLE sglcServerGroupId #-}
{-# DEPRECATED serverGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead"  #-}

-- | The launch configuration for servers in the server group.
--
-- /Note:/ Consider using 'serverLaunchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglcServerLaunchConfigurations :: Lens.Lens' ServerGroupLaunchConfiguration (Core.Maybe [Types.ServerLaunchConfiguration])
sglcServerLaunchConfigurations = Lens.field @"serverLaunchConfigurations"
{-# INLINEABLE sglcServerLaunchConfigurations #-}
{-# DEPRECATED serverLaunchConfigurations "Use generic-lens or generic-optics with 'serverLaunchConfigurations' instead"  #-}

instance Core.FromJSON ServerGroupLaunchConfiguration where
        toJSON ServerGroupLaunchConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("launchOrder" Core..=) Core.<$> launchOrder,
                  ("serverGroupId" Core..=) Core.<$> serverGroupId,
                  ("serverLaunchConfigurations" Core..=) Core.<$>
                    serverLaunchConfigurations])

instance Core.FromJSON ServerGroupLaunchConfiguration where
        parseJSON
          = Core.withObject "ServerGroupLaunchConfiguration" Core.$
              \ x ->
                ServerGroupLaunchConfiguration' Core.<$>
                  (x Core..:? "launchOrder") Core.<*> x Core..:? "serverGroupId"
                    Core.<*> x Core..:? "serverLaunchConfigurations"

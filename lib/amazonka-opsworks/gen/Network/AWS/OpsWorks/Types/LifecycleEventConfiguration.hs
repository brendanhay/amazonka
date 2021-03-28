{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
  ( LifecycleEventConfiguration (..)
  -- * Smart constructor
  , mkLifecycleEventConfiguration
  -- * Lenses
  , lecShutdown
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.ShutdownEventConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'mkLifecycleEventConfiguration' smart constructor.
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
  { shutdown :: Core.Maybe Types.ShutdownEventConfiguration
    -- ^ A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleEventConfiguration' value with any optional fields omitted.
mkLifecycleEventConfiguration
    :: LifecycleEventConfiguration
mkLifecycleEventConfiguration
  = LifecycleEventConfiguration'{shutdown = Core.Nothing}

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- /Note:/ Consider using 'shutdown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecShutdown :: Lens.Lens' LifecycleEventConfiguration (Core.Maybe Types.ShutdownEventConfiguration)
lecShutdown = Lens.field @"shutdown"
{-# INLINEABLE lecShutdown #-}
{-# DEPRECATED shutdown "Use generic-lens or generic-optics with 'shutdown' instead"  #-}

instance Core.FromJSON LifecycleEventConfiguration where
        toJSON LifecycleEventConfiguration{..}
          = Core.object
              (Core.catMaybes [("Shutdown" Core..=) Core.<$> shutdown])

instance Core.FromJSON LifecycleEventConfiguration where
        parseJSON
          = Core.withObject "LifecycleEventConfiguration" Core.$
              \ x ->
                LifecycleEventConfiguration' Core.<$> (x Core..:? "Shutdown")

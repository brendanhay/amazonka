{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
  ( AutoRollbackConfiguration (..),

    -- * Smart constructor
    mkAutoRollbackConfiguration,

    -- * Lenses
    arcEnabled,
    arcEvents,
  )
where

import qualified Network.AWS.CodeDeploy.Types.AutoRollbackEvent as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment is not completed successfully.
--
-- /See:/ 'mkAutoRollbackConfiguration' smart constructor.
data AutoRollbackConfiguration = AutoRollbackConfiguration'
  { -- | Indicates whether a defined automatic rollback configuration is currently enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The event type or types that trigger a rollback.
    events :: Core.Maybe [Types.AutoRollbackEvent]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoRollbackConfiguration' value with any optional fields omitted.
mkAutoRollbackConfiguration ::
  AutoRollbackConfiguration
mkAutoRollbackConfiguration =
  AutoRollbackConfiguration'
    { enabled = Core.Nothing,
      events = Core.Nothing
    }

-- | Indicates whether a defined automatic rollback configuration is currently enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcEnabled :: Lens.Lens' AutoRollbackConfiguration (Core.Maybe Core.Bool)
arcEnabled = Lens.field @"enabled"
{-# DEPRECATED arcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The event type or types that trigger a rollback.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcEvents :: Lens.Lens' AutoRollbackConfiguration (Core.Maybe [Types.AutoRollbackEvent])
arcEvents = Lens.field @"events"
{-# DEPRECATED arcEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Core.FromJSON AutoRollbackConfiguration where
  toJSON AutoRollbackConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("enabled" Core..=) Core.<$> enabled,
            ("events" Core..=) Core.<$> events
          ]
      )

instance Core.FromJSON AutoRollbackConfiguration where
  parseJSON =
    Core.withObject "AutoRollbackConfiguration" Core.$
      \x ->
        AutoRollbackConfiguration'
          Core.<$> (x Core..:? "enabled") Core.<*> (x Core..:? "events")

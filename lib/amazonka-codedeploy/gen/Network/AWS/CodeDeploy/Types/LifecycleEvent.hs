{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEvent
  ( LifecycleEvent (..),

    -- * Smart constructor
    mkLifecycleEvent,

    -- * Lenses
    leDiagnostics,
    leEndTime,
    leLifecycleEventName,
    leStartTime,
    leStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types.Diagnostics as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleEventName as Types
import qualified Network.AWS.CodeDeploy.Types.LifecycleEventStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment lifecycle event.
--
-- /See:/ 'mkLifecycleEvent' smart constructor.
data LifecycleEvent = LifecycleEvent'
  { -- | Diagnostic information about the deployment lifecycle event.
    diagnostics :: Core.Maybe Types.Diagnostics,
    -- | A timestamp that indicates when the deployment lifecycle event ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
    lifecycleEventName :: Core.Maybe Types.LifecycleEventName,
    -- | A timestamp that indicates when the deployment lifecycle event started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The deployment lifecycle event status:
    --
    --
    --     * Pending: The deployment lifecycle event is pending.
    --
    --
    --     * InProgress: The deployment lifecycle event is in progress.
    --
    --
    --     * Succeeded: The deployment lifecycle event ran successfully.
    --
    --
    --     * Failed: The deployment lifecycle event has failed.
    --
    --
    --     * Skipped: The deployment lifecycle event has been skipped.
    --
    --
    --     * Unknown: The deployment lifecycle event is unknown.
    status :: Core.Maybe Types.LifecycleEventStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LifecycleEvent' value with any optional fields omitted.
mkLifecycleEvent ::
  LifecycleEvent
mkLifecycleEvent =
  LifecycleEvent'
    { diagnostics = Core.Nothing,
      endTime = Core.Nothing,
      lifecycleEventName = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

-- | Diagnostic information about the deployment lifecycle event.
--
-- /Note:/ Consider using 'diagnostics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leDiagnostics :: Lens.Lens' LifecycleEvent (Core.Maybe Types.Diagnostics)
leDiagnostics = Lens.field @"diagnostics"
{-# DEPRECATED leDiagnostics "Use generic-lens or generic-optics with 'diagnostics' instead." #-}

-- | A timestamp that indicates when the deployment lifecycle event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leEndTime :: Lens.Lens' LifecycleEvent (Core.Maybe Core.NominalDiffTime)
leEndTime = Lens.field @"endTime"
{-# DEPRECATED leEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
--
-- /Note:/ Consider using 'lifecycleEventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leLifecycleEventName :: Lens.Lens' LifecycleEvent (Core.Maybe Types.LifecycleEventName)
leLifecycleEventName = Lens.field @"lifecycleEventName"
{-# DEPRECATED leLifecycleEventName "Use generic-lens or generic-optics with 'lifecycleEventName' instead." #-}

-- | A timestamp that indicates when the deployment lifecycle event started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStartTime :: Lens.Lens' LifecycleEvent (Core.Maybe Core.NominalDiffTime)
leStartTime = Lens.field @"startTime"
{-# DEPRECATED leStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The deployment lifecycle event status:
--
--
--     * Pending: The deployment lifecycle event is pending.
--
--
--     * InProgress: The deployment lifecycle event is in progress.
--
--
--     * Succeeded: The deployment lifecycle event ran successfully.
--
--
--     * Failed: The deployment lifecycle event has failed.
--
--
--     * Skipped: The deployment lifecycle event has been skipped.
--
--
--     * Unknown: The deployment lifecycle event is unknown.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leStatus :: Lens.Lens' LifecycleEvent (Core.Maybe Types.LifecycleEventStatus)
leStatus = Lens.field @"status"
{-# DEPRECATED leStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON LifecycleEvent where
  parseJSON =
    Core.withObject "LifecycleEvent" Core.$
      \x ->
        LifecycleEvent'
          Core.<$> (x Core..:? "diagnostics")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "lifecycleEventName")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "status")

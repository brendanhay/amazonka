{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingMaintenanceAction
  ( PendingMaintenanceAction (..),

    -- * Smart constructor
    mkPendingMaintenanceAction,

    -- * Lenses
    pmaAction,
    pmaAutoAppliedAfterDate,
    pmaCurrentApplyDate,
    pmaDescription,
    pmaForcedApplyDate,
    pmaOptInStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Action as Types
import qualified Network.AWS.RDS.Types.Description as Types
import qualified Network.AWS.RDS.Types.OptInStatus as Types

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
    action :: Core.Maybe Types.Action,
    -- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
    autoAppliedAfterDate :: Core.Maybe Core.UTCTime,
    -- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
    currentApplyDate :: Core.Maybe Core.UTCTime,
    -- | A description providing more detail about the maintenance action.
    description :: Core.Maybe Types.Description,
    -- | The date when the maintenance action is automatically applied.
    --
    -- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
    forcedApplyDate :: Core.Maybe Core.UTCTime,
    -- | Indicates the type of opt-in request that has been received for the resource.
    optInStatus :: Core.Maybe Types.OptInStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PendingMaintenanceAction' value with any optional fields omitted.
mkPendingMaintenanceAction ::
  PendingMaintenanceAction
mkPendingMaintenanceAction =
  PendingMaintenanceAction'
    { action = Core.Nothing,
      autoAppliedAfterDate = Core.Nothing,
      currentApplyDate = Core.Nothing,
      description = Core.Nothing,
      forcedApplyDate = Core.Nothing,
      optInStatus = Core.Nothing
    }

-- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Types.Action)
pmaAction = Lens.field @"action"
{-# DEPRECATED pmaAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
--
-- /Note:/ Consider using 'autoAppliedAfterDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAutoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaAutoAppliedAfterDate = Lens.field @"autoAppliedAfterDate"
{-# DEPRECATED pmaAutoAppliedAfterDate "Use generic-lens or generic-optics with 'autoAppliedAfterDate' instead." #-}

-- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaCurrentApplyDate = Lens.field @"currentApplyDate"
{-# DEPRECATED pmaCurrentApplyDate "Use generic-lens or generic-optics with 'currentApplyDate' instead." #-}

-- | A description providing more detail about the maintenance action.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDescription :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Types.Description)
pmaDescription = Lens.field @"description"
{-# DEPRECATED pmaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date when the maintenance action is automatically applied.
--
-- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
--
-- /Note:/ Consider using 'forcedApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaForcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaForcedApplyDate = Lens.field @"forcedApplyDate"
{-# DEPRECATED pmaForcedApplyDate "Use generic-lens or generic-optics with 'forcedApplyDate' instead." #-}

-- | Indicates the type of opt-in request that has been received for the resource.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOptInStatus :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Types.OptInStatus)
pmaOptInStatus = Lens.field @"optInStatus"
{-# DEPRECATED pmaOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

instance Core.FromXML PendingMaintenanceAction where
  parseXML x =
    PendingMaintenanceAction'
      Core.<$> (x Core..@? "Action")
      Core.<*> (x Core..@? "AutoAppliedAfterDate")
      Core.<*> (x Core..@? "CurrentApplyDate")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ForcedApplyDate")
      Core.<*> (x Core..@? "OptInStatus")

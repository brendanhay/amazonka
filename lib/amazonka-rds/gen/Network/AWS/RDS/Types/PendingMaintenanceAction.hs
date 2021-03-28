{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.PendingMaintenanceAction
  ( PendingMaintenanceAction (..)
  -- * Smart constructor
  , mkPendingMaintenanceAction
  -- * Lenses
  , pmaAction
  , pmaAutoAppliedAfterDate
  , pmaCurrentApplyDate
  , pmaDescription
  , pmaForcedApplyDate
  , pmaOptInStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { action :: Core.Maybe Core.Text
    -- ^ The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
  , autoAppliedAfterDate :: Core.Maybe Core.UTCTime
    -- ^ The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
  , currentApplyDate :: Core.Maybe Core.UTCTime
    -- ^ The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
  , description :: Core.Maybe Core.Text
    -- ^ A description providing more detail about the maintenance action.
  , forcedApplyDate :: Core.Maybe Core.UTCTime
    -- ^ The date when the maintenance action is automatically applied.
--
-- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
  , optInStatus :: Core.Maybe Core.Text
    -- ^ Indicates the type of opt-in request that has been received for the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PendingMaintenanceAction' value with any optional fields omitted.
mkPendingMaintenanceAction
    :: PendingMaintenanceAction
mkPendingMaintenanceAction
  = PendingMaintenanceAction'{action = Core.Nothing,
                              autoAppliedAfterDate = Core.Nothing,
                              currentApplyDate = Core.Nothing, description = Core.Nothing,
                              forcedApplyDate = Core.Nothing, optInStatus = Core.Nothing}

-- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pmaAction = Lens.field @"action"
{-# INLINEABLE pmaAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
--
-- /Note:/ Consider using 'autoAppliedAfterDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAutoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaAutoAppliedAfterDate = Lens.field @"autoAppliedAfterDate"
{-# INLINEABLE pmaAutoAppliedAfterDate #-}
{-# DEPRECATED autoAppliedAfterDate "Use generic-lens or generic-optics with 'autoAppliedAfterDate' instead"  #-}

-- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaCurrentApplyDate = Lens.field @"currentApplyDate"
{-# INLINEABLE pmaCurrentApplyDate #-}
{-# DEPRECATED currentApplyDate "Use generic-lens or generic-optics with 'currentApplyDate' instead"  #-}

-- | A description providing more detail about the maintenance action.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDescription :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pmaDescription = Lens.field @"description"
{-# INLINEABLE pmaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The date when the maintenance action is automatically applied.
--
-- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
--
-- /Note:/ Consider using 'forcedApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaForcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.UTCTime)
pmaForcedApplyDate = Lens.field @"forcedApplyDate"
{-# INLINEABLE pmaForcedApplyDate #-}
{-# DEPRECATED forcedApplyDate "Use generic-lens or generic-optics with 'forcedApplyDate' instead"  #-}

-- | Indicates the type of opt-in request that has been received for the resource.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOptInStatus :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pmaOptInStatus = Lens.field @"optInStatus"
{-# INLINEABLE pmaOptInStatus #-}
{-# DEPRECATED optInStatus "Use generic-lens or generic-optics with 'optInStatus' instead"  #-}

instance Core.FromXML PendingMaintenanceAction where
        parseXML x
          = PendingMaintenanceAction' Core.<$>
              (x Core..@? "Action") Core.<*> x Core..@? "AutoAppliedAfterDate"
                Core.<*> x Core..@? "CurrentApplyDate"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "ForcedApplyDate"
                Core.<*> x Core..@? "OptInStatus"

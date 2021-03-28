{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.PendingMaintenanceAction
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

-- | Describes a maintenance action pending for an AWS DMS resource, including when and how it will be applied. This data type is a response element to the @DescribePendingMaintenanceActions@ operation.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { action :: Core.Maybe Core.Text
    -- ^ The type of pending maintenance action that is available for the resource.
  , autoAppliedAfterDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date of the maintenance window when the action is to be applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
  , currentApplyDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The effective date when the pending maintenance action will be applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API operation, and also the @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This value is blank if an opt-in request has not been received and nothing has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
  , description :: Core.Maybe Core.Text
    -- ^ A description providing more detail about the maintenance action.
  , forcedApplyDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the maintenance action will be automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
  , optInStatus :: Core.Maybe Core.Text
    -- ^ The type of opt-in request that has been received for the resource.
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

-- | The type of pending maintenance action that is available for the resource.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pmaAction = Lens.field @"action"
{-# INLINEABLE pmaAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The date of the maintenance window when the action is to be applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
--
-- /Note:/ Consider using 'autoAppliedAfterDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAutoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.NominalDiffTime)
pmaAutoAppliedAfterDate = Lens.field @"autoAppliedAfterDate"
{-# INLINEABLE pmaAutoAppliedAfterDate #-}
{-# DEPRECATED autoAppliedAfterDate "Use generic-lens or generic-optics with 'autoAppliedAfterDate' instead"  #-}

-- | The effective date when the pending maintenance action will be applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API operation, and also the @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This value is blank if an opt-in request has not been received and nothing has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.NominalDiffTime)
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

-- | The date when the maintenance action will be automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
--
-- /Note:/ Consider using 'forcedApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaForcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.NominalDiffTime)
pmaForcedApplyDate = Lens.field @"forcedApplyDate"
{-# INLINEABLE pmaForcedApplyDate #-}
{-# DEPRECATED forcedApplyDate "Use generic-lens or generic-optics with 'forcedApplyDate' instead"  #-}

-- | The type of opt-in request that has been received for the resource.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOptInStatus :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.Text)
pmaOptInStatus = Lens.field @"optInStatus"
{-# INLINEABLE pmaOptInStatus #-}
{-# DEPRECATED optInStatus "Use generic-lens or generic-optics with 'optInStatus' instead"  #-}

instance Core.FromJSON PendingMaintenanceAction where
        parseJSON
          = Core.withObject "PendingMaintenanceAction" Core.$
              \ x ->
                PendingMaintenanceAction' Core.<$>
                  (x Core..:? "Action") Core.<*> x Core..:? "AutoAppliedAfterDate"
                    Core.<*> x Core..:? "CurrentApplyDate"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "ForcedApplyDate"
                    Core.<*> x Core..:? "OptInStatus"

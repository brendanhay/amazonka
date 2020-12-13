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
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { -- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
    autoAppliedAfterDate :: Lude.Maybe Lude.DateTime,
    -- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
    action :: Lude.Maybe Lude.Text,
    -- | Indicates the type of opt-in request that has been received for the resource.
    optInStatus :: Lude.Maybe Lude.Text,
    -- | A description providing more detail about the maintenance action.
    description :: Lude.Maybe Lude.Text,
    -- | The date when the maintenance action is automatically applied.
    --
    -- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
    forcedApplyDate :: Lude.Maybe Lude.DateTime,
    -- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
    currentApplyDate :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- * 'autoAppliedAfterDate' - The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
-- * 'action' - The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
-- * 'optInStatus' - Indicates the type of opt-in request that has been received for the resource.
-- * 'description' - A description providing more detail about the maintenance action.
-- * 'forcedApplyDate' - The date when the maintenance action is automatically applied.
--
-- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
-- * 'currentApplyDate' - The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
mkPendingMaintenanceAction ::
  PendingMaintenanceAction
mkPendingMaintenanceAction =
  PendingMaintenanceAction'
    { autoAppliedAfterDate = Lude.Nothing,
      action = Lude.Nothing,
      optInStatus = Lude.Nothing,
      description = Lude.Nothing,
      forcedApplyDate = Lude.Nothing,
      currentApplyDate = Lude.Nothing
    }

-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
--
-- /Note:/ Consider using 'autoAppliedAfterDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAutoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.DateTime)
pmaAutoAppliedAfterDate = Lens.lens (autoAppliedAfterDate :: PendingMaintenanceAction -> Lude.Maybe Lude.DateTime) (\s a -> s {autoAppliedAfterDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaAutoAppliedAfterDate "Use generic-lens or generic-optics with 'autoAppliedAfterDate' instead." #-}

-- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaAction = Lens.lens (action :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Indicates the type of opt-in request that has been received for the resource.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaOptInStatus :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaOptInStatus = Lens.lens (optInStatus :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {optInStatus = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

-- | A description providing more detail about the maintenance action.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDescription :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaDescription = Lens.lens (description :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date when the maintenance action is automatically applied.
--
-- On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
--
-- /Note:/ Consider using 'forcedApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaForcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.DateTime)
pmaForcedApplyDate = Lens.lens (forcedApplyDate :: PendingMaintenanceAction -> Lude.Maybe Lude.DateTime) (\s a -> s {forcedApplyDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaForcedApplyDate "Use generic-lens or generic-optics with 'forcedApplyDate' instead." #-}

-- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.DateTime)
pmaCurrentApplyDate = Lens.lens (currentApplyDate :: PendingMaintenanceAction -> Lude.Maybe Lude.DateTime) (\s a -> s {currentApplyDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaCurrentApplyDate "Use generic-lens or generic-optics with 'currentApplyDate' instead." #-}

instance Lude.FromXML PendingMaintenanceAction where
  parseXML x =
    PendingMaintenanceAction'
      Lude.<$> (x Lude..@? "AutoAppliedAfterDate")
      Lude.<*> (x Lude..@? "Action")
      Lude.<*> (x Lude..@? "OptInStatus")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "ForcedApplyDate")
      Lude.<*> (x Lude..@? "CurrentApplyDate")

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.PendingMaintenanceAction
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

-- | Describes a maintenance action pending for an AWS DMS resource, including when and how it will be applied. This data type is a response element to the @DescribePendingMaintenanceActions@ operation.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { autoAppliedAfterDate ::
      Lude.Maybe Lude.Timestamp,
    action :: Lude.Maybe Lude.Text,
    optInStatus :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    forcedApplyDate ::
      Lude.Maybe Lude.Timestamp,
    currentApplyDate ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- * 'action' - The type of pending maintenance action that is available for the resource.
-- * 'autoAppliedAfterDate' - The date of the maintenance window when the action is to be applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
-- * 'currentApplyDate' - The effective date when the pending maintenance action will be applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API operation, and also the @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This value is blank if an opt-in request has not been received and nothing has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
-- * 'description' - A description providing more detail about the maintenance action.
-- * 'forcedApplyDate' - The date when the maintenance action will be automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
-- * 'optInStatus' - The type of opt-in request that has been received for the resource.
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

-- | The date of the maintenance window when the action is to be applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
--
-- /Note:/ Consider using 'autoAppliedAfterDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAutoAppliedAfterDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Timestamp)
pmaAutoAppliedAfterDate = Lens.lens (autoAppliedAfterDate :: PendingMaintenanceAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {autoAppliedAfterDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaAutoAppliedAfterDate "Use generic-lens or generic-optics with 'autoAppliedAfterDate' instead." #-}

-- | The type of pending maintenance action that is available for the resource.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaAction = Lens.lens (action :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The type of opt-in request that has been received for the resource.
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

-- | The date when the maintenance action will be automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
--
-- /Note:/ Consider using 'forcedApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaForcedApplyDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Timestamp)
pmaForcedApplyDate = Lens.lens (forcedApplyDate :: PendingMaintenanceAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {forcedApplyDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaForcedApplyDate "Use generic-lens or generic-optics with 'forcedApplyDate' instead." #-}

-- | The effective date when the pending maintenance action will be applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API operation, and also the @AutoAppliedAfterDate@ and @ForcedApplyDate@ parameter values. This value is blank if an opt-in request has not been received and nothing has been specified for @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Timestamp)
pmaCurrentApplyDate = Lens.lens (currentApplyDate :: PendingMaintenanceAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {currentApplyDate = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaCurrentApplyDate "Use generic-lens or generic-optics with 'currentApplyDate' instead." #-}

instance Lude.FromJSON PendingMaintenanceAction where
  parseJSON =
    Lude.withObject
      "PendingMaintenanceAction"
      ( \x ->
          PendingMaintenanceAction'
            Lude.<$> (x Lude..:? "AutoAppliedAfterDate")
            Lude.<*> (x Lude..:? "Action")
            Lude.<*> (x Lude..:? "OptInStatus")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ForcedApplyDate")
            Lude.<*> (x Lude..:? "CurrentApplyDate")
      )

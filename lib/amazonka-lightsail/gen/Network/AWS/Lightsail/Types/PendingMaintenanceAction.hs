-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingMaintenanceAction
  ( PendingMaintenanceAction (..),

    -- * Smart constructor
    mkPendingMaintenanceAction,

    -- * Lenses
    pmaAction,
    pmaDescription,
    pmaCurrentApplyDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a pending database maintenance action.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { action ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
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
-- * 'action' - The type of pending database maintenance action.
-- * 'currentApplyDate' - The effective date of the pending database maintenance action.
-- * 'description' - Additional detail about the pending database maintenance action.
mkPendingMaintenanceAction ::
  PendingMaintenanceAction
mkPendingMaintenanceAction =
  PendingMaintenanceAction'
    { action = Lude.Nothing,
      description = Lude.Nothing,
      currentApplyDate = Lude.Nothing
    }

-- | The type of pending database maintenance action.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaAction = Lens.lens (action :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Additional detail about the pending database maintenance action.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDescription :: Lens.Lens' PendingMaintenanceAction (Lude.Maybe Lude.Text)
pmaDescription = Lens.lens (description :: PendingMaintenanceAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PendingMaintenanceAction)
{-# DEPRECATED pmaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The effective date of the pending database maintenance action.
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
            Lude.<$> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "currentApplyDate")
      )

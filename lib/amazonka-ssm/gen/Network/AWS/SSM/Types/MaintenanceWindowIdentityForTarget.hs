-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
  ( MaintenanceWindowIdentityForTarget (..),

    -- * Smart constructor
    mkMaintenanceWindowIdentityForTarget,

    -- * Lenses
    mwiftName,
    mwiftWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The maintenance window to which the specified target belongs.
--
-- /See:/ 'mkMaintenanceWindowIdentityForTarget' smart constructor.
data MaintenanceWindowIdentityForTarget = MaintenanceWindowIdentityForTarget'
  { name ::
      Lude.Maybe Lude.Text,
    windowId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowIdentityForTarget' with the minimum fields required to make a request.
--
-- * 'name' - The name of the maintenance window.
-- * 'windowId' - The ID of the maintenance window.
mkMaintenanceWindowIdentityForTarget ::
  MaintenanceWindowIdentityForTarget
mkMaintenanceWindowIdentityForTarget =
  MaintenanceWindowIdentityForTarget'
    { name = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiftName :: Lens.Lens' MaintenanceWindowIdentityForTarget (Lude.Maybe Lude.Text)
mwiftName = Lens.lens (name :: MaintenanceWindowIdentityForTarget -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MaintenanceWindowIdentityForTarget)
{-# DEPRECATED mwiftName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiftWindowId :: Lens.Lens' MaintenanceWindowIdentityForTarget (Lude.Maybe Lude.Text)
mwiftWindowId = Lens.lens (windowId :: MaintenanceWindowIdentityForTarget -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: MaintenanceWindowIdentityForTarget)
{-# DEPRECATED mwiftWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON MaintenanceWindowIdentityForTarget where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowIdentityForTarget"
      ( \x ->
          MaintenanceWindowIdentityForTarget'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "WindowId")
      )

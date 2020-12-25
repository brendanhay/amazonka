{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserSetting
  ( UserSetting (..),

    -- * Smart constructor
    mkUserSetting,

    -- * Lenses
    usAction,
    usPermission,
  )
where

import qualified Network.AWS.AppStream.Types.Action as Types
import qualified Network.AWS.AppStream.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action and whether the action is enabled or disabled for users during their streaming sessions.
--
-- /See:/ 'mkUserSetting' smart constructor.
data UserSetting = UserSetting'
  { -- | The action that is enabled or disabled.
    action :: Types.Action,
    -- | Indicates whether the action is enabled or disabled.
    permission :: Types.Permission
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserSetting' value with any optional fields omitted.
mkUserSetting ::
  -- | 'action'
  Types.Action ->
  -- | 'permission'
  Types.Permission ->
  UserSetting
mkUserSetting action permission = UserSetting' {action, permission}

-- | The action that is enabled or disabled.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAction :: Lens.Lens' UserSetting Types.Action
usAction = Lens.field @"action"
{-# DEPRECATED usAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Indicates whether the action is enabled or disabled.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPermission :: Lens.Lens' UserSetting Types.Permission
usPermission = Lens.field @"permission"
{-# DEPRECATED usPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

instance Core.FromJSON UserSetting where
  toJSON UserSetting {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("Permission" Core..= permission)
          ]
      )

instance Core.FromJSON UserSetting where
  parseJSON =
    Core.withObject "UserSetting" Core.$
      \x ->
        UserSetting'
          Core.<$> (x Core..: "Action") Core.<*> (x Core..: "Permission")

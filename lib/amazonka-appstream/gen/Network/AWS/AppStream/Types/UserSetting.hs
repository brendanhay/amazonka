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

import Network.AWS.AppStream.Types.Action
import Network.AWS.AppStream.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action and whether the action is enabled or disabled for users during their streaming sessions.
--
-- /See:/ 'mkUserSetting' smart constructor.
data UserSetting = UserSetting'
  { action :: Action,
    permission :: Permission
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserSetting' with the minimum fields required to make a request.
--
-- * 'action' - The action that is enabled or disabled.
-- * 'permission' - Indicates whether the action is enabled or disabled.
mkUserSetting ::
  -- | 'action'
  Action ->
  -- | 'permission'
  Permission ->
  UserSetting
mkUserSetting pAction_ pPermission_ =
  UserSetting' {action = pAction_, permission = pPermission_}

-- | The action that is enabled or disabled.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usAction :: Lens.Lens' UserSetting Action
usAction = Lens.lens (action :: UserSetting -> Action) (\s a -> s {action = a} :: UserSetting)
{-# DEPRECATED usAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Indicates whether the action is enabled or disabled.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPermission :: Lens.Lens' UserSetting Permission
usPermission = Lens.lens (permission :: UserSetting -> Permission) (\s a -> s {permission = a} :: UserSetting)
{-# DEPRECATED usPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

instance Lude.FromJSON UserSetting where
  parseJSON =
    Lude.withObject
      "UserSetting"
      ( \x ->
          UserSetting'
            Lude.<$> (x Lude..: "Action") Lude.<*> (x Lude..: "Permission")
      )

instance Lude.ToJSON UserSetting where
  toJSON UserSetting' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("Permission" Lude..= permission)
          ]
      )

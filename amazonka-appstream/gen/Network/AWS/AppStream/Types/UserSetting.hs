{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserSetting where

import Network.AWS.AppStream.Types.Action
import Network.AWS.AppStream.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action and whether the action is enabled or disabled for
-- users during their streaming sessions.
--
-- /See:/ 'newUserSetting' smart constructor.
data UserSetting = UserSetting'
  { -- | The action that is enabled or disabled.
    action :: Action,
    -- | Indicates whether the action is enabled or disabled.
    permission :: Permission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'userSetting_action' - The action that is enabled or disabled.
--
-- 'permission', 'userSetting_permission' - Indicates whether the action is enabled or disabled.
newUserSetting ::
  -- | 'action'
  Action ->
  -- | 'permission'
  Permission ->
  UserSetting
newUserSetting pAction_ pPermission_ =
  UserSetting'
    { action = pAction_,
      permission = pPermission_
    }

-- | The action that is enabled or disabled.
userSetting_action :: Lens.Lens' UserSetting Action
userSetting_action = Lens.lens (\UserSetting' {action} -> action) (\s@UserSetting' {} a -> s {action = a} :: UserSetting)

-- | Indicates whether the action is enabled or disabled.
userSetting_permission :: Lens.Lens' UserSetting Permission
userSetting_permission = Lens.lens (\UserSetting' {permission} -> permission) (\s@UserSetting' {} a -> s {permission = a} :: UserSetting)

instance Prelude.FromJSON UserSetting where
  parseJSON =
    Prelude.withObject
      "UserSetting"
      ( \x ->
          UserSetting'
            Prelude.<$> (x Prelude..: "Action")
            Prelude.<*> (x Prelude..: "Permission")
      )

instance Prelude.Hashable UserSetting

instance Prelude.NFData UserSetting

instance Prelude.ToJSON UserSetting where
  toJSON UserSetting' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just ("Permission" Prelude..= permission)
          ]
      )

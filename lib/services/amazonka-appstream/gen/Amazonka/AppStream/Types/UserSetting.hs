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
-- Module      : Amazonka.AppStream.Types.UserSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.UserSetting where

import Amazonka.AppStream.Types.Action
import Amazonka.AppStream.Types.Permission
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UserSetting where
  parseJSON =
    Data.withObject
      "UserSetting"
      ( \x ->
          UserSetting'
            Prelude.<$> (x Data..: "Action")
            Prelude.<*> (x Data..: "Permission")
      )

instance Prelude.Hashable UserSetting where
  hashWithSalt _salt UserSetting' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` permission

instance Prelude.NFData UserSetting where
  rnf UserSetting' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf permission

instance Data.ToJSON UserSetting where
  toJSON UserSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Permission" Data..= permission)
          ]
      )

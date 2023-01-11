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
-- Module      : Amazonka.Grafana.Types.UpdateInstruction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.UpdateInstruction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types.Role
import Amazonka.Grafana.Types.UpdateAction
import Amazonka.Grafana.Types.User
import qualified Amazonka.Prelude as Prelude

-- | Contains the instructions for one Grafana role permission update in a
-- <https://docs.aws.amazon.com/grafana/latest/APIReference/API_UpdatePermissions.html UpdatePermissions>
-- operation.
--
-- /See:/ 'newUpdateInstruction' smart constructor.
data UpdateInstruction = UpdateInstruction'
  { -- | Specifies whether this update is to add or revoke role permissions.
    action :: UpdateAction,
    -- | The role to add or revoke for the user or the group specified in
    -- @users@.
    role' :: Role,
    -- | A structure that specifies the user or group to add or revoke the role
    -- for.
    users :: [User]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstruction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateInstruction_action' - Specifies whether this update is to add or revoke role permissions.
--
-- 'role'', 'updateInstruction_role' - The role to add or revoke for the user or the group specified in
-- @users@.
--
-- 'users', 'updateInstruction_users' - A structure that specifies the user or group to add or revoke the role
-- for.
newUpdateInstruction ::
  -- | 'action'
  UpdateAction ->
  -- | 'role''
  Role ->
  UpdateInstruction
newUpdateInstruction pAction_ pRole_ =
  UpdateInstruction'
    { action = pAction_,
      role' = pRole_,
      users = Prelude.mempty
    }

-- | Specifies whether this update is to add or revoke role permissions.
updateInstruction_action :: Lens.Lens' UpdateInstruction UpdateAction
updateInstruction_action = Lens.lens (\UpdateInstruction' {action} -> action) (\s@UpdateInstruction' {} a -> s {action = a} :: UpdateInstruction)

-- | The role to add or revoke for the user or the group specified in
-- @users@.
updateInstruction_role :: Lens.Lens' UpdateInstruction Role
updateInstruction_role = Lens.lens (\UpdateInstruction' {role'} -> role') (\s@UpdateInstruction' {} a -> s {role' = a} :: UpdateInstruction)

-- | A structure that specifies the user or group to add or revoke the role
-- for.
updateInstruction_users :: Lens.Lens' UpdateInstruction [User]
updateInstruction_users = Lens.lens (\UpdateInstruction' {users} -> users) (\s@UpdateInstruction' {} a -> s {users = a} :: UpdateInstruction) Prelude.. Lens.coerced

instance Data.FromJSON UpdateInstruction where
  parseJSON =
    Data.withObject
      "UpdateInstruction"
      ( \x ->
          UpdateInstruction'
            Prelude.<$> (x Data..: "action")
            Prelude.<*> (x Data..: "role")
            Prelude.<*> (x Data..:? "users" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UpdateInstruction where
  hashWithSalt _salt UpdateInstruction' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` users

instance Prelude.NFData UpdateInstruction where
  rnf UpdateInstruction' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf users

instance Data.ToJSON UpdateInstruction where
  toJSON UpdateInstruction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("action" Data..= action),
            Prelude.Just ("role" Data..= role'),
            Prelude.Just ("users" Data..= users)
          ]
      )

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
-- Module      : Amazonka.Grafana.Types.PermissionEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.PermissionEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types.Role
import Amazonka.Grafana.Types.User
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the identity of one user or group and the
-- @Admin@, @Editor@, or @Viewer@ role that they have.
--
-- /See:/ 'newPermissionEntry' smart constructor.
data PermissionEntry = PermissionEntry'
  { -- | Specifies whether the user or group has the @Admin@, @Editor@, or
    -- @Viewer@ role.
    role' :: Role,
    -- | A structure with the ID of the user or group with this role.
    user :: User
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'permissionEntry_role' - Specifies whether the user or group has the @Admin@, @Editor@, or
-- @Viewer@ role.
--
-- 'user', 'permissionEntry_user' - A structure with the ID of the user or group with this role.
newPermissionEntry ::
  -- | 'role''
  Role ->
  -- | 'user'
  User ->
  PermissionEntry
newPermissionEntry pRole_ pUser_ =
  PermissionEntry' {role' = pRole_, user = pUser_}

-- | Specifies whether the user or group has the @Admin@, @Editor@, or
-- @Viewer@ role.
permissionEntry_role :: Lens.Lens' PermissionEntry Role
permissionEntry_role = Lens.lens (\PermissionEntry' {role'} -> role') (\s@PermissionEntry' {} a -> s {role' = a} :: PermissionEntry)

-- | A structure with the ID of the user or group with this role.
permissionEntry_user :: Lens.Lens' PermissionEntry User
permissionEntry_user = Lens.lens (\PermissionEntry' {user} -> user) (\s@PermissionEntry' {} a -> s {user = a} :: PermissionEntry)

instance Data.FromJSON PermissionEntry where
  parseJSON =
    Data.withObject
      "PermissionEntry"
      ( \x ->
          PermissionEntry'
            Prelude.<$> (x Data..: "role") Prelude.<*> (x Data..: "user")
      )

instance Prelude.Hashable PermissionEntry where
  hashWithSalt _salt PermissionEntry' {..} =
    _salt `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` user

instance Prelude.NFData PermissionEntry where
  rnf PermissionEntry' {..} =
    Prelude.rnf role' `Prelude.seq` Prelude.rnf user

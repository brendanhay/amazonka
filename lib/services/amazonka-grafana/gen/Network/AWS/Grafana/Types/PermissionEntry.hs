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
-- Module      : Network.AWS.Grafana.Types.PermissionEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Types.PermissionEntry where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types.Role
import Network.AWS.Grafana.Types.User
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing the identity of one user or group and the @Admin@
-- or @Editor@ role that they have.
--
-- /See:/ 'newPermissionEntry' smart constructor.
data PermissionEntry = PermissionEntry'
  { -- | Specifies whether the user or group has the @Admin@ or @Editor@ role.
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
-- 'role'', 'permissionEntry_role' - Specifies whether the user or group has the @Admin@ or @Editor@ role.
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

-- | Specifies whether the user or group has the @Admin@ or @Editor@ role.
permissionEntry_role :: Lens.Lens' PermissionEntry Role
permissionEntry_role = Lens.lens (\PermissionEntry' {role'} -> role') (\s@PermissionEntry' {} a -> s {role' = a} :: PermissionEntry)

-- | A structure with the ID of the user or group with this role.
permissionEntry_user :: Lens.Lens' PermissionEntry User
permissionEntry_user = Lens.lens (\PermissionEntry' {user} -> user) (\s@PermissionEntry' {} a -> s {user = a} :: PermissionEntry)

instance Core.FromJSON PermissionEntry where
  parseJSON =
    Core.withObject
      "PermissionEntry"
      ( \x ->
          PermissionEntry'
            Prelude.<$> (x Core..: "role") Prelude.<*> (x Core..: "user")
      )

instance Prelude.Hashable PermissionEntry

instance Prelude.NFData PermissionEntry

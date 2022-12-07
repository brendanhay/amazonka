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
-- Module      : Amazonka.WorkDocs.Types.PermissionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.PermissionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.RolePermissionType
import Amazonka.WorkDocs.Types.RoleType

-- | Describes the permissions.
--
-- /See:/ 'newPermissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { -- | The type of permissions.
    type' :: Prelude.Maybe RolePermissionType,
    -- | The role of the user.
    role' :: Prelude.Maybe RoleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'permissionInfo_type' - The type of permissions.
--
-- 'role'', 'permissionInfo_role' - The role of the user.
newPermissionInfo ::
  PermissionInfo
newPermissionInfo =
  PermissionInfo'
    { type' = Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | The type of permissions.
permissionInfo_type :: Lens.Lens' PermissionInfo (Prelude.Maybe RolePermissionType)
permissionInfo_type = Lens.lens (\PermissionInfo' {type'} -> type') (\s@PermissionInfo' {} a -> s {type' = a} :: PermissionInfo)

-- | The role of the user.
permissionInfo_role :: Lens.Lens' PermissionInfo (Prelude.Maybe RoleType)
permissionInfo_role = Lens.lens (\PermissionInfo' {role'} -> role') (\s@PermissionInfo' {} a -> s {role' = a} :: PermissionInfo)

instance Data.FromJSON PermissionInfo where
  parseJSON =
    Data.withObject
      "PermissionInfo"
      ( \x ->
          PermissionInfo'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Role")
      )

instance Prelude.Hashable PermissionInfo where
  hashWithSalt _salt PermissionInfo' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` role'

instance Prelude.NFData PermissionInfo where
  rnf PermissionInfo' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf role'

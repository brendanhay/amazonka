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
-- Module      : Network.AWS.WorkDocs.Types.PermissionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.PermissionInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.RolePermissionType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the permissions.
--
-- /See:/ 'newPermissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { -- | The role of the user.
    role' :: Prelude.Maybe RoleType,
    -- | The type of permissions.
    type' :: Prelude.Maybe RolePermissionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PermissionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'permissionInfo_role' - The role of the user.
--
-- 'type'', 'permissionInfo_type' - The type of permissions.
newPermissionInfo ::
  PermissionInfo
newPermissionInfo =
  PermissionInfo'
    { role' = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The role of the user.
permissionInfo_role :: Lens.Lens' PermissionInfo (Prelude.Maybe RoleType)
permissionInfo_role = Lens.lens (\PermissionInfo' {role'} -> role') (\s@PermissionInfo' {} a -> s {role' = a} :: PermissionInfo)

-- | The type of permissions.
permissionInfo_type :: Lens.Lens' PermissionInfo (Prelude.Maybe RolePermissionType)
permissionInfo_type = Lens.lens (\PermissionInfo' {type'} -> type') (\s@PermissionInfo' {} a -> s {type' = a} :: PermissionInfo)

instance Prelude.FromJSON PermissionInfo where
  parseJSON =
    Prelude.withObject
      "PermissionInfo"
      ( \x ->
          PermissionInfo'
            Prelude.<$> (x Prelude..:? "Role")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable PermissionInfo

instance Prelude.NFData PermissionInfo

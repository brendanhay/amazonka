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
-- Module      : Network.AWS.Glue.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PrincipalPermissions where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Permission
import qualified Network.AWS.Lens as Lens

-- | Permissions granted to a principal.
--
-- /See:/ 'newPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { -- | The permissions that are granted to the principal.
    permissions :: Core.Maybe [Permission],
    -- | The principal who is granted permissions.
    principal :: Core.Maybe DataLakePrincipal
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PrincipalPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'principalPermissions_permissions' - The permissions that are granted to the principal.
--
-- 'principal', 'principalPermissions_principal' - The principal who is granted permissions.
newPrincipalPermissions ::
  PrincipalPermissions
newPrincipalPermissions =
  PrincipalPermissions'
    { permissions = Core.Nothing,
      principal = Core.Nothing
    }

-- | The permissions that are granted to the principal.
principalPermissions_permissions :: Lens.Lens' PrincipalPermissions (Core.Maybe [Permission])
principalPermissions_permissions = Lens.lens (\PrincipalPermissions' {permissions} -> permissions) (\s@PrincipalPermissions' {} a -> s {permissions = a} :: PrincipalPermissions) Core.. Lens.mapping Lens._Coerce

-- | The principal who is granted permissions.
principalPermissions_principal :: Lens.Lens' PrincipalPermissions (Core.Maybe DataLakePrincipal)
principalPermissions_principal = Lens.lens (\PrincipalPermissions' {principal} -> principal) (\s@PrincipalPermissions' {} a -> s {principal = a} :: PrincipalPermissions)

instance Core.FromJSON PrincipalPermissions where
  parseJSON =
    Core.withObject
      "PrincipalPermissions"
      ( \x ->
          PrincipalPermissions'
            Core.<$> (x Core..:? "Permissions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Principal")
      )

instance Core.Hashable PrincipalPermissions

instance Core.NFData PrincipalPermissions

instance Core.ToJSON PrincipalPermissions where
  toJSON PrincipalPermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Permissions" Core..=) Core.<$> permissions,
            ("Principal" Core..=) Core.<$> principal
          ]
      )

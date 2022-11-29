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
-- Module      : Amazonka.Glue.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PrincipalPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.DataLakePrincipal
import Amazonka.Glue.Types.Permission
import qualified Amazonka.Prelude as Prelude

-- | Permissions granted to a principal.
--
-- /See:/ 'newPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { -- | The principal who is granted permissions.
    principal :: Prelude.Maybe DataLakePrincipal,
    -- | The permissions that are granted to the principal.
    permissions :: Prelude.Maybe [Permission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrincipalPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'principalPermissions_principal' - The principal who is granted permissions.
--
-- 'permissions', 'principalPermissions_permissions' - The permissions that are granted to the principal.
newPrincipalPermissions ::
  PrincipalPermissions
newPrincipalPermissions =
  PrincipalPermissions'
    { principal = Prelude.Nothing,
      permissions = Prelude.Nothing
    }

-- | The principal who is granted permissions.
principalPermissions_principal :: Lens.Lens' PrincipalPermissions (Prelude.Maybe DataLakePrincipal)
principalPermissions_principal = Lens.lens (\PrincipalPermissions' {principal} -> principal) (\s@PrincipalPermissions' {} a -> s {principal = a} :: PrincipalPermissions)

-- | The permissions that are granted to the principal.
principalPermissions_permissions :: Lens.Lens' PrincipalPermissions (Prelude.Maybe [Permission])
principalPermissions_permissions = Lens.lens (\PrincipalPermissions' {permissions} -> permissions) (\s@PrincipalPermissions' {} a -> s {permissions = a} :: PrincipalPermissions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PrincipalPermissions where
  parseJSON =
    Core.withObject
      "PrincipalPermissions"
      ( \x ->
          PrincipalPermissions'
            Prelude.<$> (x Core..:? "Principal")
            Prelude.<*> (x Core..:? "Permissions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PrincipalPermissions where
  hashWithSalt _salt PrincipalPermissions' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` permissions

instance Prelude.NFData PrincipalPermissions where
  rnf PrincipalPermissions' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf permissions

instance Core.ToJSON PrincipalPermissions where
  toJSON PrincipalPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Principal" Core..=) Prelude.<$> principal,
            ("Permissions" Core..=) Prelude.<$> permissions
          ]
      )

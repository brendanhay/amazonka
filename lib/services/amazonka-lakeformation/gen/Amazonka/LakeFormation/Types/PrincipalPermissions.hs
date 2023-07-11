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
-- Module      : Amazonka.LakeFormation.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.PrincipalPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.Permission
import qualified Amazonka.Prelude as Prelude

-- | Permissions granted to a principal.
--
-- /See:/ 'newPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { -- | The permissions that are granted to the principal.
    permissions :: Prelude.Maybe [Permission],
    -- | The principal who is granted permissions.
    principal :: Prelude.Maybe DataLakePrincipal
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
-- 'permissions', 'principalPermissions_permissions' - The permissions that are granted to the principal.
--
-- 'principal', 'principalPermissions_principal' - The principal who is granted permissions.
newPrincipalPermissions ::
  PrincipalPermissions
newPrincipalPermissions =
  PrincipalPermissions'
    { permissions =
        Prelude.Nothing,
      principal = Prelude.Nothing
    }

-- | The permissions that are granted to the principal.
principalPermissions_permissions :: Lens.Lens' PrincipalPermissions (Prelude.Maybe [Permission])
principalPermissions_permissions = Lens.lens (\PrincipalPermissions' {permissions} -> permissions) (\s@PrincipalPermissions' {} a -> s {permissions = a} :: PrincipalPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The principal who is granted permissions.
principalPermissions_principal :: Lens.Lens' PrincipalPermissions (Prelude.Maybe DataLakePrincipal)
principalPermissions_principal = Lens.lens (\PrincipalPermissions' {principal} -> principal) (\s@PrincipalPermissions' {} a -> s {principal = a} :: PrincipalPermissions)

instance Data.FromJSON PrincipalPermissions where
  parseJSON =
    Data.withObject
      "PrincipalPermissions"
      ( \x ->
          PrincipalPermissions'
            Prelude.<$> (x Data..:? "Permissions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Principal")
      )

instance Prelude.Hashable PrincipalPermissions where
  hashWithSalt _salt PrincipalPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` principal

instance Prelude.NFData PrincipalPermissions where
  rnf PrincipalPermissions' {..} =
    Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf principal

instance Data.ToJSON PrincipalPermissions where
  toJSON PrincipalPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Permissions" Data..=) Prelude.<$> permissions,
            ("Principal" Data..=) Prelude.<$> principal
          ]
      )

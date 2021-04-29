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
-- Module      : Network.AWS.Glue.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PrincipalPermissions where

import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Permissions granted to a principal.
--
-- /See:/ 'newPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { -- | The permissions that are granted to the principal.
    permissions :: Prelude.Maybe [Permission],
    -- | The principal who is granted permissions.
    principal :: Prelude.Maybe DataLakePrincipal
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
principalPermissions_permissions = Lens.lens (\PrincipalPermissions' {permissions} -> permissions) (\s@PrincipalPermissions' {} a -> s {permissions = a} :: PrincipalPermissions) Prelude.. Lens.mapping Prelude._Coerce

-- | The principal who is granted permissions.
principalPermissions_principal :: Lens.Lens' PrincipalPermissions (Prelude.Maybe DataLakePrincipal)
principalPermissions_principal = Lens.lens (\PrincipalPermissions' {principal} -> principal) (\s@PrincipalPermissions' {} a -> s {principal = a} :: PrincipalPermissions)

instance Prelude.FromJSON PrincipalPermissions where
  parseJSON =
    Prelude.withObject
      "PrincipalPermissions"
      ( \x ->
          PrincipalPermissions'
            Prelude.<$> ( x Prelude..:? "Permissions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Principal")
      )

instance Prelude.Hashable PrincipalPermissions

instance Prelude.NFData PrincipalPermissions

instance Prelude.ToJSON PrincipalPermissions where
  toJSON PrincipalPermissions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Permissions" Prelude..=) Prelude.<$> permissions,
            ("Principal" Prelude..=) Prelude.<$> principal
          ]
      )

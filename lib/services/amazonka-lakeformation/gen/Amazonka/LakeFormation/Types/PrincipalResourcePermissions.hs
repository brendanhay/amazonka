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
-- Module      : Amazonka.LakeFormation.Types.PrincipalResourcePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.PrincipalResourcePermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.DetailsMap
import Amazonka.LakeFormation.Types.Permission
import Amazonka.LakeFormation.Types.Resource
import qualified Amazonka.Prelude as Prelude

-- | The permissions granted or revoked on a resource.
--
-- /See:/ 'newPrincipalResourcePermissions' smart constructor.
data PrincipalResourcePermissions = PrincipalResourcePermissions'
  { -- | This attribute can be used to return any additional details of
    -- @PrincipalResourcePermissions@. Currently returns only as a RAM resource
    -- share ARN.
    additionalDetails :: Prelude.Maybe DetailsMap,
    -- | The permissions to be granted or revoked on the resource.
    permissions :: Prelude.Maybe [Permission],
    -- | Indicates whether to grant the ability to grant permissions (as a subset
    -- of permissions granted).
    permissionsWithGrantOption :: Prelude.Maybe [Permission],
    -- | The Data Lake principal to be granted or revoked permissions.
    principal :: Prelude.Maybe DataLakePrincipal,
    -- | The resource where permissions are to be granted or revoked.
    resource :: Prelude.Maybe Resource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrincipalResourcePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDetails', 'principalResourcePermissions_additionalDetails' - This attribute can be used to return any additional details of
-- @PrincipalResourcePermissions@. Currently returns only as a RAM resource
-- share ARN.
--
-- 'permissions', 'principalResourcePermissions_permissions' - The permissions to be granted or revoked on the resource.
--
-- 'permissionsWithGrantOption', 'principalResourcePermissions_permissionsWithGrantOption' - Indicates whether to grant the ability to grant permissions (as a subset
-- of permissions granted).
--
-- 'principal', 'principalResourcePermissions_principal' - The Data Lake principal to be granted or revoked permissions.
--
-- 'resource', 'principalResourcePermissions_resource' - The resource where permissions are to be granted or revoked.
newPrincipalResourcePermissions ::
  PrincipalResourcePermissions
newPrincipalResourcePermissions =
  PrincipalResourcePermissions'
    { additionalDetails =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      permissionsWithGrantOption = Prelude.Nothing,
      principal = Prelude.Nothing,
      resource = Prelude.Nothing
    }

-- | This attribute can be used to return any additional details of
-- @PrincipalResourcePermissions@. Currently returns only as a RAM resource
-- share ARN.
principalResourcePermissions_additionalDetails :: Lens.Lens' PrincipalResourcePermissions (Prelude.Maybe DetailsMap)
principalResourcePermissions_additionalDetails = Lens.lens (\PrincipalResourcePermissions' {additionalDetails} -> additionalDetails) (\s@PrincipalResourcePermissions' {} a -> s {additionalDetails = a} :: PrincipalResourcePermissions)

-- | The permissions to be granted or revoked on the resource.
principalResourcePermissions_permissions :: Lens.Lens' PrincipalResourcePermissions (Prelude.Maybe [Permission])
principalResourcePermissions_permissions = Lens.lens (\PrincipalResourcePermissions' {permissions} -> permissions) (\s@PrincipalResourcePermissions' {} a -> s {permissions = a} :: PrincipalResourcePermissions) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to grant the ability to grant permissions (as a subset
-- of permissions granted).
principalResourcePermissions_permissionsWithGrantOption :: Lens.Lens' PrincipalResourcePermissions (Prelude.Maybe [Permission])
principalResourcePermissions_permissionsWithGrantOption = Lens.lens (\PrincipalResourcePermissions' {permissionsWithGrantOption} -> permissionsWithGrantOption) (\s@PrincipalResourcePermissions' {} a -> s {permissionsWithGrantOption = a} :: PrincipalResourcePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The Data Lake principal to be granted or revoked permissions.
principalResourcePermissions_principal :: Lens.Lens' PrincipalResourcePermissions (Prelude.Maybe DataLakePrincipal)
principalResourcePermissions_principal = Lens.lens (\PrincipalResourcePermissions' {principal} -> principal) (\s@PrincipalResourcePermissions' {} a -> s {principal = a} :: PrincipalResourcePermissions)

-- | The resource where permissions are to be granted or revoked.
principalResourcePermissions_resource :: Lens.Lens' PrincipalResourcePermissions (Prelude.Maybe Resource)
principalResourcePermissions_resource = Lens.lens (\PrincipalResourcePermissions' {resource} -> resource) (\s@PrincipalResourcePermissions' {} a -> s {resource = a} :: PrincipalResourcePermissions)

instance Data.FromJSON PrincipalResourcePermissions where
  parseJSON =
    Data.withObject
      "PrincipalResourcePermissions"
      ( \x ->
          PrincipalResourcePermissions'
            Prelude.<$> (x Data..:? "AdditionalDetails")
            Prelude.<*> (x Data..:? "Permissions" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "PermissionsWithGrantOption"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Principal")
            Prelude.<*> (x Data..:? "Resource")
      )

instance
  Prelude.Hashable
    PrincipalResourcePermissions
  where
  hashWithSalt _salt PrincipalResourcePermissions' {..} =
    _salt `Prelude.hashWithSalt` additionalDetails
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` permissionsWithGrantOption
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource

instance Prelude.NFData PrincipalResourcePermissions where
  rnf PrincipalResourcePermissions' {..} =
    Prelude.rnf additionalDetails
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf permissionsWithGrantOption
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource

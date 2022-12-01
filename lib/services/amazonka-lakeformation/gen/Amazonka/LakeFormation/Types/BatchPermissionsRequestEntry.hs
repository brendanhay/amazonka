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
-- Module      : Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.Permission
import Amazonka.LakeFormation.Types.Resource
import qualified Amazonka.Prelude as Prelude

-- | A permission to a resource granted by batch operation to the principal.
--
-- /See:/ 'newBatchPermissionsRequestEntry' smart constructor.
data BatchPermissionsRequestEntry = BatchPermissionsRequestEntry'
  { -- | The principal to be granted a permission.
    principal :: Prelude.Maybe DataLakePrincipal,
    -- | The permissions to be granted.
    permissions :: Prelude.Maybe [Permission],
    -- | Indicates if the option to pass permissions is granted.
    permissionsWithGrantOption :: Prelude.Maybe [Permission],
    -- | The resource to which the principal is to be granted a permission.
    resource :: Prelude.Maybe Resource,
    -- | A unique identifier for the batch permissions request entry.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPermissionsRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'batchPermissionsRequestEntry_principal' - The principal to be granted a permission.
--
-- 'permissions', 'batchPermissionsRequestEntry_permissions' - The permissions to be granted.
--
-- 'permissionsWithGrantOption', 'batchPermissionsRequestEntry_permissionsWithGrantOption' - Indicates if the option to pass permissions is granted.
--
-- 'resource', 'batchPermissionsRequestEntry_resource' - The resource to which the principal is to be granted a permission.
--
-- 'id', 'batchPermissionsRequestEntry_id' - A unique identifier for the batch permissions request entry.
newBatchPermissionsRequestEntry ::
  -- | 'id'
  Prelude.Text ->
  BatchPermissionsRequestEntry
newBatchPermissionsRequestEntry pId_ =
  BatchPermissionsRequestEntry'
    { principal =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      permissionsWithGrantOption = Prelude.Nothing,
      resource = Prelude.Nothing,
      id = pId_
    }

-- | The principal to be granted a permission.
batchPermissionsRequestEntry_principal :: Lens.Lens' BatchPermissionsRequestEntry (Prelude.Maybe DataLakePrincipal)
batchPermissionsRequestEntry_principal = Lens.lens (\BatchPermissionsRequestEntry' {principal} -> principal) (\s@BatchPermissionsRequestEntry' {} a -> s {principal = a} :: BatchPermissionsRequestEntry)

-- | The permissions to be granted.
batchPermissionsRequestEntry_permissions :: Lens.Lens' BatchPermissionsRequestEntry (Prelude.Maybe [Permission])
batchPermissionsRequestEntry_permissions = Lens.lens (\BatchPermissionsRequestEntry' {permissions} -> permissions) (\s@BatchPermissionsRequestEntry' {} a -> s {permissions = a} :: BatchPermissionsRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if the option to pass permissions is granted.
batchPermissionsRequestEntry_permissionsWithGrantOption :: Lens.Lens' BatchPermissionsRequestEntry (Prelude.Maybe [Permission])
batchPermissionsRequestEntry_permissionsWithGrantOption = Lens.lens (\BatchPermissionsRequestEntry' {permissionsWithGrantOption} -> permissionsWithGrantOption) (\s@BatchPermissionsRequestEntry' {} a -> s {permissionsWithGrantOption = a} :: BatchPermissionsRequestEntry) Prelude.. Lens.mapping Lens.coerced

-- | The resource to which the principal is to be granted a permission.
batchPermissionsRequestEntry_resource :: Lens.Lens' BatchPermissionsRequestEntry (Prelude.Maybe Resource)
batchPermissionsRequestEntry_resource = Lens.lens (\BatchPermissionsRequestEntry' {resource} -> resource) (\s@BatchPermissionsRequestEntry' {} a -> s {resource = a} :: BatchPermissionsRequestEntry)

-- | A unique identifier for the batch permissions request entry.
batchPermissionsRequestEntry_id :: Lens.Lens' BatchPermissionsRequestEntry Prelude.Text
batchPermissionsRequestEntry_id = Lens.lens (\BatchPermissionsRequestEntry' {id} -> id) (\s@BatchPermissionsRequestEntry' {} a -> s {id = a} :: BatchPermissionsRequestEntry)

instance Core.FromJSON BatchPermissionsRequestEntry where
  parseJSON =
    Core.withObject
      "BatchPermissionsRequestEntry"
      ( \x ->
          BatchPermissionsRequestEntry'
            Prelude.<$> (x Core..:? "Principal")
            Prelude.<*> (x Core..:? "Permissions" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "PermissionsWithGrantOption"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Resource")
            Prelude.<*> (x Core..: "Id")
      )

instance
  Prelude.Hashable
    BatchPermissionsRequestEntry
  where
  hashWithSalt _salt BatchPermissionsRequestEntry' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` permissionsWithGrantOption
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` id

instance Prelude.NFData BatchPermissionsRequestEntry where
  rnf BatchPermissionsRequestEntry' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf permissionsWithGrantOption
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf id

instance Core.ToJSON BatchPermissionsRequestEntry where
  toJSON BatchPermissionsRequestEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Principal" Core..=) Prelude.<$> principal,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("PermissionsWithGrantOption" Core..=)
              Prelude.<$> permissionsWithGrantOption,
            ("Resource" Core..=) Prelude.<$> resource,
            Prelude.Just ("Id" Core..= id)
          ]
      )

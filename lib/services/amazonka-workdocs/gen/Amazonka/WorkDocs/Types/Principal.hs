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
-- Module      : Amazonka.WorkDocs.Types.Principal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.PermissionInfo
import Amazonka.WorkDocs.Types.PrincipalType

-- | Describes a resource.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The type of resource.
    type' :: Prelude.Maybe PrincipalType,
    -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The permission information for the resource.
    roles :: Prelude.Maybe [PermissionInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'principal_type' - The type of resource.
--
-- 'id', 'principal_id' - The ID of the resource.
--
-- 'roles', 'principal_roles' - The permission information for the resource.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { type' = Prelude.Nothing,
      id = Prelude.Nothing,
      roles = Prelude.Nothing
    }

-- | The type of resource.
principal_type :: Lens.Lens' Principal (Prelude.Maybe PrincipalType)
principal_type = Lens.lens (\Principal' {type'} -> type') (\s@Principal' {} a -> s {type' = a} :: Principal)

-- | The ID of the resource.
principal_id :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_id = Lens.lens (\Principal' {id} -> id) (\s@Principal' {} a -> s {id = a} :: Principal)

-- | The permission information for the resource.
principal_roles :: Lens.Lens' Principal (Prelude.Maybe [PermissionInfo])
principal_roles = Lens.lens (\Principal' {roles} -> roles) (\s@Principal' {} a -> s {roles = a} :: Principal) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Principal where
  parseJSON =
    Data.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Roles" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` roles

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf roles

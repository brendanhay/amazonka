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
-- Module      : Network.AWS.WorkDocs.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Principal where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.PrincipalType

-- | Describes a resource.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ID of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The permission information for the resource.
    roles :: Prelude.Maybe [PermissionInfo],
    -- | The type of resource.
    type' :: Prelude.Maybe PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'principal_id' - The ID of the resource.
--
-- 'roles', 'principal_roles' - The permission information for the resource.
--
-- 'type'', 'principal_type' - The type of resource.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { id = Prelude.Nothing,
      roles = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ID of the resource.
principal_id :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_id = Lens.lens (\Principal' {id} -> id) (\s@Principal' {} a -> s {id = a} :: Principal)

-- | The permission information for the resource.
principal_roles :: Lens.Lens' Principal (Prelude.Maybe [PermissionInfo])
principal_roles = Lens.lens (\Principal' {roles} -> roles) (\s@Principal' {} a -> s {roles = a} :: Principal) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of resource.
principal_type :: Lens.Lens' Principal (Prelude.Maybe PrincipalType)
principal_type = Lens.lens (\Principal' {type'} -> type') (\s@Principal' {} a -> s {type' = a} :: Principal)

instance Prelude.FromJSON Principal where
  parseJSON =
    Prelude.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Roles" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Principal

instance Prelude.NFData Principal

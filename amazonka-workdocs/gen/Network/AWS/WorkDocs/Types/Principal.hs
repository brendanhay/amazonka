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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.PrincipalType

-- | Describes a resource.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ID of the resource.
    id :: Core.Maybe Core.Text,
    -- | The permission information for the resource.
    roles :: Core.Maybe [PermissionInfo],
    -- | The type of resource.
    type' :: Core.Maybe PrincipalType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { id = Core.Nothing,
      roles = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ID of the resource.
principal_id :: Lens.Lens' Principal (Core.Maybe Core.Text)
principal_id = Lens.lens (\Principal' {id} -> id) (\s@Principal' {} a -> s {id = a} :: Principal)

-- | The permission information for the resource.
principal_roles :: Lens.Lens' Principal (Core.Maybe [PermissionInfo])
principal_roles = Lens.lens (\Principal' {roles} -> roles) (\s@Principal' {} a -> s {roles = a} :: Principal) Core.. Lens.mapping Lens._Coerce

-- | The type of resource.
principal_type :: Lens.Lens' Principal (Core.Maybe PrincipalType)
principal_type = Lens.lens (\Principal' {type'} -> type') (\s@Principal' {} a -> s {type' = a} :: Principal)

instance Core.FromJSON Principal where
  parseJSON =
    Core.withObject
      "Principal"
      ( \x ->
          Principal'
            Core.<$> (x Core..:? "Id")
            Core.<*> (x Core..:? "Roles" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Principal

instance Core.NFData Principal

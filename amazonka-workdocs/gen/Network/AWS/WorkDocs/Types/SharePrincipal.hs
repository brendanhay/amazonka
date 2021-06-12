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
-- Module      : Network.AWS.WorkDocs.Types.SharePrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SharePrincipal where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkDocs.Types.PrincipalType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the recipient type and ID, if available.
--
-- /See:/ 'newSharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { -- | The ID of the recipient.
    id :: Core.Text,
    -- | The type of the recipient.
    type' :: PrincipalType,
    -- | The role of the recipient.
    role' :: RoleType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SharePrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'sharePrincipal_id' - The ID of the recipient.
--
-- 'type'', 'sharePrincipal_type' - The type of the recipient.
--
-- 'role'', 'sharePrincipal_role' - The role of the recipient.
newSharePrincipal ::
  -- | 'id'
  Core.Text ->
  -- | 'type''
  PrincipalType ->
  -- | 'role''
  RoleType ->
  SharePrincipal
newSharePrincipal pId_ pType_ pRole_ =
  SharePrincipal'
    { id = pId_,
      type' = pType_,
      role' = pRole_
    }

-- | The ID of the recipient.
sharePrincipal_id :: Lens.Lens' SharePrincipal Core.Text
sharePrincipal_id = Lens.lens (\SharePrincipal' {id} -> id) (\s@SharePrincipal' {} a -> s {id = a} :: SharePrincipal)

-- | The type of the recipient.
sharePrincipal_type :: Lens.Lens' SharePrincipal PrincipalType
sharePrincipal_type = Lens.lens (\SharePrincipal' {type'} -> type') (\s@SharePrincipal' {} a -> s {type' = a} :: SharePrincipal)

-- | The role of the recipient.
sharePrincipal_role :: Lens.Lens' SharePrincipal RoleType
sharePrincipal_role = Lens.lens (\SharePrincipal' {role'} -> role') (\s@SharePrincipal' {} a -> s {role' = a} :: SharePrincipal)

instance Core.Hashable SharePrincipal

instance Core.NFData SharePrincipal

instance Core.ToJSON SharePrincipal where
  toJSON SharePrincipal' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            Core.Just ("Type" Core..= type'),
            Core.Just ("Role" Core..= role')
          ]
      )

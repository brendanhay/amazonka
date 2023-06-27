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
-- Module      : Amazonka.WorkDocs.Types.SharePrincipal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.SharePrincipal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.PrincipalType
import Amazonka.WorkDocs.Types.RoleType

-- | Describes the recipient type and ID, if available.
--
-- /See:/ 'newSharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { -- | The ID of the recipient.
    id :: Prelude.Text,
    -- | The type of the recipient.
    type' :: PrincipalType,
    -- | The role of the recipient.
    role' :: RoleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
sharePrincipal_id :: Lens.Lens' SharePrincipal Prelude.Text
sharePrincipal_id = Lens.lens (\SharePrincipal' {id} -> id) (\s@SharePrincipal' {} a -> s {id = a} :: SharePrincipal)

-- | The type of the recipient.
sharePrincipal_type :: Lens.Lens' SharePrincipal PrincipalType
sharePrincipal_type = Lens.lens (\SharePrincipal' {type'} -> type') (\s@SharePrincipal' {} a -> s {type' = a} :: SharePrincipal)

-- | The role of the recipient.
sharePrincipal_role :: Lens.Lens' SharePrincipal RoleType
sharePrincipal_role = Lens.lens (\SharePrincipal' {role'} -> role') (\s@SharePrincipal' {} a -> s {role' = a} :: SharePrincipal)

instance Prelude.Hashable SharePrincipal where
  hashWithSalt _salt SharePrincipal' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` role'

instance Prelude.NFData SharePrincipal where
  rnf SharePrincipal' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf role'

instance Data.ToJSON SharePrincipal where
  toJSON SharePrincipal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Role" Data..= role')
          ]
      )

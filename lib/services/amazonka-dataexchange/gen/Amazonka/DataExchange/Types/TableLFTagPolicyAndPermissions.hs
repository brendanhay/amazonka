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
-- Module      : Amazonka.DataExchange.Types.TableLFTagPolicyAndPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.TableLFTagPolicyAndPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.LFTag
import Amazonka.DataExchange.Types.TableTagPolicyLFPermission
import qualified Amazonka.Prelude as Prelude

-- | The LF-tag policy and permissions that apply to table resources.
--
-- /See:/ 'newTableLFTagPolicyAndPermissions' smart constructor.
data TableLFTagPolicyAndPermissions = TableLFTagPolicyAndPermissions'
  { -- | A list of LF-tag conditions that apply to table resources.
    expression :: [LFTag],
    -- | The permissions granted to subscribers on table resources.
    permissions :: [TableTagPolicyLFPermission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableLFTagPolicyAndPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'tableLFTagPolicyAndPermissions_expression' - A list of LF-tag conditions that apply to table resources.
--
-- 'permissions', 'tableLFTagPolicyAndPermissions_permissions' - The permissions granted to subscribers on table resources.
newTableLFTagPolicyAndPermissions ::
  TableLFTagPolicyAndPermissions
newTableLFTagPolicyAndPermissions =
  TableLFTagPolicyAndPermissions'
    { expression =
        Prelude.mempty,
      permissions = Prelude.mempty
    }

-- | A list of LF-tag conditions that apply to table resources.
tableLFTagPolicyAndPermissions_expression :: Lens.Lens' TableLFTagPolicyAndPermissions [LFTag]
tableLFTagPolicyAndPermissions_expression = Lens.lens (\TableLFTagPolicyAndPermissions' {expression} -> expression) (\s@TableLFTagPolicyAndPermissions' {} a -> s {expression = a} :: TableLFTagPolicyAndPermissions) Prelude.. Lens.coerced

-- | The permissions granted to subscribers on table resources.
tableLFTagPolicyAndPermissions_permissions :: Lens.Lens' TableLFTagPolicyAndPermissions [TableTagPolicyLFPermission]
tableLFTagPolicyAndPermissions_permissions = Lens.lens (\TableLFTagPolicyAndPermissions' {permissions} -> permissions) (\s@TableLFTagPolicyAndPermissions' {} a -> s {permissions = a} :: TableLFTagPolicyAndPermissions) Prelude.. Lens.coerced

instance Data.FromJSON TableLFTagPolicyAndPermissions where
  parseJSON =
    Data.withObject
      "TableLFTagPolicyAndPermissions"
      ( \x ->
          TableLFTagPolicyAndPermissions'
            Prelude.<$> (x Data..:? "Expression" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Permissions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    TableLFTagPolicyAndPermissions
  where
  hashWithSalt
    _salt
    TableLFTagPolicyAndPermissions' {..} =
      _salt
        `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` permissions

instance
  Prelude.NFData
    TableLFTagPolicyAndPermissions
  where
  rnf TableLFTagPolicyAndPermissions' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf permissions

instance Data.ToJSON TableLFTagPolicyAndPermissions where
  toJSON TableLFTagPolicyAndPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Expression" Data..= expression),
            Prelude.Just ("Permissions" Data..= permissions)
          ]
      )

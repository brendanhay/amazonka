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
-- Module      : Amazonka.DataExchange.Types.DatabaseLFTagPolicyAndPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.DatabaseLFTagPolicyAndPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.DatabaseLFTagPolicyPermission
import Amazonka.DataExchange.Types.LFTag
import qualified Amazonka.Prelude as Prelude

-- | The LF-tag policy and permissions for database resources.
--
-- /See:/ 'newDatabaseLFTagPolicyAndPermissions' smart constructor.
data DatabaseLFTagPolicyAndPermissions = DatabaseLFTagPolicyAndPermissions'
  { -- | A list of LF-tag conditions that apply to database resources.
    expression :: [LFTag],
    -- | The permissions granted to subscribers on database resources.
    permissions :: [DatabaseLFTagPolicyPermission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseLFTagPolicyAndPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'databaseLFTagPolicyAndPermissions_expression' - A list of LF-tag conditions that apply to database resources.
--
-- 'permissions', 'databaseLFTagPolicyAndPermissions_permissions' - The permissions granted to subscribers on database resources.
newDatabaseLFTagPolicyAndPermissions ::
  DatabaseLFTagPolicyAndPermissions
newDatabaseLFTagPolicyAndPermissions =
  DatabaseLFTagPolicyAndPermissions'
    { expression =
        Prelude.mempty,
      permissions = Prelude.mempty
    }

-- | A list of LF-tag conditions that apply to database resources.
databaseLFTagPolicyAndPermissions_expression :: Lens.Lens' DatabaseLFTagPolicyAndPermissions [LFTag]
databaseLFTagPolicyAndPermissions_expression = Lens.lens (\DatabaseLFTagPolicyAndPermissions' {expression} -> expression) (\s@DatabaseLFTagPolicyAndPermissions' {} a -> s {expression = a} :: DatabaseLFTagPolicyAndPermissions) Prelude.. Lens.coerced

-- | The permissions granted to subscribers on database resources.
databaseLFTagPolicyAndPermissions_permissions :: Lens.Lens' DatabaseLFTagPolicyAndPermissions [DatabaseLFTagPolicyPermission]
databaseLFTagPolicyAndPermissions_permissions = Lens.lens (\DatabaseLFTagPolicyAndPermissions' {permissions} -> permissions) (\s@DatabaseLFTagPolicyAndPermissions' {} a -> s {permissions = a} :: DatabaseLFTagPolicyAndPermissions) Prelude.. Lens.coerced

instance
  Data.FromJSON
    DatabaseLFTagPolicyAndPermissions
  where
  parseJSON =
    Data.withObject
      "DatabaseLFTagPolicyAndPermissions"
      ( \x ->
          DatabaseLFTagPolicyAndPermissions'
            Prelude.<$> (x Data..:? "Expression" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Permissions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DatabaseLFTagPolicyAndPermissions
  where
  hashWithSalt
    _salt
    DatabaseLFTagPolicyAndPermissions' {..} =
      _salt `Prelude.hashWithSalt` expression
        `Prelude.hashWithSalt` permissions

instance
  Prelude.NFData
    DatabaseLFTagPolicyAndPermissions
  where
  rnf DatabaseLFTagPolicyAndPermissions' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf permissions

instance
  Data.ToJSON
    DatabaseLFTagPolicyAndPermissions
  where
  toJSON DatabaseLFTagPolicyAndPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Expression" Data..= expression),
            Prelude.Just ("Permissions" Data..= permissions)
          ]
      )

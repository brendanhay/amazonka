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
-- Module      : Amazonka.Glue.Types.SqlAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SqlAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in the list of values for @SqlAliases@.
--
-- /See:/ 'newSqlAlias' smart constructor.
data SqlAlias = SqlAlias'
  { -- | A table, or a column in a table.
    from :: Prelude.Text,
    -- | A temporary name given to a table, or a column in a table.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'sqlAlias_from' - A table, or a column in a table.
--
-- 'alias', 'sqlAlias_alias' - A temporary name given to a table, or a column in a table.
newSqlAlias ::
  -- | 'from'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  SqlAlias
newSqlAlias pFrom_ pAlias_ =
  SqlAlias' {from = pFrom_, alias = pAlias_}

-- | A table, or a column in a table.
sqlAlias_from :: Lens.Lens' SqlAlias Prelude.Text
sqlAlias_from = Lens.lens (\SqlAlias' {from} -> from) (\s@SqlAlias' {} a -> s {from = a} :: SqlAlias)

-- | A temporary name given to a table, or a column in a table.
sqlAlias_alias :: Lens.Lens' SqlAlias Prelude.Text
sqlAlias_alias = Lens.lens (\SqlAlias' {alias} -> alias) (\s@SqlAlias' {} a -> s {alias = a} :: SqlAlias)

instance Data.FromJSON SqlAlias where
  parseJSON =
    Data.withObject
      "SqlAlias"
      ( \x ->
          SqlAlias'
            Prelude.<$> (x Data..: "From")
            Prelude.<*> (x Data..: "Alias")
      )

instance Prelude.Hashable SqlAlias where
  hashWithSalt _salt SqlAlias' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` alias

instance Prelude.NFData SqlAlias where
  rnf SqlAlias' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf alias

instance Data.ToJSON SqlAlias where
  toJSON SqlAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("From" Data..= from),
            Prelude.Just ("Alias" Data..= alias)
          ]
      )

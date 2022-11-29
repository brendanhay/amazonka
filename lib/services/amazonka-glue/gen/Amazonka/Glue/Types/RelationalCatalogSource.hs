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
-- Module      : Amazonka.Glue.Types.RelationalCatalogSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RelationalCatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Relational database data source in the Glue Data Catalog.
--
-- /See:/ 'newRelationalCatalogSource' smart constructor.
data RelationalCatalogSource = RelationalCatalogSource'
  { -- | The name of the data source.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalCatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'relationalCatalogSource_name' - The name of the data source.
--
-- 'database', 'relationalCatalogSource_database' - The name of the database to read from.
--
-- 'table', 'relationalCatalogSource_table' - The name of the table in the database to read from.
newRelationalCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  RelationalCatalogSource
newRelationalCatalogSource pName_ pDatabase_ pTable_ =
  RelationalCatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data source.
relationalCatalogSource_name :: Lens.Lens' RelationalCatalogSource Prelude.Text
relationalCatalogSource_name = Lens.lens (\RelationalCatalogSource' {name} -> name) (\s@RelationalCatalogSource' {} a -> s {name = a} :: RelationalCatalogSource)

-- | The name of the database to read from.
relationalCatalogSource_database :: Lens.Lens' RelationalCatalogSource Prelude.Text
relationalCatalogSource_database = Lens.lens (\RelationalCatalogSource' {database} -> database) (\s@RelationalCatalogSource' {} a -> s {database = a} :: RelationalCatalogSource)

-- | The name of the table in the database to read from.
relationalCatalogSource_table :: Lens.Lens' RelationalCatalogSource Prelude.Text
relationalCatalogSource_table = Lens.lens (\RelationalCatalogSource' {table} -> table) (\s@RelationalCatalogSource' {} a -> s {table = a} :: RelationalCatalogSource)

instance Core.FromJSON RelationalCatalogSource where
  parseJSON =
    Core.withObject
      "RelationalCatalogSource"
      ( \x ->
          RelationalCatalogSource'
            Prelude.<$> (x Core..: "Name")
            Prelude.<*> (x Core..: "Database")
            Prelude.<*> (x Core..: "Table")
      )

instance Prelude.Hashable RelationalCatalogSource where
  hashWithSalt _salt RelationalCatalogSource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData RelationalCatalogSource where
  rnf RelationalCatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Core.ToJSON RelationalCatalogSource where
  toJSON RelationalCatalogSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Database" Core..= database),
            Prelude.Just ("Table" Core..= table)
          ]
      )

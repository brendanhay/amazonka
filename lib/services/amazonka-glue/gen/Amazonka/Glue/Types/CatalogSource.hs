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
-- Module      : Amazonka.Glue.Types.CatalogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a data store in the Glue Data Catalog.
--
-- /See:/ 'newCatalogSource' smart constructor.
data CatalogSource = CatalogSource'
  { -- | The name of the data store.
    name :: Prelude.Text,
    -- | The name of the database to read from.
    database :: Prelude.Text,
    -- | The name of the table in the database to read from.
    table :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'catalogSource_name' - The name of the data store.
--
-- 'database', 'catalogSource_database' - The name of the database to read from.
--
-- 'table', 'catalogSource_table' - The name of the table in the database to read from.
newCatalogSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'table'
  Prelude.Text ->
  CatalogSource
newCatalogSource pName_ pDatabase_ pTable_ =
  CatalogSource'
    { name = pName_,
      database = pDatabase_,
      table = pTable_
    }

-- | The name of the data store.
catalogSource_name :: Lens.Lens' CatalogSource Prelude.Text
catalogSource_name = Lens.lens (\CatalogSource' {name} -> name) (\s@CatalogSource' {} a -> s {name = a} :: CatalogSource)

-- | The name of the database to read from.
catalogSource_database :: Lens.Lens' CatalogSource Prelude.Text
catalogSource_database = Lens.lens (\CatalogSource' {database} -> database) (\s@CatalogSource' {} a -> s {database = a} :: CatalogSource)

-- | The name of the table in the database to read from.
catalogSource_table :: Lens.Lens' CatalogSource Prelude.Text
catalogSource_table = Lens.lens (\CatalogSource' {table} -> table) (\s@CatalogSource' {} a -> s {table = a} :: CatalogSource)

instance Data.FromJSON CatalogSource where
  parseJSON =
    Data.withObject
      "CatalogSource"
      ( \x ->
          CatalogSource'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Database")
            Prelude.<*> (x Data..: "Table")
      )

instance Prelude.Hashable CatalogSource where
  hashWithSalt _salt CatalogSource' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` table

instance Prelude.NFData CatalogSource where
  rnf CatalogSource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf table

instance Data.ToJSON CatalogSource where
  toJSON CatalogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Database" Data..= database),
            Prelude.Just ("Table" Data..= table)
          ]
      )

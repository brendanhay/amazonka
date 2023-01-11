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
-- Module      : Amazonka.LakeFormation.Types.DataCellsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataCellsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.ColumnWildcard
import Amazonka.LakeFormation.Types.RowFilter
import qualified Amazonka.Prelude as Prelude

-- | A structure that describes certain columns on certain rows.
--
-- /See:/ 'newDataCellsFilter' smart constructor.
data DataCellsFilter = DataCellsFilter'
  { -- | A list of column names.
    columnNames :: Prelude.Maybe [Prelude.Text],
    -- | A wildcard with exclusions.
    --
    -- You must specify either a @ColumnNames@ list or the @ColumnWildCard@.
    columnWildcard :: Prelude.Maybe ColumnWildcard,
    -- | A PartiQL predicate.
    rowFilter :: Prelude.Maybe RowFilter,
    -- | The ID of the catalog to which the table belongs.
    tableCatalogId :: Prelude.Text,
    -- | A database in the Glue Data Catalog.
    databaseName :: Prelude.Text,
    -- | A table in the database.
    tableName :: Prelude.Text,
    -- | The name given by the user to the data filter cell.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCellsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnNames', 'dataCellsFilter_columnNames' - A list of column names.
--
-- 'columnWildcard', 'dataCellsFilter_columnWildcard' - A wildcard with exclusions.
--
-- You must specify either a @ColumnNames@ list or the @ColumnWildCard@.
--
-- 'rowFilter', 'dataCellsFilter_rowFilter' - A PartiQL predicate.
--
-- 'tableCatalogId', 'dataCellsFilter_tableCatalogId' - The ID of the catalog to which the table belongs.
--
-- 'databaseName', 'dataCellsFilter_databaseName' - A database in the Glue Data Catalog.
--
-- 'tableName', 'dataCellsFilter_tableName' - A table in the database.
--
-- 'name', 'dataCellsFilter_name' - The name given by the user to the data filter cell.
newDataCellsFilter ::
  -- | 'tableCatalogId'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DataCellsFilter
newDataCellsFilter
  pTableCatalogId_
  pDatabaseName_
  pTableName_
  pName_ =
    DataCellsFilter'
      { columnNames = Prelude.Nothing,
        columnWildcard = Prelude.Nothing,
        rowFilter = Prelude.Nothing,
        tableCatalogId = pTableCatalogId_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        name = pName_
      }

-- | A list of column names.
dataCellsFilter_columnNames :: Lens.Lens' DataCellsFilter (Prelude.Maybe [Prelude.Text])
dataCellsFilter_columnNames = Lens.lens (\DataCellsFilter' {columnNames} -> columnNames) (\s@DataCellsFilter' {} a -> s {columnNames = a} :: DataCellsFilter) Prelude.. Lens.mapping Lens.coerced

-- | A wildcard with exclusions.
--
-- You must specify either a @ColumnNames@ list or the @ColumnWildCard@.
dataCellsFilter_columnWildcard :: Lens.Lens' DataCellsFilter (Prelude.Maybe ColumnWildcard)
dataCellsFilter_columnWildcard = Lens.lens (\DataCellsFilter' {columnWildcard} -> columnWildcard) (\s@DataCellsFilter' {} a -> s {columnWildcard = a} :: DataCellsFilter)

-- | A PartiQL predicate.
dataCellsFilter_rowFilter :: Lens.Lens' DataCellsFilter (Prelude.Maybe RowFilter)
dataCellsFilter_rowFilter = Lens.lens (\DataCellsFilter' {rowFilter} -> rowFilter) (\s@DataCellsFilter' {} a -> s {rowFilter = a} :: DataCellsFilter)

-- | The ID of the catalog to which the table belongs.
dataCellsFilter_tableCatalogId :: Lens.Lens' DataCellsFilter Prelude.Text
dataCellsFilter_tableCatalogId = Lens.lens (\DataCellsFilter' {tableCatalogId} -> tableCatalogId) (\s@DataCellsFilter' {} a -> s {tableCatalogId = a} :: DataCellsFilter)

-- | A database in the Glue Data Catalog.
dataCellsFilter_databaseName :: Lens.Lens' DataCellsFilter Prelude.Text
dataCellsFilter_databaseName = Lens.lens (\DataCellsFilter' {databaseName} -> databaseName) (\s@DataCellsFilter' {} a -> s {databaseName = a} :: DataCellsFilter)

-- | A table in the database.
dataCellsFilter_tableName :: Lens.Lens' DataCellsFilter Prelude.Text
dataCellsFilter_tableName = Lens.lens (\DataCellsFilter' {tableName} -> tableName) (\s@DataCellsFilter' {} a -> s {tableName = a} :: DataCellsFilter)

-- | The name given by the user to the data filter cell.
dataCellsFilter_name :: Lens.Lens' DataCellsFilter Prelude.Text
dataCellsFilter_name = Lens.lens (\DataCellsFilter' {name} -> name) (\s@DataCellsFilter' {} a -> s {name = a} :: DataCellsFilter)

instance Data.FromJSON DataCellsFilter where
  parseJSON =
    Data.withObject
      "DataCellsFilter"
      ( \x ->
          DataCellsFilter'
            Prelude.<$> (x Data..:? "ColumnNames" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ColumnWildcard")
            Prelude.<*> (x Data..:? "RowFilter")
            Prelude.<*> (x Data..: "TableCatalogId")
            Prelude.<*> (x Data..: "DatabaseName")
            Prelude.<*> (x Data..: "TableName")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DataCellsFilter where
  hashWithSalt _salt DataCellsFilter' {..} =
    _salt `Prelude.hashWithSalt` columnNames
      `Prelude.hashWithSalt` columnWildcard
      `Prelude.hashWithSalt` rowFilter
      `Prelude.hashWithSalt` tableCatalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DataCellsFilter where
  rnf DataCellsFilter' {..} =
    Prelude.rnf columnNames
      `Prelude.seq` Prelude.rnf columnWildcard
      `Prelude.seq` Prelude.rnf rowFilter
      `Prelude.seq` Prelude.rnf tableCatalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON DataCellsFilter where
  toJSON DataCellsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnNames" Data..=) Prelude.<$> columnNames,
            ("ColumnWildcard" Data..=)
              Prelude.<$> columnWildcard,
            ("RowFilter" Data..=) Prelude.<$> rowFilter,
            Prelude.Just
              ("TableCatalogId" Data..= tableCatalogId),
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("Name" Data..= name)
          ]
      )

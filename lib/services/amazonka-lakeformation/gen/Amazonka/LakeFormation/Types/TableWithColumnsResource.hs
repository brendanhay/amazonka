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
-- Module      : Amazonka.LakeFormation.Types.TableWithColumnsResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.TableWithColumnsResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.ColumnWildcard
import qualified Amazonka.Prelude as Prelude

-- | A structure for a table with columns object. This object is only used
-- when granting a SELECT permission.
--
-- This object must take a value for at least one of @ColumnsNames@,
-- @ColumnsIndexes@, or @ColumnsWildcard@.
--
-- /See:/ 'newTableWithColumnsResource' smart constructor.
data TableWithColumnsResource = TableWithColumnsResource'
  { -- | The list of column names for the table. At least one of @ColumnNames@ or
    -- @ColumnWildcard@ is required.
    columnNames :: Prelude.Maybe [Prelude.Text],
    -- | A wildcard specified by a @ColumnWildcard@ object. At least one of
    -- @ColumnNames@ or @ColumnWildcard@ is required.
    columnWildcard :: Prelude.Maybe ColumnWildcard,
    -- | The identifier for the Data Catalog. By default, it is the account ID of
    -- the caller.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database for the table with columns resource. Unique to
    -- the Data Catalog. A database is a set of associated table definitions
    -- organized into a logical group. You can Grant and Revoke database
    -- privileges to a principal.
    databaseName :: Prelude.Text,
    -- | The name of the table resource. A table is a metadata definition that
    -- represents your data. You can Grant and Revoke table privileges to a
    -- principal.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableWithColumnsResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnNames', 'tableWithColumnsResource_columnNames' - The list of column names for the table. At least one of @ColumnNames@ or
-- @ColumnWildcard@ is required.
--
-- 'columnWildcard', 'tableWithColumnsResource_columnWildcard' - A wildcard specified by a @ColumnWildcard@ object. At least one of
-- @ColumnNames@ or @ColumnWildcard@ is required.
--
-- 'catalogId', 'tableWithColumnsResource_catalogId' - The identifier for the Data Catalog. By default, it is the account ID of
-- the caller.
--
-- 'databaseName', 'tableWithColumnsResource_databaseName' - The name of the database for the table with columns resource. Unique to
-- the Data Catalog. A database is a set of associated table definitions
-- organized into a logical group. You can Grant and Revoke database
-- privileges to a principal.
--
-- 'name', 'tableWithColumnsResource_name' - The name of the table resource. A table is a metadata definition that
-- represents your data. You can Grant and Revoke table privileges to a
-- principal.
newTableWithColumnsResource ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  TableWithColumnsResource
newTableWithColumnsResource pDatabaseName_ pName_ =
  TableWithColumnsResource'
    { columnNames =
        Prelude.Nothing,
      columnWildcard = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      name = pName_
    }

-- | The list of column names for the table. At least one of @ColumnNames@ or
-- @ColumnWildcard@ is required.
tableWithColumnsResource_columnNames :: Lens.Lens' TableWithColumnsResource (Prelude.Maybe [Prelude.Text])
tableWithColumnsResource_columnNames = Lens.lens (\TableWithColumnsResource' {columnNames} -> columnNames) (\s@TableWithColumnsResource' {} a -> s {columnNames = a} :: TableWithColumnsResource) Prelude.. Lens.mapping Lens.coerced

-- | A wildcard specified by a @ColumnWildcard@ object. At least one of
-- @ColumnNames@ or @ColumnWildcard@ is required.
tableWithColumnsResource_columnWildcard :: Lens.Lens' TableWithColumnsResource (Prelude.Maybe ColumnWildcard)
tableWithColumnsResource_columnWildcard = Lens.lens (\TableWithColumnsResource' {columnWildcard} -> columnWildcard) (\s@TableWithColumnsResource' {} a -> s {columnWildcard = a} :: TableWithColumnsResource)

-- | The identifier for the Data Catalog. By default, it is the account ID of
-- the caller.
tableWithColumnsResource_catalogId :: Lens.Lens' TableWithColumnsResource (Prelude.Maybe Prelude.Text)
tableWithColumnsResource_catalogId = Lens.lens (\TableWithColumnsResource' {catalogId} -> catalogId) (\s@TableWithColumnsResource' {} a -> s {catalogId = a} :: TableWithColumnsResource)

-- | The name of the database for the table with columns resource. Unique to
-- the Data Catalog. A database is a set of associated table definitions
-- organized into a logical group. You can Grant and Revoke database
-- privileges to a principal.
tableWithColumnsResource_databaseName :: Lens.Lens' TableWithColumnsResource Prelude.Text
tableWithColumnsResource_databaseName = Lens.lens (\TableWithColumnsResource' {databaseName} -> databaseName) (\s@TableWithColumnsResource' {} a -> s {databaseName = a} :: TableWithColumnsResource)

-- | The name of the table resource. A table is a metadata definition that
-- represents your data. You can Grant and Revoke table privileges to a
-- principal.
tableWithColumnsResource_name :: Lens.Lens' TableWithColumnsResource Prelude.Text
tableWithColumnsResource_name = Lens.lens (\TableWithColumnsResource' {name} -> name) (\s@TableWithColumnsResource' {} a -> s {name = a} :: TableWithColumnsResource)

instance Core.FromJSON TableWithColumnsResource where
  parseJSON =
    Core.withObject
      "TableWithColumnsResource"
      ( \x ->
          TableWithColumnsResource'
            Prelude.<$> (x Core..:? "ColumnNames" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ColumnWildcard")
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable TableWithColumnsResource where
  hashWithSalt _salt TableWithColumnsResource' {..} =
    _salt `Prelude.hashWithSalt` columnNames
      `Prelude.hashWithSalt` columnWildcard
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` name

instance Prelude.NFData TableWithColumnsResource where
  rnf TableWithColumnsResource' {..} =
    Prelude.rnf columnNames
      `Prelude.seq` Prelude.rnf columnWildcard
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON TableWithColumnsResource where
  toJSON TableWithColumnsResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ColumnNames" Core..=) Prelude.<$> columnNames,
            ("ColumnWildcard" Core..=)
              Prelude.<$> columnWildcard,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("Name" Core..= name)
          ]
      )

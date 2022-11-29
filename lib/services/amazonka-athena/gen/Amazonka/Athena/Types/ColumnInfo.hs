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
-- Module      : Amazonka.Athena.Types.ColumnInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ColumnInfo where

import Amazonka.Athena.Types.ColumnNullable
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the columns in a query execution result.
--
-- /See:/ 'newColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { -- | The table name for the query results.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The catalog to which the query results belong.
    catalogName :: Prelude.Maybe Prelude.Text,
    -- | A column label.
    label :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether values in the column are case-sensitive.
    caseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | The schema name (database name) to which the query results belong.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the column\'s nullable status.
    nullable :: Prelude.Maybe ColumnNullable,
    -- | For @DECIMAL@ data types, specifies the total number of digits, up to
    -- 38. For performance reasons, we recommend up to 18 digits.
    precision :: Prelude.Maybe Prelude.Int,
    -- | For @DECIMAL@ data types, specifies the total number of digits in the
    -- fractional part of the value. Defaults to 0.
    scale :: Prelude.Maybe Prelude.Int,
    -- | The name of the column.
    name :: Prelude.Text,
    -- | The data type of the column.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'columnInfo_tableName' - The table name for the query results.
--
-- 'catalogName', 'columnInfo_catalogName' - The catalog to which the query results belong.
--
-- 'label', 'columnInfo_label' - A column label.
--
-- 'caseSensitive', 'columnInfo_caseSensitive' - Indicates whether values in the column are case-sensitive.
--
-- 'schemaName', 'columnInfo_schemaName' - The schema name (database name) to which the query results belong.
--
-- 'nullable', 'columnInfo_nullable' - Indicates the column\'s nullable status.
--
-- 'precision', 'columnInfo_precision' - For @DECIMAL@ data types, specifies the total number of digits, up to
-- 38. For performance reasons, we recommend up to 18 digits.
--
-- 'scale', 'columnInfo_scale' - For @DECIMAL@ data types, specifies the total number of digits in the
-- fractional part of the value. Defaults to 0.
--
-- 'name', 'columnInfo_name' - The name of the column.
--
-- 'type'', 'columnInfo_type' - The data type of the column.
newColumnInfo ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  ColumnInfo
newColumnInfo pName_ pType_ =
  ColumnInfo'
    { tableName = Prelude.Nothing,
      catalogName = Prelude.Nothing,
      label = Prelude.Nothing,
      caseSensitive = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      nullable = Prelude.Nothing,
      precision = Prelude.Nothing,
      scale = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The table name for the query results.
columnInfo_tableName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_tableName = Lens.lens (\ColumnInfo' {tableName} -> tableName) (\s@ColumnInfo' {} a -> s {tableName = a} :: ColumnInfo)

-- | The catalog to which the query results belong.
columnInfo_catalogName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_catalogName = Lens.lens (\ColumnInfo' {catalogName} -> catalogName) (\s@ColumnInfo' {} a -> s {catalogName = a} :: ColumnInfo)

-- | A column label.
columnInfo_label :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_label = Lens.lens (\ColumnInfo' {label} -> label) (\s@ColumnInfo' {} a -> s {label = a} :: ColumnInfo)

-- | Indicates whether values in the column are case-sensitive.
columnInfo_caseSensitive :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Bool)
columnInfo_caseSensitive = Lens.lens (\ColumnInfo' {caseSensitive} -> caseSensitive) (\s@ColumnInfo' {} a -> s {caseSensitive = a} :: ColumnInfo)

-- | The schema name (database name) to which the query results belong.
columnInfo_schemaName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_schemaName = Lens.lens (\ColumnInfo' {schemaName} -> schemaName) (\s@ColumnInfo' {} a -> s {schemaName = a} :: ColumnInfo)

-- | Indicates the column\'s nullable status.
columnInfo_nullable :: Lens.Lens' ColumnInfo (Prelude.Maybe ColumnNullable)
columnInfo_nullable = Lens.lens (\ColumnInfo' {nullable} -> nullable) (\s@ColumnInfo' {} a -> s {nullable = a} :: ColumnInfo)

-- | For @DECIMAL@ data types, specifies the total number of digits, up to
-- 38. For performance reasons, we recommend up to 18 digits.
columnInfo_precision :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Int)
columnInfo_precision = Lens.lens (\ColumnInfo' {precision} -> precision) (\s@ColumnInfo' {} a -> s {precision = a} :: ColumnInfo)

-- | For @DECIMAL@ data types, specifies the total number of digits in the
-- fractional part of the value. Defaults to 0.
columnInfo_scale :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Int)
columnInfo_scale = Lens.lens (\ColumnInfo' {scale} -> scale) (\s@ColumnInfo' {} a -> s {scale = a} :: ColumnInfo)

-- | The name of the column.
columnInfo_name :: Lens.Lens' ColumnInfo Prelude.Text
columnInfo_name = Lens.lens (\ColumnInfo' {name} -> name) (\s@ColumnInfo' {} a -> s {name = a} :: ColumnInfo)

-- | The data type of the column.
columnInfo_type :: Lens.Lens' ColumnInfo Prelude.Text
columnInfo_type = Lens.lens (\ColumnInfo' {type'} -> type') (\s@ColumnInfo' {} a -> s {type' = a} :: ColumnInfo)

instance Core.FromJSON ColumnInfo where
  parseJSON =
    Core.withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            Prelude.<$> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "CatalogName")
            Prelude.<*> (x Core..:? "Label")
            Prelude.<*> (x Core..:? "CaseSensitive")
            Prelude.<*> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "Nullable")
            Prelude.<*> (x Core..:? "Precision")
            Prelude.<*> (x Core..:? "Scale")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable ColumnInfo where
  hashWithSalt _salt ColumnInfo' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` catalogName
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` caseSensitive
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` nullable
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ColumnInfo where
  rnf ColumnInfo' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf catalogName
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf caseSensitive
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf nullable
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf scale
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

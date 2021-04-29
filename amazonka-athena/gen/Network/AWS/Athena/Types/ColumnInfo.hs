{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Athena.Types.ColumnInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnInfo where

import Network.AWS.Athena.Types.ColumnNullable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the columns in a query execution result.
--
-- /See:/ 'newColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { -- | The catalog to which the query results belong.
    catalogName :: Prelude.Maybe Prelude.Text,
    -- | The table name for the query results.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | For @DECIMAL@ data types, specifies the total number of digits, up to
    -- 38. For performance reasons, we recommend up to 18 digits.
    precision :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether values in the column are case-sensitive.
    caseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the column\'s nullable status.
    nullable :: Prelude.Maybe ColumnNullable,
    -- | A column label.
    label :: Prelude.Maybe Prelude.Text,
    -- | The schema name (database name) to which the query results belong.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | For @DECIMAL@ data types, specifies the total number of digits in the
    -- fractional part of the value. Defaults to 0.
    scale :: Prelude.Maybe Prelude.Int,
    -- | The name of the column.
    name :: Prelude.Text,
    -- | The data type of the column.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ColumnInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogName', 'columnInfo_catalogName' - The catalog to which the query results belong.
--
-- 'tableName', 'columnInfo_tableName' - The table name for the query results.
--
-- 'precision', 'columnInfo_precision' - For @DECIMAL@ data types, specifies the total number of digits, up to
-- 38. For performance reasons, we recommend up to 18 digits.
--
-- 'caseSensitive', 'columnInfo_caseSensitive' - Indicates whether values in the column are case-sensitive.
--
-- 'nullable', 'columnInfo_nullable' - Indicates the column\'s nullable status.
--
-- 'label', 'columnInfo_label' - A column label.
--
-- 'schemaName', 'columnInfo_schemaName' - The schema name (database name) to which the query results belong.
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
    { catalogName = Prelude.Nothing,
      tableName = Prelude.Nothing,
      precision = Prelude.Nothing,
      caseSensitive = Prelude.Nothing,
      nullable = Prelude.Nothing,
      label = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      scale = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The catalog to which the query results belong.
columnInfo_catalogName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_catalogName = Lens.lens (\ColumnInfo' {catalogName} -> catalogName) (\s@ColumnInfo' {} a -> s {catalogName = a} :: ColumnInfo)

-- | The table name for the query results.
columnInfo_tableName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_tableName = Lens.lens (\ColumnInfo' {tableName} -> tableName) (\s@ColumnInfo' {} a -> s {tableName = a} :: ColumnInfo)

-- | For @DECIMAL@ data types, specifies the total number of digits, up to
-- 38. For performance reasons, we recommend up to 18 digits.
columnInfo_precision :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Int)
columnInfo_precision = Lens.lens (\ColumnInfo' {precision} -> precision) (\s@ColumnInfo' {} a -> s {precision = a} :: ColumnInfo)

-- | Indicates whether values in the column are case-sensitive.
columnInfo_caseSensitive :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Bool)
columnInfo_caseSensitive = Lens.lens (\ColumnInfo' {caseSensitive} -> caseSensitive) (\s@ColumnInfo' {} a -> s {caseSensitive = a} :: ColumnInfo)

-- | Indicates the column\'s nullable status.
columnInfo_nullable :: Lens.Lens' ColumnInfo (Prelude.Maybe ColumnNullable)
columnInfo_nullable = Lens.lens (\ColumnInfo' {nullable} -> nullable) (\s@ColumnInfo' {} a -> s {nullable = a} :: ColumnInfo)

-- | A column label.
columnInfo_label :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_label = Lens.lens (\ColumnInfo' {label} -> label) (\s@ColumnInfo' {} a -> s {label = a} :: ColumnInfo)

-- | The schema name (database name) to which the query results belong.
columnInfo_schemaName :: Lens.Lens' ColumnInfo (Prelude.Maybe Prelude.Text)
columnInfo_schemaName = Lens.lens (\ColumnInfo' {schemaName} -> schemaName) (\s@ColumnInfo' {} a -> s {schemaName = a} :: ColumnInfo)

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

instance Prelude.FromJSON ColumnInfo where
  parseJSON =
    Prelude.withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            Prelude.<$> (x Prelude..:? "CatalogName")
            Prelude.<*> (x Prelude..:? "TableName")
            Prelude.<*> (x Prelude..:? "Precision")
            Prelude.<*> (x Prelude..:? "CaseSensitive")
            Prelude.<*> (x Prelude..:? "Nullable")
            Prelude.<*> (x Prelude..:? "Label")
            Prelude.<*> (x Prelude..:? "SchemaName")
            Prelude.<*> (x Prelude..:? "Scale")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable ColumnInfo

instance Prelude.NFData ColumnInfo

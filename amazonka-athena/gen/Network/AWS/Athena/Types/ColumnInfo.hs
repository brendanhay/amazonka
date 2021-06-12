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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the columns in a query execution result.
--
-- /See:/ 'newColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { -- | The catalog to which the query results belong.
    catalogName :: Core.Maybe Core.Text,
    -- | The table name for the query results.
    tableName :: Core.Maybe Core.Text,
    -- | For @DECIMAL@ data types, specifies the total number of digits, up to
    -- 38. For performance reasons, we recommend up to 18 digits.
    precision :: Core.Maybe Core.Int,
    -- | Indicates whether values in the column are case-sensitive.
    caseSensitive :: Core.Maybe Core.Bool,
    -- | Indicates the column\'s nullable status.
    nullable :: Core.Maybe ColumnNullable,
    -- | A column label.
    label :: Core.Maybe Core.Text,
    -- | The schema name (database name) to which the query results belong.
    schemaName :: Core.Maybe Core.Text,
    -- | For @DECIMAL@ data types, specifies the total number of digits in the
    -- fractional part of the value. Defaults to 0.
    scale :: Core.Maybe Core.Int,
    -- | The name of the column.
    name :: Core.Text,
    -- | The data type of the column.
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'type''
  Core.Text ->
  ColumnInfo
newColumnInfo pName_ pType_ =
  ColumnInfo'
    { catalogName = Core.Nothing,
      tableName = Core.Nothing,
      precision = Core.Nothing,
      caseSensitive = Core.Nothing,
      nullable = Core.Nothing,
      label = Core.Nothing,
      schemaName = Core.Nothing,
      scale = Core.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The catalog to which the query results belong.
columnInfo_catalogName :: Lens.Lens' ColumnInfo (Core.Maybe Core.Text)
columnInfo_catalogName = Lens.lens (\ColumnInfo' {catalogName} -> catalogName) (\s@ColumnInfo' {} a -> s {catalogName = a} :: ColumnInfo)

-- | The table name for the query results.
columnInfo_tableName :: Lens.Lens' ColumnInfo (Core.Maybe Core.Text)
columnInfo_tableName = Lens.lens (\ColumnInfo' {tableName} -> tableName) (\s@ColumnInfo' {} a -> s {tableName = a} :: ColumnInfo)

-- | For @DECIMAL@ data types, specifies the total number of digits, up to
-- 38. For performance reasons, we recommend up to 18 digits.
columnInfo_precision :: Lens.Lens' ColumnInfo (Core.Maybe Core.Int)
columnInfo_precision = Lens.lens (\ColumnInfo' {precision} -> precision) (\s@ColumnInfo' {} a -> s {precision = a} :: ColumnInfo)

-- | Indicates whether values in the column are case-sensitive.
columnInfo_caseSensitive :: Lens.Lens' ColumnInfo (Core.Maybe Core.Bool)
columnInfo_caseSensitive = Lens.lens (\ColumnInfo' {caseSensitive} -> caseSensitive) (\s@ColumnInfo' {} a -> s {caseSensitive = a} :: ColumnInfo)

-- | Indicates the column\'s nullable status.
columnInfo_nullable :: Lens.Lens' ColumnInfo (Core.Maybe ColumnNullable)
columnInfo_nullable = Lens.lens (\ColumnInfo' {nullable} -> nullable) (\s@ColumnInfo' {} a -> s {nullable = a} :: ColumnInfo)

-- | A column label.
columnInfo_label :: Lens.Lens' ColumnInfo (Core.Maybe Core.Text)
columnInfo_label = Lens.lens (\ColumnInfo' {label} -> label) (\s@ColumnInfo' {} a -> s {label = a} :: ColumnInfo)

-- | The schema name (database name) to which the query results belong.
columnInfo_schemaName :: Lens.Lens' ColumnInfo (Core.Maybe Core.Text)
columnInfo_schemaName = Lens.lens (\ColumnInfo' {schemaName} -> schemaName) (\s@ColumnInfo' {} a -> s {schemaName = a} :: ColumnInfo)

-- | For @DECIMAL@ data types, specifies the total number of digits in the
-- fractional part of the value. Defaults to 0.
columnInfo_scale :: Lens.Lens' ColumnInfo (Core.Maybe Core.Int)
columnInfo_scale = Lens.lens (\ColumnInfo' {scale} -> scale) (\s@ColumnInfo' {} a -> s {scale = a} :: ColumnInfo)

-- | The name of the column.
columnInfo_name :: Lens.Lens' ColumnInfo Core.Text
columnInfo_name = Lens.lens (\ColumnInfo' {name} -> name) (\s@ColumnInfo' {} a -> s {name = a} :: ColumnInfo)

-- | The data type of the column.
columnInfo_type :: Lens.Lens' ColumnInfo Core.Text
columnInfo_type = Lens.lens (\ColumnInfo' {type'} -> type') (\s@ColumnInfo' {} a -> s {type' = a} :: ColumnInfo)

instance Core.FromJSON ColumnInfo where
  parseJSON =
    Core.withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            Core.<$> (x Core..:? "CatalogName")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "Precision")
            Core.<*> (x Core..:? "CaseSensitive")
            Core.<*> (x Core..:? "Nullable")
            Core.<*> (x Core..:? "Label")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (x Core..:? "Scale")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable ColumnInfo

instance Core.NFData ColumnInfo

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
-- Module      : Amazonka.RDSData.Types.ColumnMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.ColumnMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the metadata for a column.
--
-- /See:/ 'newColumnMetadata' smart constructor.
data ColumnMetadata = ColumnMetadata'
  { -- | The database-specific data type of the column.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column is case-sensitive.
    isCaseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the column contains currency values.
    isCurrency :: Prelude.Maybe Prelude.Bool,
    -- | The scale value of a decimal number column.
    scale :: Prelude.Maybe Prelude.Int,
    -- | The precision value of a decimal number column.
    precision :: Prelude.Maybe Prelude.Int,
    -- | The name of the schema that owns the table that includes the column.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column increments automatically.
    isAutoIncrement :: Prelude.Maybe Prelude.Bool,
    -- | The name of the column.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the column.
    arrayBaseColumnType :: Prelude.Maybe Prelude.Int,
    -- | The type of the column.
    type' :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether an integer column is signed.
    isSigned :: Prelude.Maybe Prelude.Bool,
    -- | The label for the column.
    label :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column is nullable.
    nullable :: Prelude.Maybe Prelude.Int,
    -- | The name of the table that includes the column.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'columnMetadata_typeName' - The database-specific data type of the column.
--
-- 'isCaseSensitive', 'columnMetadata_isCaseSensitive' - A value that indicates whether the column is case-sensitive.
--
-- 'isCurrency', 'columnMetadata_isCurrency' - A value that indicates whether the column contains currency values.
--
-- 'scale', 'columnMetadata_scale' - The scale value of a decimal number column.
--
-- 'precision', 'columnMetadata_precision' - The precision value of a decimal number column.
--
-- 'schemaName', 'columnMetadata_schemaName' - The name of the schema that owns the table that includes the column.
--
-- 'isAutoIncrement', 'columnMetadata_isAutoIncrement' - A value that indicates whether the column increments automatically.
--
-- 'name', 'columnMetadata_name' - The name of the column.
--
-- 'arrayBaseColumnType', 'columnMetadata_arrayBaseColumnType' - The type of the column.
--
-- 'type'', 'columnMetadata_type' - The type of the column.
--
-- 'isSigned', 'columnMetadata_isSigned' - A value that indicates whether an integer column is signed.
--
-- 'label', 'columnMetadata_label' - The label for the column.
--
-- 'nullable', 'columnMetadata_nullable' - A value that indicates whether the column is nullable.
--
-- 'tableName', 'columnMetadata_tableName' - The name of the table that includes the column.
newColumnMetadata ::
  ColumnMetadata
newColumnMetadata =
  ColumnMetadata'
    { typeName = Prelude.Nothing,
      isCaseSensitive = Prelude.Nothing,
      isCurrency = Prelude.Nothing,
      scale = Prelude.Nothing,
      precision = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      isAutoIncrement = Prelude.Nothing,
      name = Prelude.Nothing,
      arrayBaseColumnType = Prelude.Nothing,
      type' = Prelude.Nothing,
      isSigned = Prelude.Nothing,
      label = Prelude.Nothing,
      nullable = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The database-specific data type of the column.
columnMetadata_typeName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_typeName = Lens.lens (\ColumnMetadata' {typeName} -> typeName) (\s@ColumnMetadata' {} a -> s {typeName = a} :: ColumnMetadata)

-- | A value that indicates whether the column is case-sensitive.
columnMetadata_isCaseSensitive :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCaseSensitive = Lens.lens (\ColumnMetadata' {isCaseSensitive} -> isCaseSensitive) (\s@ColumnMetadata' {} a -> s {isCaseSensitive = a} :: ColumnMetadata)

-- | A value that indicates whether the column contains currency values.
columnMetadata_isCurrency :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCurrency = Lens.lens (\ColumnMetadata' {isCurrency} -> isCurrency) (\s@ColumnMetadata' {} a -> s {isCurrency = a} :: ColumnMetadata)

-- | The scale value of a decimal number column.
columnMetadata_scale :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_scale = Lens.lens (\ColumnMetadata' {scale} -> scale) (\s@ColumnMetadata' {} a -> s {scale = a} :: ColumnMetadata)

-- | The precision value of a decimal number column.
columnMetadata_precision :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_precision = Lens.lens (\ColumnMetadata' {precision} -> precision) (\s@ColumnMetadata' {} a -> s {precision = a} :: ColumnMetadata)

-- | The name of the schema that owns the table that includes the column.
columnMetadata_schemaName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_schemaName = Lens.lens (\ColumnMetadata' {schemaName} -> schemaName) (\s@ColumnMetadata' {} a -> s {schemaName = a} :: ColumnMetadata)

-- | A value that indicates whether the column increments automatically.
columnMetadata_isAutoIncrement :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isAutoIncrement = Lens.lens (\ColumnMetadata' {isAutoIncrement} -> isAutoIncrement) (\s@ColumnMetadata' {} a -> s {isAutoIncrement = a} :: ColumnMetadata)

-- | The name of the column.
columnMetadata_name :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_name = Lens.lens (\ColumnMetadata' {name} -> name) (\s@ColumnMetadata' {} a -> s {name = a} :: ColumnMetadata)

-- | The type of the column.
columnMetadata_arrayBaseColumnType :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_arrayBaseColumnType = Lens.lens (\ColumnMetadata' {arrayBaseColumnType} -> arrayBaseColumnType) (\s@ColumnMetadata' {} a -> s {arrayBaseColumnType = a} :: ColumnMetadata)

-- | The type of the column.
columnMetadata_type :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_type = Lens.lens (\ColumnMetadata' {type'} -> type') (\s@ColumnMetadata' {} a -> s {type' = a} :: ColumnMetadata)

-- | A value that indicates whether an integer column is signed.
columnMetadata_isSigned :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isSigned = Lens.lens (\ColumnMetadata' {isSigned} -> isSigned) (\s@ColumnMetadata' {} a -> s {isSigned = a} :: ColumnMetadata)

-- | The label for the column.
columnMetadata_label :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_label = Lens.lens (\ColumnMetadata' {label} -> label) (\s@ColumnMetadata' {} a -> s {label = a} :: ColumnMetadata)

-- | A value that indicates whether the column is nullable.
columnMetadata_nullable :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_nullable = Lens.lens (\ColumnMetadata' {nullable} -> nullable) (\s@ColumnMetadata' {} a -> s {nullable = a} :: ColumnMetadata)

-- | The name of the table that includes the column.
columnMetadata_tableName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_tableName = Lens.lens (\ColumnMetadata' {tableName} -> tableName) (\s@ColumnMetadata' {} a -> s {tableName = a} :: ColumnMetadata)

instance Core.FromJSON ColumnMetadata where
  parseJSON =
    Core.withObject
      "ColumnMetadata"
      ( \x ->
          ColumnMetadata'
            Prelude.<$> (x Core..:? "typeName")
            Prelude.<*> (x Core..:? "isCaseSensitive")
            Prelude.<*> (x Core..:? "isCurrency")
            Prelude.<*> (x Core..:? "scale")
            Prelude.<*> (x Core..:? "precision")
            Prelude.<*> (x Core..:? "schemaName")
            Prelude.<*> (x Core..:? "isAutoIncrement")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arrayBaseColumnType")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "isSigned")
            Prelude.<*> (x Core..:? "label")
            Prelude.<*> (x Core..:? "nullable")
            Prelude.<*> (x Core..:? "tableName")
      )

instance Prelude.Hashable ColumnMetadata where
  hashWithSalt salt' ColumnMetadata' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` nullable
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` isSigned
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arrayBaseColumnType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` isAutoIncrement
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` isCurrency
      `Prelude.hashWithSalt` isCaseSensitive
      `Prelude.hashWithSalt` typeName

instance Prelude.NFData ColumnMetadata where
  rnf ColumnMetadata' {..} =
    Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf nullable
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf isSigned
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arrayBaseColumnType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf isAutoIncrement
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf scale
      `Prelude.seq` Prelude.rnf isCurrency
      `Prelude.seq` Prelude.rnf isCaseSensitive

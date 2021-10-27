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
-- Module      : Network.AWS.RedshiftData.Types.ColumnMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RedshiftData.Types.ColumnMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties (metadata) of a column.
--
-- /See:/ 'newColumnMetadata' smart constructor.
data ColumnMetadata = ColumnMetadata'
  { -- | The length of the column.
    length :: Prelude.Maybe Prelude.Int,
    -- | The database-specific data type of the column.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column is case-sensitive.
    isCaseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | The default value of the column.
    columnDefault :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column contains currency values.
    isCurrency :: Prelude.Maybe Prelude.Bool,
    -- | The scale value of a decimal number column.
    scale :: Prelude.Maybe Prelude.Int,
    -- | The precision value of a decimal number column.
    precision :: Prelude.Maybe Prelude.Int,
    -- | The name of the schema that contains the table that includes the column.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The name of the column.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'length', 'columnMetadata_length' - The length of the column.
--
-- 'typeName', 'columnMetadata_typeName' - The database-specific data type of the column.
--
-- 'isCaseSensitive', 'columnMetadata_isCaseSensitive' - A value that indicates whether the column is case-sensitive.
--
-- 'columnDefault', 'columnMetadata_columnDefault' - The default value of the column.
--
-- 'isCurrency', 'columnMetadata_isCurrency' - A value that indicates whether the column contains currency values.
--
-- 'scale', 'columnMetadata_scale' - The scale value of a decimal number column.
--
-- 'precision', 'columnMetadata_precision' - The precision value of a decimal number column.
--
-- 'schemaName', 'columnMetadata_schemaName' - The name of the schema that contains the table that includes the column.
--
-- 'name', 'columnMetadata_name' - The name of the column.
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
    { length = Prelude.Nothing,
      typeName = Prelude.Nothing,
      isCaseSensitive = Prelude.Nothing,
      columnDefault = Prelude.Nothing,
      isCurrency = Prelude.Nothing,
      scale = Prelude.Nothing,
      precision = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      name = Prelude.Nothing,
      isSigned = Prelude.Nothing,
      label = Prelude.Nothing,
      nullable = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The length of the column.
columnMetadata_length :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_length = Lens.lens (\ColumnMetadata' {length} -> length) (\s@ColumnMetadata' {} a -> s {length = a} :: ColumnMetadata)

-- | The database-specific data type of the column.
columnMetadata_typeName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_typeName = Lens.lens (\ColumnMetadata' {typeName} -> typeName) (\s@ColumnMetadata' {} a -> s {typeName = a} :: ColumnMetadata)

-- | A value that indicates whether the column is case-sensitive.
columnMetadata_isCaseSensitive :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCaseSensitive = Lens.lens (\ColumnMetadata' {isCaseSensitive} -> isCaseSensitive) (\s@ColumnMetadata' {} a -> s {isCaseSensitive = a} :: ColumnMetadata)

-- | The default value of the column.
columnMetadata_columnDefault :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_columnDefault = Lens.lens (\ColumnMetadata' {columnDefault} -> columnDefault) (\s@ColumnMetadata' {} a -> s {columnDefault = a} :: ColumnMetadata)

-- | A value that indicates whether the column contains currency values.
columnMetadata_isCurrency :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCurrency = Lens.lens (\ColumnMetadata' {isCurrency} -> isCurrency) (\s@ColumnMetadata' {} a -> s {isCurrency = a} :: ColumnMetadata)

-- | The scale value of a decimal number column.
columnMetadata_scale :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_scale = Lens.lens (\ColumnMetadata' {scale} -> scale) (\s@ColumnMetadata' {} a -> s {scale = a} :: ColumnMetadata)

-- | The precision value of a decimal number column.
columnMetadata_precision :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_precision = Lens.lens (\ColumnMetadata' {precision} -> precision) (\s@ColumnMetadata' {} a -> s {precision = a} :: ColumnMetadata)

-- | The name of the schema that contains the table that includes the column.
columnMetadata_schemaName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_schemaName = Lens.lens (\ColumnMetadata' {schemaName} -> schemaName) (\s@ColumnMetadata' {} a -> s {schemaName = a} :: ColumnMetadata)

-- | The name of the column.
columnMetadata_name :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_name = Lens.lens (\ColumnMetadata' {name} -> name) (\s@ColumnMetadata' {} a -> s {name = a} :: ColumnMetadata)

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
            Prelude.<$> (x Core..:? "length")
            Prelude.<*> (x Core..:? "typeName")
            Prelude.<*> (x Core..:? "isCaseSensitive")
            Prelude.<*> (x Core..:? "columnDefault")
            Prelude.<*> (x Core..:? "isCurrency")
            Prelude.<*> (x Core..:? "scale")
            Prelude.<*> (x Core..:? "precision")
            Prelude.<*> (x Core..:? "schemaName")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "isSigned")
            Prelude.<*> (x Core..:? "label")
            Prelude.<*> (x Core..:? "nullable")
            Prelude.<*> (x Core..:? "tableName")
      )

instance Prelude.Hashable ColumnMetadata

instance Prelude.NFData ColumnMetadata

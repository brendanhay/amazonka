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
-- Module      : Amazonka.RedshiftData.Types.ColumnMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types.ColumnMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties (metadata) of a column.
--
-- /See:/ 'newColumnMetadata' smart constructor.
data ColumnMetadata = ColumnMetadata'
  { -- | The name of the table that includes the column.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The name of the column.
    name :: Prelude.Maybe Prelude.Text,
    -- | The label for the column.
    label :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema that contains the table that includes the column.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the column is nullable.
    nullable :: Prelude.Maybe Prelude.Int,
    -- | The length of the column.
    length :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the column is case-sensitive.
    isCaseSensitive :: Prelude.Maybe Prelude.Bool,
    -- | The database-specific data type of the column.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The default value of the column.
    columnDefault :: Prelude.Maybe Prelude.Text,
    -- | The precision value of a decimal number column.
    precision :: Prelude.Maybe Prelude.Int,
    -- | The scale value of a decimal number column.
    scale :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the column contains currency values.
    isCurrency :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether an integer column is signed.
    isSigned :: Prelude.Maybe Prelude.Bool
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
-- 'tableName', 'columnMetadata_tableName' - The name of the table that includes the column.
--
-- 'name', 'columnMetadata_name' - The name of the column.
--
-- 'label', 'columnMetadata_label' - The label for the column.
--
-- 'schemaName', 'columnMetadata_schemaName' - The name of the schema that contains the table that includes the column.
--
-- 'nullable', 'columnMetadata_nullable' - A value that indicates whether the column is nullable.
--
-- 'length', 'columnMetadata_length' - The length of the column.
--
-- 'isCaseSensitive', 'columnMetadata_isCaseSensitive' - A value that indicates whether the column is case-sensitive.
--
-- 'typeName', 'columnMetadata_typeName' - The database-specific data type of the column.
--
-- 'columnDefault', 'columnMetadata_columnDefault' - The default value of the column.
--
-- 'precision', 'columnMetadata_precision' - The precision value of a decimal number column.
--
-- 'scale', 'columnMetadata_scale' - The scale value of a decimal number column.
--
-- 'isCurrency', 'columnMetadata_isCurrency' - A value that indicates whether the column contains currency values.
--
-- 'isSigned', 'columnMetadata_isSigned' - A value that indicates whether an integer column is signed.
newColumnMetadata ::
  ColumnMetadata
newColumnMetadata =
  ColumnMetadata'
    { tableName = Prelude.Nothing,
      name = Prelude.Nothing,
      label = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      nullable = Prelude.Nothing,
      length = Prelude.Nothing,
      isCaseSensitive = Prelude.Nothing,
      typeName = Prelude.Nothing,
      columnDefault = Prelude.Nothing,
      precision = Prelude.Nothing,
      scale = Prelude.Nothing,
      isCurrency = Prelude.Nothing,
      isSigned = Prelude.Nothing
    }

-- | The name of the table that includes the column.
columnMetadata_tableName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_tableName = Lens.lens (\ColumnMetadata' {tableName} -> tableName) (\s@ColumnMetadata' {} a -> s {tableName = a} :: ColumnMetadata)

-- | The name of the column.
columnMetadata_name :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_name = Lens.lens (\ColumnMetadata' {name} -> name) (\s@ColumnMetadata' {} a -> s {name = a} :: ColumnMetadata)

-- | The label for the column.
columnMetadata_label :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_label = Lens.lens (\ColumnMetadata' {label} -> label) (\s@ColumnMetadata' {} a -> s {label = a} :: ColumnMetadata)

-- | The name of the schema that contains the table that includes the column.
columnMetadata_schemaName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_schemaName = Lens.lens (\ColumnMetadata' {schemaName} -> schemaName) (\s@ColumnMetadata' {} a -> s {schemaName = a} :: ColumnMetadata)

-- | A value that indicates whether the column is nullable.
columnMetadata_nullable :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_nullable = Lens.lens (\ColumnMetadata' {nullable} -> nullable) (\s@ColumnMetadata' {} a -> s {nullable = a} :: ColumnMetadata)

-- | The length of the column.
columnMetadata_length :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_length = Lens.lens (\ColumnMetadata' {length} -> length) (\s@ColumnMetadata' {} a -> s {length = a} :: ColumnMetadata)

-- | A value that indicates whether the column is case-sensitive.
columnMetadata_isCaseSensitive :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCaseSensitive = Lens.lens (\ColumnMetadata' {isCaseSensitive} -> isCaseSensitive) (\s@ColumnMetadata' {} a -> s {isCaseSensitive = a} :: ColumnMetadata)

-- | The database-specific data type of the column.
columnMetadata_typeName :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_typeName = Lens.lens (\ColumnMetadata' {typeName} -> typeName) (\s@ColumnMetadata' {} a -> s {typeName = a} :: ColumnMetadata)

-- | The default value of the column.
columnMetadata_columnDefault :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Text)
columnMetadata_columnDefault = Lens.lens (\ColumnMetadata' {columnDefault} -> columnDefault) (\s@ColumnMetadata' {} a -> s {columnDefault = a} :: ColumnMetadata)

-- | The precision value of a decimal number column.
columnMetadata_precision :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_precision = Lens.lens (\ColumnMetadata' {precision} -> precision) (\s@ColumnMetadata' {} a -> s {precision = a} :: ColumnMetadata)

-- | The scale value of a decimal number column.
columnMetadata_scale :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Int)
columnMetadata_scale = Lens.lens (\ColumnMetadata' {scale} -> scale) (\s@ColumnMetadata' {} a -> s {scale = a} :: ColumnMetadata)

-- | A value that indicates whether the column contains currency values.
columnMetadata_isCurrency :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isCurrency = Lens.lens (\ColumnMetadata' {isCurrency} -> isCurrency) (\s@ColumnMetadata' {} a -> s {isCurrency = a} :: ColumnMetadata)

-- | A value that indicates whether an integer column is signed.
columnMetadata_isSigned :: Lens.Lens' ColumnMetadata (Prelude.Maybe Prelude.Bool)
columnMetadata_isSigned = Lens.lens (\ColumnMetadata' {isSigned} -> isSigned) (\s@ColumnMetadata' {} a -> s {isSigned = a} :: ColumnMetadata)

instance Data.FromJSON ColumnMetadata where
  parseJSON =
    Data.withObject
      "ColumnMetadata"
      ( \x ->
          ColumnMetadata'
            Prelude.<$> (x Data..:? "tableName")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "label")
            Prelude.<*> (x Data..:? "schemaName")
            Prelude.<*> (x Data..:? "nullable")
            Prelude.<*> (x Data..:? "length")
            Prelude.<*> (x Data..:? "isCaseSensitive")
            Prelude.<*> (x Data..:? "typeName")
            Prelude.<*> (x Data..:? "columnDefault")
            Prelude.<*> (x Data..:? "precision")
            Prelude.<*> (x Data..:? "scale")
            Prelude.<*> (x Data..:? "isCurrency")
            Prelude.<*> (x Data..:? "isSigned")
      )

instance Prelude.Hashable ColumnMetadata where
  hashWithSalt _salt ColumnMetadata' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` nullable
      `Prelude.hashWithSalt` length
      `Prelude.hashWithSalt` isCaseSensitive
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` columnDefault
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` isCurrency
      `Prelude.hashWithSalt` isSigned

instance Prelude.NFData ColumnMetadata where
  rnf ColumnMetadata' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf nullable
      `Prelude.seq` Prelude.rnf length
      `Prelude.seq` Prelude.rnf isCaseSensitive
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf columnDefault
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf scale
      `Prelude.seq` Prelude.rnf isCurrency
      `Prelude.seq` Prelude.rnf isSigned

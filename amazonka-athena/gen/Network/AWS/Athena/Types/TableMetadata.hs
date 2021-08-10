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
-- Module      : Network.AWS.Athena.Types.TableMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.TableMetadata where

import Network.AWS.Athena.Types.Column
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata for a table.
--
-- /See:/ 'newTableMetadata' smart constructor.
data TableMetadata = TableMetadata'
  { -- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
    tableType :: Prelude.Maybe Prelude.Text,
    -- | The time that the table was created.
    createTime :: Prelude.Maybe Core.POSIX,
    -- | A list of the partition keys in the table.
    partitionKeys :: Prelude.Maybe [Column],
    -- | The last time the table was accessed.
    lastAccessTime :: Prelude.Maybe Core.POSIX,
    -- | A list of the columns in the table.
    columns :: Prelude.Maybe [Column],
    -- | A set of custom key\/value pairs for table properties.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the table.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableType', 'tableMetadata_tableType' - The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
--
-- 'createTime', 'tableMetadata_createTime' - The time that the table was created.
--
-- 'partitionKeys', 'tableMetadata_partitionKeys' - A list of the partition keys in the table.
--
-- 'lastAccessTime', 'tableMetadata_lastAccessTime' - The last time the table was accessed.
--
-- 'columns', 'tableMetadata_columns' - A list of the columns in the table.
--
-- 'parameters', 'tableMetadata_parameters' - A set of custom key\/value pairs for table properties.
--
-- 'name', 'tableMetadata_name' - The name of the table.
newTableMetadata ::
  -- | 'name'
  Prelude.Text ->
  TableMetadata
newTableMetadata pName_ =
  TableMetadata'
    { tableType = Prelude.Nothing,
      createTime = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      columns = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
tableMetadata_tableType :: Lens.Lens' TableMetadata (Prelude.Maybe Prelude.Text)
tableMetadata_tableType = Lens.lens (\TableMetadata' {tableType} -> tableType) (\s@TableMetadata' {} a -> s {tableType = a} :: TableMetadata)

-- | The time that the table was created.
tableMetadata_createTime :: Lens.Lens' TableMetadata (Prelude.Maybe Prelude.UTCTime)
tableMetadata_createTime = Lens.lens (\TableMetadata' {createTime} -> createTime) (\s@TableMetadata' {} a -> s {createTime = a} :: TableMetadata) Prelude.. Lens.mapping Core._Time

-- | A list of the partition keys in the table.
tableMetadata_partitionKeys :: Lens.Lens' TableMetadata (Prelude.Maybe [Column])
tableMetadata_partitionKeys = Lens.lens (\TableMetadata' {partitionKeys} -> partitionKeys) (\s@TableMetadata' {} a -> s {partitionKeys = a} :: TableMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | The last time the table was accessed.
tableMetadata_lastAccessTime :: Lens.Lens' TableMetadata (Prelude.Maybe Prelude.UTCTime)
tableMetadata_lastAccessTime = Lens.lens (\TableMetadata' {lastAccessTime} -> lastAccessTime) (\s@TableMetadata' {} a -> s {lastAccessTime = a} :: TableMetadata) Prelude.. Lens.mapping Core._Time

-- | A list of the columns in the table.
tableMetadata_columns :: Lens.Lens' TableMetadata (Prelude.Maybe [Column])
tableMetadata_columns = Lens.lens (\TableMetadata' {columns} -> columns) (\s@TableMetadata' {} a -> s {columns = a} :: TableMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | A set of custom key\/value pairs for table properties.
tableMetadata_parameters :: Lens.Lens' TableMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
tableMetadata_parameters = Lens.lens (\TableMetadata' {parameters} -> parameters) (\s@TableMetadata' {} a -> s {parameters = a} :: TableMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the table.
tableMetadata_name :: Lens.Lens' TableMetadata Prelude.Text
tableMetadata_name = Lens.lens (\TableMetadata' {name} -> name) (\s@TableMetadata' {} a -> s {name = a} :: TableMetadata)

instance Core.FromJSON TableMetadata where
  parseJSON =
    Core.withObject
      "TableMetadata"
      ( \x ->
          TableMetadata'
            Prelude.<$> (x Core..:? "TableType")
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "PartitionKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastAccessTime")
            Prelude.<*> (x Core..:? "Columns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable TableMetadata

instance Prelude.NFData TableMetadata

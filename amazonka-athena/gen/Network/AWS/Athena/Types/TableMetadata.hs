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

-- | Contains metadata for a table.
--
-- /See:/ 'newTableMetadata' smart constructor.
data TableMetadata = TableMetadata'
  { -- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
    tableType :: Core.Maybe Core.Text,
    -- | The time that the table was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | A list of the partition keys in the table.
    partitionKeys :: Core.Maybe [Column],
    -- | The last time the table was accessed.
    lastAccessTime :: Core.Maybe Core.POSIX,
    -- | A list of the columns in the table.
    columns :: Core.Maybe [Column],
    -- | A set of custom key\/value pairs for table properties.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the table.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  TableMetadata
newTableMetadata pName_ =
  TableMetadata'
    { tableType = Core.Nothing,
      createTime = Core.Nothing,
      partitionKeys = Core.Nothing,
      lastAccessTime = Core.Nothing,
      columns = Core.Nothing,
      parameters = Core.Nothing,
      name = pName_
    }

-- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
tableMetadata_tableType :: Lens.Lens' TableMetadata (Core.Maybe Core.Text)
tableMetadata_tableType = Lens.lens (\TableMetadata' {tableType} -> tableType) (\s@TableMetadata' {} a -> s {tableType = a} :: TableMetadata)

-- | The time that the table was created.
tableMetadata_createTime :: Lens.Lens' TableMetadata (Core.Maybe Core.UTCTime)
tableMetadata_createTime = Lens.lens (\TableMetadata' {createTime} -> createTime) (\s@TableMetadata' {} a -> s {createTime = a} :: TableMetadata) Core.. Lens.mapping Core._Time

-- | A list of the partition keys in the table.
tableMetadata_partitionKeys :: Lens.Lens' TableMetadata (Core.Maybe [Column])
tableMetadata_partitionKeys = Lens.lens (\TableMetadata' {partitionKeys} -> partitionKeys) (\s@TableMetadata' {} a -> s {partitionKeys = a} :: TableMetadata) Core.. Lens.mapping Lens._Coerce

-- | The last time the table was accessed.
tableMetadata_lastAccessTime :: Lens.Lens' TableMetadata (Core.Maybe Core.UTCTime)
tableMetadata_lastAccessTime = Lens.lens (\TableMetadata' {lastAccessTime} -> lastAccessTime) (\s@TableMetadata' {} a -> s {lastAccessTime = a} :: TableMetadata) Core.. Lens.mapping Core._Time

-- | A list of the columns in the table.
tableMetadata_columns :: Lens.Lens' TableMetadata (Core.Maybe [Column])
tableMetadata_columns = Lens.lens (\TableMetadata' {columns} -> columns) (\s@TableMetadata' {} a -> s {columns = a} :: TableMetadata) Core.. Lens.mapping Lens._Coerce

-- | A set of custom key\/value pairs for table properties.
tableMetadata_parameters :: Lens.Lens' TableMetadata (Core.Maybe (Core.HashMap Core.Text Core.Text))
tableMetadata_parameters = Lens.lens (\TableMetadata' {parameters} -> parameters) (\s@TableMetadata' {} a -> s {parameters = a} :: TableMetadata) Core.. Lens.mapping Lens._Coerce

-- | The name of the table.
tableMetadata_name :: Lens.Lens' TableMetadata Core.Text
tableMetadata_name = Lens.lens (\TableMetadata' {name} -> name) (\s@TableMetadata' {} a -> s {name = a} :: TableMetadata)

instance Core.FromJSON TableMetadata where
  parseJSON =
    Core.withObject
      "TableMetadata"
      ( \x ->
          TableMetadata'
            Core.<$> (x Core..:? "TableType")
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "PartitionKeys" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastAccessTime")
            Core.<*> (x Core..:? "Columns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable TableMetadata

instance Core.NFData TableMetadata

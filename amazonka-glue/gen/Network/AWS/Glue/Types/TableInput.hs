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
-- Module      : Network.AWS.Glue.Types.TableInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableInput where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.TableIdentifier
import qualified Network.AWS.Lens as Lens

-- | A structure used to define a table.
--
-- /See:/ 'newTableInput' smart constructor.
data TableInput = TableInput'
  { -- | If the table is a view, the original text of the view; otherwise @null@.
    viewOriginalText :: Core.Maybe Core.Text,
    -- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
    tableType :: Core.Maybe Core.Text,
    -- | A storage descriptor containing information about the physical storage
    -- of this table.
    storageDescriptor :: Core.Maybe StorageDescriptor,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Core.Maybe Core.POSIX,
    -- | If the table is a view, the expanded text of the view; otherwise @null@.
    viewExpandedText :: Core.Maybe Core.Text,
    -- | A @TableIdentifier@ structure that describes a target table for resource
    -- linking.
    targetTable :: Core.Maybe TableIdentifier,
    -- | The retention time for this table.
    retention :: Core.Maybe Core.Natural,
    -- | The table owner.
    owner :: Core.Maybe Core.Text,
    -- | A list of columns by which the table is partitioned. Only primitive
    -- types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify
    -- any @partitionKeys@, you must at least set the value of @partitionKeys@
    -- to an empty list. For example:
    --
    -- @\"PartitionKeys\": []@
    partitionKeys :: Core.Maybe [Column],
    -- | A description of the table.
    description :: Core.Maybe Core.Text,
    -- | The last time that the table was accessed.
    lastAccessTime :: Core.Maybe Core.POSIX,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The table name. For Hive compatibility, this is folded to lowercase when
    -- it is stored.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TableInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewOriginalText', 'tableInput_viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@.
--
-- 'tableType', 'tableInput_tableType' - The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
--
-- 'storageDescriptor', 'tableInput_storageDescriptor' - A storage descriptor containing information about the physical storage
-- of this table.
--
-- 'lastAnalyzedTime', 'tableInput_lastAnalyzedTime' - The last time that column statistics were computed for this table.
--
-- 'viewExpandedText', 'tableInput_viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@.
--
-- 'targetTable', 'tableInput_targetTable' - A @TableIdentifier@ structure that describes a target table for resource
-- linking.
--
-- 'retention', 'tableInput_retention' - The retention time for this table.
--
-- 'owner', 'tableInput_owner' - The table owner.
--
-- 'partitionKeys', 'tableInput_partitionKeys' - A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
--
-- 'description', 'tableInput_description' - A description of the table.
--
-- 'lastAccessTime', 'tableInput_lastAccessTime' - The last time that the table was accessed.
--
-- 'parameters', 'tableInput_parameters' - These key-value pairs define properties associated with the table.
--
-- 'name', 'tableInput_name' - The table name. For Hive compatibility, this is folded to lowercase when
-- it is stored.
newTableInput ::
  -- | 'name'
  Core.Text ->
  TableInput
newTableInput pName_ =
  TableInput'
    { viewOriginalText = Core.Nothing,
      tableType = Core.Nothing,
      storageDescriptor = Core.Nothing,
      lastAnalyzedTime = Core.Nothing,
      viewExpandedText = Core.Nothing,
      targetTable = Core.Nothing,
      retention = Core.Nothing,
      owner = Core.Nothing,
      partitionKeys = Core.Nothing,
      description = Core.Nothing,
      lastAccessTime = Core.Nothing,
      parameters = Core.Nothing,
      name = pName_
    }

-- | If the table is a view, the original text of the view; otherwise @null@.
tableInput_viewOriginalText :: Lens.Lens' TableInput (Core.Maybe Core.Text)
tableInput_viewOriginalText = Lens.lens (\TableInput' {viewOriginalText} -> viewOriginalText) (\s@TableInput' {} a -> s {viewOriginalText = a} :: TableInput)

-- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
tableInput_tableType :: Lens.Lens' TableInput (Core.Maybe Core.Text)
tableInput_tableType = Lens.lens (\TableInput' {tableType} -> tableType) (\s@TableInput' {} a -> s {tableType = a} :: TableInput)

-- | A storage descriptor containing information about the physical storage
-- of this table.
tableInput_storageDescriptor :: Lens.Lens' TableInput (Core.Maybe StorageDescriptor)
tableInput_storageDescriptor = Lens.lens (\TableInput' {storageDescriptor} -> storageDescriptor) (\s@TableInput' {} a -> s {storageDescriptor = a} :: TableInput)

-- | The last time that column statistics were computed for this table.
tableInput_lastAnalyzedTime :: Lens.Lens' TableInput (Core.Maybe Core.UTCTime)
tableInput_lastAnalyzedTime = Lens.lens (\TableInput' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@TableInput' {} a -> s {lastAnalyzedTime = a} :: TableInput) Core.. Lens.mapping Core._Time

-- | If the table is a view, the expanded text of the view; otherwise @null@.
tableInput_viewExpandedText :: Lens.Lens' TableInput (Core.Maybe Core.Text)
tableInput_viewExpandedText = Lens.lens (\TableInput' {viewExpandedText} -> viewExpandedText) (\s@TableInput' {} a -> s {viewExpandedText = a} :: TableInput)

-- | A @TableIdentifier@ structure that describes a target table for resource
-- linking.
tableInput_targetTable :: Lens.Lens' TableInput (Core.Maybe TableIdentifier)
tableInput_targetTable = Lens.lens (\TableInput' {targetTable} -> targetTable) (\s@TableInput' {} a -> s {targetTable = a} :: TableInput)

-- | The retention time for this table.
tableInput_retention :: Lens.Lens' TableInput (Core.Maybe Core.Natural)
tableInput_retention = Lens.lens (\TableInput' {retention} -> retention) (\s@TableInput' {} a -> s {retention = a} :: TableInput)

-- | The table owner.
tableInput_owner :: Lens.Lens' TableInput (Core.Maybe Core.Text)
tableInput_owner = Lens.lens (\TableInput' {owner} -> owner) (\s@TableInput' {} a -> s {owner = a} :: TableInput)

-- | A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
tableInput_partitionKeys :: Lens.Lens' TableInput (Core.Maybe [Column])
tableInput_partitionKeys = Lens.lens (\TableInput' {partitionKeys} -> partitionKeys) (\s@TableInput' {} a -> s {partitionKeys = a} :: TableInput) Core.. Lens.mapping Lens._Coerce

-- | A description of the table.
tableInput_description :: Lens.Lens' TableInput (Core.Maybe Core.Text)
tableInput_description = Lens.lens (\TableInput' {description} -> description) (\s@TableInput' {} a -> s {description = a} :: TableInput)

-- | The last time that the table was accessed.
tableInput_lastAccessTime :: Lens.Lens' TableInput (Core.Maybe Core.UTCTime)
tableInput_lastAccessTime = Lens.lens (\TableInput' {lastAccessTime} -> lastAccessTime) (\s@TableInput' {} a -> s {lastAccessTime = a} :: TableInput) Core.. Lens.mapping Core._Time

-- | These key-value pairs define properties associated with the table.
tableInput_parameters :: Lens.Lens' TableInput (Core.Maybe (Core.HashMap Core.Text Core.Text))
tableInput_parameters = Lens.lens (\TableInput' {parameters} -> parameters) (\s@TableInput' {} a -> s {parameters = a} :: TableInput) Core.. Lens.mapping Lens._Coerce

-- | The table name. For Hive compatibility, this is folded to lowercase when
-- it is stored.
tableInput_name :: Lens.Lens' TableInput Core.Text
tableInput_name = Lens.lens (\TableInput' {name} -> name) (\s@TableInput' {} a -> s {name = a} :: TableInput)

instance Core.Hashable TableInput

instance Core.NFData TableInput

instance Core.ToJSON TableInput where
  toJSON TableInput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ViewOriginalText" Core..=)
              Core.<$> viewOriginalText,
            ("TableType" Core..=) Core.<$> tableType,
            ("StorageDescriptor" Core..=)
              Core.<$> storageDescriptor,
            ("LastAnalyzedTime" Core..=)
              Core.<$> lastAnalyzedTime,
            ("ViewExpandedText" Core..=)
              Core.<$> viewExpandedText,
            ("TargetTable" Core..=) Core.<$> targetTable,
            ("Retention" Core..=) Core.<$> retention,
            ("Owner" Core..=) Core.<$> owner,
            ("PartitionKeys" Core..=) Core.<$> partitionKeys,
            ("Description" Core..=) Core.<$> description,
            ("LastAccessTime" Core..=) Core.<$> lastAccessTime,
            ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("Name" Core..= name)
          ]
      )

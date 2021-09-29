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
import qualified Network.AWS.Prelude as Prelude

-- | A structure used to define a table.
--
-- /See:/ 'newTableInput' smart constructor.
data TableInput = TableInput'
  { -- | If the table is a view, the original text of the view; otherwise @null@.
    viewOriginalText :: Prelude.Maybe Prelude.Text,
    -- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
    tableType :: Prelude.Maybe Prelude.Text,
    -- | A storage descriptor containing information about the physical storage
    -- of this table.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Prelude.Maybe Core.POSIX,
    -- | If the table is a view, the expanded text of the view; otherwise @null@.
    viewExpandedText :: Prelude.Maybe Prelude.Text,
    -- | A @TableIdentifier@ structure that describes a target table for resource
    -- linking.
    targetTable :: Prelude.Maybe TableIdentifier,
    -- | The retention time for this table.
    retention :: Prelude.Maybe Prelude.Natural,
    -- | The table owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | A description of the table.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of columns by which the table is partitioned. Only primitive
    -- types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify
    -- any @partitionKeys@, you must at least set the value of @partitionKeys@
    -- to an empty list. For example:
    --
    -- @\"PartitionKeys\": []@
    partitionKeys :: Prelude.Maybe [Column],
    -- | The last time that the table was accessed.
    lastAccessTime :: Prelude.Maybe Core.POSIX,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The table name. For Hive compatibility, this is folded to lowercase when
    -- it is stored.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'tableInput_description' - A description of the table.
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
-- 'lastAccessTime', 'tableInput_lastAccessTime' - The last time that the table was accessed.
--
-- 'parameters', 'tableInput_parameters' - These key-value pairs define properties associated with the table.
--
-- 'name', 'tableInput_name' - The table name. For Hive compatibility, this is folded to lowercase when
-- it is stored.
newTableInput ::
  -- | 'name'
  Prelude.Text ->
  TableInput
newTableInput pName_ =
  TableInput'
    { viewOriginalText = Prelude.Nothing,
      tableType = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      viewExpandedText = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      retention = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing,
      partitionKeys = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing,
      parameters = Prelude.Nothing,
      name = pName_
    }

-- | If the table is a view, the original text of the view; otherwise @null@.
tableInput_viewOriginalText :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Text)
tableInput_viewOriginalText = Lens.lens (\TableInput' {viewOriginalText} -> viewOriginalText) (\s@TableInput' {} a -> s {viewOriginalText = a} :: TableInput)

-- | The type of this table (@EXTERNAL_TABLE@, @VIRTUAL_VIEW@, etc.).
tableInput_tableType :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Text)
tableInput_tableType = Lens.lens (\TableInput' {tableType} -> tableType) (\s@TableInput' {} a -> s {tableType = a} :: TableInput)

-- | A storage descriptor containing information about the physical storage
-- of this table.
tableInput_storageDescriptor :: Lens.Lens' TableInput (Prelude.Maybe StorageDescriptor)
tableInput_storageDescriptor = Lens.lens (\TableInput' {storageDescriptor} -> storageDescriptor) (\s@TableInput' {} a -> s {storageDescriptor = a} :: TableInput)

-- | The last time that column statistics were computed for this table.
tableInput_lastAnalyzedTime :: Lens.Lens' TableInput (Prelude.Maybe Prelude.UTCTime)
tableInput_lastAnalyzedTime = Lens.lens (\TableInput' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@TableInput' {} a -> s {lastAnalyzedTime = a} :: TableInput) Prelude.. Lens.mapping Core._Time

-- | If the table is a view, the expanded text of the view; otherwise @null@.
tableInput_viewExpandedText :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Text)
tableInput_viewExpandedText = Lens.lens (\TableInput' {viewExpandedText} -> viewExpandedText) (\s@TableInput' {} a -> s {viewExpandedText = a} :: TableInput)

-- | A @TableIdentifier@ structure that describes a target table for resource
-- linking.
tableInput_targetTable :: Lens.Lens' TableInput (Prelude.Maybe TableIdentifier)
tableInput_targetTable = Lens.lens (\TableInput' {targetTable} -> targetTable) (\s@TableInput' {} a -> s {targetTable = a} :: TableInput)

-- | The retention time for this table.
tableInput_retention :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Natural)
tableInput_retention = Lens.lens (\TableInput' {retention} -> retention) (\s@TableInput' {} a -> s {retention = a} :: TableInput)

-- | The table owner.
tableInput_owner :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Text)
tableInput_owner = Lens.lens (\TableInput' {owner} -> owner) (\s@TableInput' {} a -> s {owner = a} :: TableInput)

-- | A description of the table.
tableInput_description :: Lens.Lens' TableInput (Prelude.Maybe Prelude.Text)
tableInput_description = Lens.lens (\TableInput' {description} -> description) (\s@TableInput' {} a -> s {description = a} :: TableInput)

-- | A list of columns by which the table is partitioned. Only primitive
-- types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify
-- any @partitionKeys@, you must at least set the value of @partitionKeys@
-- to an empty list. For example:
--
-- @\"PartitionKeys\": []@
tableInput_partitionKeys :: Lens.Lens' TableInput (Prelude.Maybe [Column])
tableInput_partitionKeys = Lens.lens (\TableInput' {partitionKeys} -> partitionKeys) (\s@TableInput' {} a -> s {partitionKeys = a} :: TableInput) Prelude.. Lens.mapping Lens._Coerce

-- | The last time that the table was accessed.
tableInput_lastAccessTime :: Lens.Lens' TableInput (Prelude.Maybe Prelude.UTCTime)
tableInput_lastAccessTime = Lens.lens (\TableInput' {lastAccessTime} -> lastAccessTime) (\s@TableInput' {} a -> s {lastAccessTime = a} :: TableInput) Prelude.. Lens.mapping Core._Time

-- | These key-value pairs define properties associated with the table.
tableInput_parameters :: Lens.Lens' TableInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
tableInput_parameters = Lens.lens (\TableInput' {parameters} -> parameters) (\s@TableInput' {} a -> s {parameters = a} :: TableInput) Prelude.. Lens.mapping Lens._Coerce

-- | The table name. For Hive compatibility, this is folded to lowercase when
-- it is stored.
tableInput_name :: Lens.Lens' TableInput Prelude.Text
tableInput_name = Lens.lens (\TableInput' {name} -> name) (\s@TableInput' {} a -> s {name = a} :: TableInput)

instance Prelude.Hashable TableInput

instance Prelude.NFData TableInput

instance Core.ToJSON TableInput where
  toJSON TableInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ViewOriginalText" Core..=)
              Prelude.<$> viewOriginalText,
            ("TableType" Core..=) Prelude.<$> tableType,
            ("StorageDescriptor" Core..=)
              Prelude.<$> storageDescriptor,
            ("LastAnalyzedTime" Core..=)
              Prelude.<$> lastAnalyzedTime,
            ("ViewExpandedText" Core..=)
              Prelude.<$> viewExpandedText,
            ("TargetTable" Core..=) Prelude.<$> targetTable,
            ("Retention" Core..=) Prelude.<$> retention,
            ("Owner" Core..=) Prelude.<$> owner,
            ("Description" Core..=) Prelude.<$> description,
            ("PartitionKeys" Core..=) Prelude.<$> partitionKeys,
            ("LastAccessTime" Core..=)
              Prelude.<$> lastAccessTime,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Core..= name)
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableInput
  ( TableInput (..),

    -- * Smart constructor
    mkTableInput,

    -- * Lenses
    tifRetention,
    tifTargetTable,
    tifTableType,
    tifOwner,
    tifViewOriginalText,
    tifViewExpandedText,
    tifLastAnalyzedTime,
    tifName,
    tifStorageDescriptor,
    tifParameters,
    tifLastAccessTime,
    tifDescription,
    tifPartitionKeys,
  )
where

import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.TableIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure used to define a table.
--
-- /See:/ 'mkTableInput' smart constructor.
data TableInput = TableInput'
  { -- | The retention time for this table.
    retention :: Lude.Maybe Lude.Natural,
    -- | A @TableIdentifier@ structure that describes a target table for resource linking.
    targetTable :: Lude.Maybe TableIdentifier,
    -- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
    tableType :: Lude.Maybe Lude.Text,
    -- | The table owner.
    owner :: Lude.Maybe Lude.Text,
    -- | If the table is a view, the original text of the view; otherwise @null@ .
    viewOriginalText :: Lude.Maybe Lude.Text,
    -- | If the table is a view, the expanded text of the view; otherwise @null@ .
    viewExpandedText :: Lude.Maybe Lude.Text,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    -- | The table name. For Hive compatibility, this is folded to lowercase when it is stored.
    name :: Lude.Text,
    -- | A storage descriptor containing information about the physical storage of this table.
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The last time that the table was accessed.
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    -- | A description of the table.
    description :: Lude.Maybe Lude.Text,
    -- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
    -- @"PartitionKeys": []@
    partitionKeys :: Lude.Maybe [Column]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableInput' with the minimum fields required to make a request.
--
-- * 'retention' - The retention time for this table.
-- * 'targetTable' - A @TableIdentifier@ structure that describes a target table for resource linking.
-- * 'tableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
-- * 'owner' - The table owner.
-- * 'viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
-- * 'viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
-- * 'lastAnalyzedTime' - The last time that column statistics were computed for this table.
-- * 'name' - The table name. For Hive compatibility, this is folded to lowercase when it is stored.
-- * 'storageDescriptor' - A storage descriptor containing information about the physical storage of this table.
-- * 'parameters' - These key-value pairs define properties associated with the table.
-- * 'lastAccessTime' - The last time that the table was accessed.
-- * 'description' - A description of the table.
-- * 'partitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
mkTableInput ::
  -- | 'name'
  Lude.Text ->
  TableInput
mkTableInput pName_ =
  TableInput'
    { retention = Lude.Nothing,
      targetTable = Lude.Nothing,
      tableType = Lude.Nothing,
      owner = Lude.Nothing,
      viewOriginalText = Lude.Nothing,
      viewExpandedText = Lude.Nothing,
      lastAnalyzedTime = Lude.Nothing,
      name = pName_,
      storageDescriptor = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      description = Lude.Nothing,
      partitionKeys = Lude.Nothing
    }

-- | The retention time for this table.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifRetention :: Lens.Lens' TableInput (Lude.Maybe Lude.Natural)
tifRetention = Lens.lens (retention :: TableInput -> Lude.Maybe Lude.Natural) (\s a -> s {retention = a} :: TableInput)
{-# DEPRECATED tifRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | A @TableIdentifier@ structure that describes a target table for resource linking.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifTargetTable :: Lens.Lens' TableInput (Lude.Maybe TableIdentifier)
tifTargetTable = Lens.lens (targetTable :: TableInput -> Lude.Maybe TableIdentifier) (\s a -> s {targetTable = a} :: TableInput)
{-# DEPRECATED tifTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifTableType :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tifTableType = Lens.lens (tableType :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {tableType = a} :: TableInput)
{-# DEPRECATED tifTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

-- | The table owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifOwner :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tifOwner = Lens.lens (owner :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: TableInput)
{-# DEPRECATED tifOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | If the table is a view, the original text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewOriginalText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifViewOriginalText :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tifViewOriginalText = Lens.lens (viewOriginalText :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {viewOriginalText = a} :: TableInput)
{-# DEPRECATED tifViewOriginalText "Use generic-lens or generic-optics with 'viewOriginalText' instead." #-}

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewExpandedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifViewExpandedText :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tifViewExpandedText = Lens.lens (viewExpandedText :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {viewExpandedText = a} :: TableInput)
{-# DEPRECATED tifViewExpandedText "Use generic-lens or generic-optics with 'viewExpandedText' instead." #-}

-- | The last time that column statistics were computed for this table.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifLastAnalyzedTime :: Lens.Lens' TableInput (Lude.Maybe Lude.Timestamp)
tifLastAnalyzedTime = Lens.lens (lastAnalyzedTime :: TableInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAnalyzedTime = a} :: TableInput)
{-# DEPRECATED tifLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | The table name. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifName :: Lens.Lens' TableInput Lude.Text
tifName = Lens.lens (name :: TableInput -> Lude.Text) (\s a -> s {name = a} :: TableInput)
{-# DEPRECATED tifName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A storage descriptor containing information about the physical storage of this table.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifStorageDescriptor :: Lens.Lens' TableInput (Lude.Maybe StorageDescriptor)
tifStorageDescriptor = Lens.lens (storageDescriptor :: TableInput -> Lude.Maybe StorageDescriptor) (\s a -> s {storageDescriptor = a} :: TableInput)
{-# DEPRECATED tifStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | These key-value pairs define properties associated with the table.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifParameters :: Lens.Lens' TableInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tifParameters = Lens.lens (parameters :: TableInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: TableInput)
{-# DEPRECATED tifParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The last time that the table was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifLastAccessTime :: Lens.Lens' TableInput (Lude.Maybe Lude.Timestamp)
tifLastAccessTime = Lens.lens (lastAccessTime :: TableInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: TableInput)
{-# DEPRECATED tifLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | A description of the table.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifDescription :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tifDescription = Lens.lens (description :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TableInput)
{-# DEPRECATED tifDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifPartitionKeys :: Lens.Lens' TableInput (Lude.Maybe [Column])
tifPartitionKeys = Lens.lens (partitionKeys :: TableInput -> Lude.Maybe [Column]) (\s a -> s {partitionKeys = a} :: TableInput)
{-# DEPRECATED tifPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

instance Lude.ToJSON TableInput where
  toJSON TableInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Retention" Lude..=) Lude.<$> retention,
            ("TargetTable" Lude..=) Lude.<$> targetTable,
            ("TableType" Lude..=) Lude.<$> tableType,
            ("Owner" Lude..=) Lude.<$> owner,
            ("ViewOriginalText" Lude..=) Lude.<$> viewOriginalText,
            ("ViewExpandedText" Lude..=) Lude.<$> viewExpandedText,
            ("LastAnalyzedTime" Lude..=) Lude.<$> lastAnalyzedTime,
            Lude.Just ("Name" Lude..= name),
            ("StorageDescriptor" Lude..=) Lude.<$> storageDescriptor,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("LastAccessTime" Lude..=) Lude.<$> lastAccessTime,
            ("Description" Lude..=) Lude.<$> description,
            ("PartitionKeys" Lude..=) Lude.<$> partitionKeys
          ]
      )

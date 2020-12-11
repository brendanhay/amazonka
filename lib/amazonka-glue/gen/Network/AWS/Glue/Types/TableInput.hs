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
    tabRetention,
    tabTargetTable,
    tabTableType,
    tabOwner,
    tabViewOriginalText,
    tabViewExpandedText,
    tabLastAnalyzedTime,
    tabStorageDescriptor,
    tabParameters,
    tabLastAccessTime,
    tabDescription,
    tabPartitionKeys,
    tabName,
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
  { retention :: Lude.Maybe Lude.Natural,
    targetTable :: Lude.Maybe TableIdentifier,
    tableType :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    viewOriginalText :: Lude.Maybe Lude.Text,
    viewExpandedText :: Lude.Maybe Lude.Text,
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    partitionKeys :: Lude.Maybe [Column],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableInput' with the minimum fields required to make a request.
--
-- * 'description' - A description of the table.
-- * 'lastAccessTime' - The last time that the table was accessed.
-- * 'lastAnalyzedTime' - The last time that column statistics were computed for this table.
-- * 'name' - The table name. For Hive compatibility, this is folded to lowercase when it is stored.
-- * 'owner' - The table owner.
-- * 'parameters' - These key-value pairs define properties associated with the table.
-- * 'partitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
-- * 'retention' - The retention time for this table.
-- * 'storageDescriptor' - A storage descriptor containing information about the physical storage of this table.
-- * 'tableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
-- * 'targetTable' - A @TableIdentifier@ structure that describes a target table for resource linking.
-- * 'viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
-- * 'viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
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
      storageDescriptor = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      description = Lude.Nothing,
      partitionKeys = Lude.Nothing,
      name = pName_
    }

-- | The retention time for this table.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabRetention :: Lens.Lens' TableInput (Lude.Maybe Lude.Natural)
tabRetention = Lens.lens (retention :: TableInput -> Lude.Maybe Lude.Natural) (\s a -> s {retention = a} :: TableInput)
{-# DEPRECATED tabRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | A @TableIdentifier@ structure that describes a target table for resource linking.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabTargetTable :: Lens.Lens' TableInput (Lude.Maybe TableIdentifier)
tabTargetTable = Lens.lens (targetTable :: TableInput -> Lude.Maybe TableIdentifier) (\s a -> s {targetTable = a} :: TableInput)
{-# DEPRECATED tabTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabTableType :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tabTableType = Lens.lens (tableType :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {tableType = a} :: TableInput)
{-# DEPRECATED tabTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

-- | The table owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabOwner :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tabOwner = Lens.lens (owner :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: TableInput)
{-# DEPRECATED tabOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | If the table is a view, the original text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewOriginalText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabViewOriginalText :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tabViewOriginalText = Lens.lens (viewOriginalText :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {viewOriginalText = a} :: TableInput)
{-# DEPRECATED tabViewOriginalText "Use generic-lens or generic-optics with 'viewOriginalText' instead." #-}

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewExpandedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabViewExpandedText :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tabViewExpandedText = Lens.lens (viewExpandedText :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {viewExpandedText = a} :: TableInput)
{-# DEPRECATED tabViewExpandedText "Use generic-lens or generic-optics with 'viewExpandedText' instead." #-}

-- | The last time that column statistics were computed for this table.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabLastAnalyzedTime :: Lens.Lens' TableInput (Lude.Maybe Lude.Timestamp)
tabLastAnalyzedTime = Lens.lens (lastAnalyzedTime :: TableInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAnalyzedTime = a} :: TableInput)
{-# DEPRECATED tabLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | A storage descriptor containing information about the physical storage of this table.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabStorageDescriptor :: Lens.Lens' TableInput (Lude.Maybe StorageDescriptor)
tabStorageDescriptor = Lens.lens (storageDescriptor :: TableInput -> Lude.Maybe StorageDescriptor) (\s a -> s {storageDescriptor = a} :: TableInput)
{-# DEPRECATED tabStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | These key-value pairs define properties associated with the table.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabParameters :: Lens.Lens' TableInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tabParameters = Lens.lens (parameters :: TableInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: TableInput)
{-# DEPRECATED tabParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The last time that the table was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabLastAccessTime :: Lens.Lens' TableInput (Lude.Maybe Lude.Timestamp)
tabLastAccessTime = Lens.lens (lastAccessTime :: TableInput -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: TableInput)
{-# DEPRECATED tabLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | A description of the table.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabDescription :: Lens.Lens' TableInput (Lude.Maybe Lude.Text)
tabDescription = Lens.lens (description :: TableInput -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TableInput)
{-# DEPRECATED tabDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabPartitionKeys :: Lens.Lens' TableInput (Lude.Maybe [Column])
tabPartitionKeys = Lens.lens (partitionKeys :: TableInput -> Lude.Maybe [Column]) (\s a -> s {partitionKeys = a} :: TableInput)
{-# DEPRECATED tabPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

-- | The table name. For Hive compatibility, this is folded to lowercase when it is stored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tabName :: Lens.Lens' TableInput Lude.Text
tabName = Lens.lens (name :: TableInput -> Lude.Text) (\s a -> s {name = a} :: TableInput)
{-# DEPRECATED tabName "Use generic-lens or generic-optics with 'name' instead." #-}

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
            ("StorageDescriptor" Lude..=) Lude.<$> storageDescriptor,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("LastAccessTime" Lude..=) Lude.<$> lastAccessTime,
            ("Description" Lude..=) Lude.<$> description,
            ("PartitionKeys" Lude..=) Lude.<$> partitionKeys,
            Lude.Just ("Name" Lude..= name)
          ]
      )

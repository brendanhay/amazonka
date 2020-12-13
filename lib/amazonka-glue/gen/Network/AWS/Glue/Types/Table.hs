{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Table
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Table
  ( Table (..),

    -- * Smart constructor
    mkTable,

    -- * Lenses
    tRetention,
    tTargetTable,
    tIsRegisteredWithLakeFormation,
    tCreatedBy,
    tTableType,
    tCatalogId,
    tOwner,
    tViewOriginalText,
    tUpdateTime,
    tViewExpandedText,
    tLastAnalyzedTime,
    tName,
    tStorageDescriptor,
    tDatabaseName,
    tParameters,
    tLastAccessTime,
    tDescription,
    tPartitionKeys,
    tCreateTime,
  )
where

import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.TableIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a collection of related data organized in columns and rows.
--
-- /See:/ 'mkTable' smart constructor.
data Table = Table'
  { -- | The retention time for this table.
    retention :: Lude.Maybe Lude.Natural,
    -- | A @TableIdentifier@ structure that describes a target table for resource linking.
    targetTable :: Lude.Maybe TableIdentifier,
    -- | Indicates whether the table has been registered with AWS Lake Formation.
    isRegisteredWithLakeFormation :: Lude.Maybe Lude.Bool,
    -- | The person or entity who created the table.
    createdBy :: Lude.Maybe Lude.Text,
    -- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
    tableType :: Lude.Maybe Lude.Text,
    -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Lude.Maybe Lude.Text,
    -- | The owner of the table.
    owner :: Lude.Maybe Lude.Text,
    -- | If the table is a view, the original text of the view; otherwise @null@ .
    viewOriginalText :: Lude.Maybe Lude.Text,
    -- | The last time that the table was updated.
    updateTime :: Lude.Maybe Lude.Timestamp,
    -- | If the table is a view, the expanded text of the view; otherwise @null@ .
    viewExpandedText :: Lude.Maybe Lude.Text,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    -- | The table name. For Hive compatibility, this must be entirely lowercase.
    name :: Lude.Text,
    -- | A storage descriptor containing information about the physical storage of this table.
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    -- | The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
    databaseName :: Lude.Maybe Lude.Text,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    -- | A description of the table.
    description :: Lude.Maybe Lude.Text,
    -- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
    -- @"PartitionKeys": []@
    partitionKeys :: Lude.Maybe [Column],
    -- | The time when the table definition was created in the Data Catalog.
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Table' with the minimum fields required to make a request.
--
-- * 'retention' - The retention time for this table.
-- * 'targetTable' - A @TableIdentifier@ structure that describes a target table for resource linking.
-- * 'isRegisteredWithLakeFormation' - Indicates whether the table has been registered with AWS Lake Formation.
-- * 'createdBy' - The person or entity who created the table.
-- * 'tableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
-- * 'catalogId' - The ID of the Data Catalog in which the table resides.
-- * 'owner' - The owner of the table.
-- * 'viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
-- * 'updateTime' - The last time that the table was updated.
-- * 'viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
-- * 'lastAnalyzedTime' - The last time that column statistics were computed for this table.
-- * 'name' - The table name. For Hive compatibility, this must be entirely lowercase.
-- * 'storageDescriptor' - A storage descriptor containing information about the physical storage of this table.
-- * 'databaseName' - The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
-- * 'parameters' - These key-value pairs define properties associated with the table.
-- * 'lastAccessTime' - The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
-- * 'description' - A description of the table.
-- * 'partitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
-- * 'createTime' - The time when the table definition was created in the Data Catalog.
mkTable ::
  -- | 'name'
  Lude.Text ->
  Table
mkTable pName_ =
  Table'
    { retention = Lude.Nothing,
      targetTable = Lude.Nothing,
      isRegisteredWithLakeFormation = Lude.Nothing,
      createdBy = Lude.Nothing,
      tableType = Lude.Nothing,
      catalogId = Lude.Nothing,
      owner = Lude.Nothing,
      viewOriginalText = Lude.Nothing,
      updateTime = Lude.Nothing,
      viewExpandedText = Lude.Nothing,
      lastAnalyzedTime = Lude.Nothing,
      name = pName_,
      storageDescriptor = Lude.Nothing,
      databaseName = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      description = Lude.Nothing,
      partitionKeys = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The retention time for this table.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetention :: Lens.Lens' Table (Lude.Maybe Lude.Natural)
tRetention = Lens.lens (retention :: Table -> Lude.Maybe Lude.Natural) (\s a -> s {retention = a} :: Table)
{-# DEPRECATED tRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | A @TableIdentifier@ structure that describes a target table for resource linking.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTargetTable :: Lens.Lens' Table (Lude.Maybe TableIdentifier)
tTargetTable = Lens.lens (targetTable :: Table -> Lude.Maybe TableIdentifier) (\s a -> s {targetTable = a} :: Table)
{-# DEPRECATED tTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | Indicates whether the table has been registered with AWS Lake Formation.
--
-- /Note:/ Consider using 'isRegisteredWithLakeFormation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsRegisteredWithLakeFormation :: Lens.Lens' Table (Lude.Maybe Lude.Bool)
tIsRegisteredWithLakeFormation = Lens.lens (isRegisteredWithLakeFormation :: Table -> Lude.Maybe Lude.Bool) (\s a -> s {isRegisteredWithLakeFormation = a} :: Table)
{-# DEPRECATED tIsRegisteredWithLakeFormation "Use generic-lens or generic-optics with 'isRegisteredWithLakeFormation' instead." #-}

-- | The person or entity who created the table.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedBy :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tCreatedBy = Lens.lens (createdBy :: Table -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: Table)
{-# DEPRECATED tCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTableType :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tTableType = Lens.lens (tableType :: Table -> Lude.Maybe Lude.Text) (\s a -> s {tableType = a} :: Table)
{-# DEPRECATED tTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

-- | The ID of the Data Catalog in which the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCatalogId :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tCatalogId = Lens.lens (catalogId :: Table -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: Table)
{-# DEPRECATED tCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The owner of the table.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOwner :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tOwner = Lens.lens (owner :: Table -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: Table)
{-# DEPRECATED tOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | If the table is a view, the original text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewOriginalText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tViewOriginalText :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tViewOriginalText = Lens.lens (viewOriginalText :: Table -> Lude.Maybe Lude.Text) (\s a -> s {viewOriginalText = a} :: Table)
{-# DEPRECATED tViewOriginalText "Use generic-lens or generic-optics with 'viewOriginalText' instead." #-}

-- | The last time that the table was updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tUpdateTime :: Lens.Lens' Table (Lude.Maybe Lude.Timestamp)
tUpdateTime = Lens.lens (updateTime :: Table -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateTime = a} :: Table)
{-# DEPRECATED tUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewExpandedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tViewExpandedText :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tViewExpandedText = Lens.lens (viewExpandedText :: Table -> Lude.Maybe Lude.Text) (\s a -> s {viewExpandedText = a} :: Table)
{-# DEPRECATED tViewExpandedText "Use generic-lens or generic-optics with 'viewExpandedText' instead." #-}

-- | The last time that column statistics were computed for this table.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastAnalyzedTime :: Lens.Lens' Table (Lude.Maybe Lude.Timestamp)
tLastAnalyzedTime = Lens.lens (lastAnalyzedTime :: Table -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAnalyzedTime = a} :: Table)
{-# DEPRECATED tLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | The table name. For Hive compatibility, this must be entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Table Lude.Text
tName = Lens.lens (name :: Table -> Lude.Text) (\s a -> s {name = a} :: Table)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A storage descriptor containing information about the physical storage of this table.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStorageDescriptor :: Lens.Lens' Table (Lude.Maybe StorageDescriptor)
tStorageDescriptor = Lens.lens (storageDescriptor :: Table -> Lude.Maybe StorageDescriptor) (\s a -> s {storageDescriptor = a} :: Table)
{-# DEPRECATED tStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDatabaseName :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tDatabaseName = Lens.lens (databaseName :: Table -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: Table)
{-# DEPRECATED tDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | These key-value pairs define properties associated with the table.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tParameters :: Lens.Lens' Table (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tParameters = Lens.lens (parameters :: Table -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Table)
{-# DEPRECATED tParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastAccessTime :: Lens.Lens' Table (Lude.Maybe Lude.Timestamp)
tLastAccessTime = Lens.lens (lastAccessTime :: Table -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastAccessTime = a} :: Table)
{-# DEPRECATED tLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | A description of the table.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDescription :: Lens.Lens' Table (Lude.Maybe Lude.Text)
tDescription = Lens.lens (description :: Table -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Table)
{-# DEPRECATED tDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPartitionKeys :: Lens.Lens' Table (Lude.Maybe [Column])
tPartitionKeys = Lens.lens (partitionKeys :: Table -> Lude.Maybe [Column]) (\s a -> s {partitionKeys = a} :: Table)
{-# DEPRECATED tPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

-- | The time when the table definition was created in the Data Catalog.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreateTime :: Lens.Lens' Table (Lude.Maybe Lude.Timestamp)
tCreateTime = Lens.lens (createTime :: Table -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: Table)
{-# DEPRECATED tCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON Table where
  parseJSON =
    Lude.withObject
      "Table"
      ( \x ->
          Table'
            Lude.<$> (x Lude..:? "Retention")
            Lude.<*> (x Lude..:? "TargetTable")
            Lude.<*> (x Lude..:? "IsRegisteredWithLakeFormation")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "TableType")
            Lude.<*> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "ViewOriginalText")
            Lude.<*> (x Lude..:? "UpdateTime")
            Lude.<*> (x Lude..:? "ViewExpandedText")
            Lude.<*> (x Lude..:? "LastAnalyzedTime")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..:? "StorageDescriptor")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastAccessTime")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "PartitionKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreateTime")
      )

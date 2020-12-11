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
    tStorageDescriptor,
    tDatabaseName,
    tParameters,
    tLastAccessTime,
    tDescription,
    tPartitionKeys,
    tCreateTime,
    tName,
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
  { retention :: Lude.Maybe Lude.Natural,
    targetTable :: Lude.Maybe TableIdentifier,
    isRegisteredWithLakeFormation :: Lude.Maybe Lude.Bool,
    createdBy :: Lude.Maybe Lude.Text,
    tableType :: Lude.Maybe Lude.Text,
    catalogId :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    viewOriginalText :: Lude.Maybe Lude.Text,
    updateTime :: Lude.Maybe Lude.Timestamp,
    viewExpandedText :: Lude.Maybe Lude.Text,
    lastAnalyzedTime :: Lude.Maybe Lude.Timestamp,
    storageDescriptor :: Lude.Maybe StorageDescriptor,
    databaseName :: Lude.Maybe Lude.Text,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    lastAccessTime :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    partitionKeys :: Lude.Maybe [Column],
    createTime :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'Table' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the table resides.
-- * 'createTime' - The time when the table definition was created in the Data Catalog.
-- * 'createdBy' - The person or entity who created the table.
-- * 'databaseName' - The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
-- * 'description' - A description of the table.
-- * 'isRegisteredWithLakeFormation' - Indicates whether the table has been registered with AWS Lake Formation.
-- * 'lastAccessTime' - The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
-- * 'lastAnalyzedTime' - The last time that column statistics were computed for this table.
-- * 'name' - The table name. For Hive compatibility, this must be entirely lowercase.
-- * 'owner' - The owner of the table.
-- * 'parameters' - These key-value pairs define properties associated with the table.
-- * 'partitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
-- * 'retention' - The retention time for this table.
-- * 'storageDescriptor' - A storage descriptor containing information about the physical storage of this table.
-- * 'tableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
-- * 'targetTable' - A @TableIdentifier@ structure that describes a target table for resource linking.
-- * 'updateTime' - The last time that the table was updated.
-- * 'viewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
-- * 'viewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
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
      storageDescriptor = Lude.Nothing,
      databaseName = Lude.Nothing,
      parameters = Lude.Nothing,
      lastAccessTime = Lude.Nothing,
      description = Lude.Nothing,
      partitionKeys = Lude.Nothing,
      createTime = Lude.Nothing,
      name = pName_
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

-- | The table name. For Hive compatibility, this must be entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Table Lude.Text
tName = Lens.lens (name :: Table -> Lude.Text) (\s a -> s {name = a} :: Table)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

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
            Lude.<*> (x Lude..:? "StorageDescriptor")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LastAccessTime")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "PartitionKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreateTime")
            Lude.<*> (x Lude..: "Name")
      )

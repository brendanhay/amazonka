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
    tName,
    tCatalogId,
    tCreateTime,
    tCreatedBy,
    tDatabaseName,
    tDescription,
    tIsRegisteredWithLakeFormation,
    tLastAccessTime,
    tLastAnalyzedTime,
    tOwner,
    tParameters,
    tPartitionKeys,
    tRetention,
    tStorageDescriptor,
    tTableType,
    tTargetTable,
    tUpdateTime,
    tViewExpandedText,
    tViewOriginalText,
  )
where

import qualified Network.AWS.Glue.Types.CatalogIdString as Types
import qualified Network.AWS.Glue.Types.Column as Types
import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.KeyString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.ParametersMapValue as Types
import qualified Network.AWS.Glue.Types.StorageDescriptor as Types
import qualified Network.AWS.Glue.Types.TableIdentifier as Types
import qualified Network.AWS.Glue.Types.TableTypeString as Types
import qualified Network.AWS.Glue.Types.ViewTextString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a collection of related data organized in columns and rows.
--
-- /See:/ 'mkTable' smart constructor.
data Table = Table'
  { -- | The table name. For Hive compatibility, this must be entirely lowercase.
    name :: Types.NameString,
    -- | The ID of the Data Catalog in which the table resides.
    catalogId :: Core.Maybe Types.CatalogIdString,
    -- | The time when the table definition was created in the Data Catalog.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The person or entity who created the table.
    createdBy :: Core.Maybe Types.NameString,
    -- | The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
    databaseName :: Core.Maybe Types.NameString,
    -- | A description of the table.
    description :: Core.Maybe Types.DescriptionString,
    -- | Indicates whether the table has been registered with AWS Lake Formation.
    isRegisteredWithLakeFormation :: Core.Maybe Core.Bool,
    -- | The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
    lastAccessTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time that column statistics were computed for this table.
    lastAnalyzedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The owner of the table.
    owner :: Core.Maybe Types.NameString,
    -- | These key-value pairs define properties associated with the table.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
    --
    -- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
    -- @"PartitionKeys": []@
    partitionKeys :: Core.Maybe [Types.Column],
    -- | The retention time for this table.
    retention :: Core.Maybe Core.Natural,
    -- | A storage descriptor containing information about the physical storage of this table.
    storageDescriptor :: Core.Maybe Types.StorageDescriptor,
    -- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
    tableType :: Core.Maybe Types.TableTypeString,
    -- | A @TableIdentifier@ structure that describes a target table for resource linking.
    targetTable :: Core.Maybe Types.TableIdentifier,
    -- | The last time that the table was updated.
    updateTime :: Core.Maybe Core.NominalDiffTime,
    -- | If the table is a view, the expanded text of the view; otherwise @null@ .
    viewExpandedText :: Core.Maybe Types.ViewTextString,
    -- | If the table is a view, the original text of the view; otherwise @null@ .
    viewOriginalText :: Core.Maybe Types.ViewTextString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Table' value with any optional fields omitted.
mkTable ::
  -- | 'name'
  Types.NameString ->
  Table
mkTable name =
  Table'
    { name,
      catalogId = Core.Nothing,
      createTime = Core.Nothing,
      createdBy = Core.Nothing,
      databaseName = Core.Nothing,
      description = Core.Nothing,
      isRegisteredWithLakeFormation = Core.Nothing,
      lastAccessTime = Core.Nothing,
      lastAnalyzedTime = Core.Nothing,
      owner = Core.Nothing,
      parameters = Core.Nothing,
      partitionKeys = Core.Nothing,
      retention = Core.Nothing,
      storageDescriptor = Core.Nothing,
      tableType = Core.Nothing,
      targetTable = Core.Nothing,
      updateTime = Core.Nothing,
      viewExpandedText = Core.Nothing,
      viewOriginalText = Core.Nothing
    }

-- | The table name. For Hive compatibility, this must be entirely lowercase.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Table Types.NameString
tName = Lens.field @"name"
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Data Catalog in which the table resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCatalogId :: Lens.Lens' Table (Core.Maybe Types.CatalogIdString)
tCatalogId = Lens.field @"catalogId"
{-# DEPRECATED tCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The time when the table definition was created in the Data Catalog.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreateTime :: Lens.Lens' Table (Core.Maybe Core.NominalDiffTime)
tCreateTime = Lens.field @"createTime"
{-# DEPRECATED tCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The person or entity who created the table.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedBy :: Lens.Lens' Table (Core.Maybe Types.NameString)
tCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED tCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The name of the database where the table metadata resides. For Hive compatibility, this must be all lowercase.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDatabaseName :: Lens.Lens' Table (Core.Maybe Types.NameString)
tDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED tDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A description of the table.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDescription :: Lens.Lens' Table (Core.Maybe Types.DescriptionString)
tDescription = Lens.field @"description"
{-# DEPRECATED tDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether the table has been registered with AWS Lake Formation.
--
-- /Note:/ Consider using 'isRegisteredWithLakeFormation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tIsRegisteredWithLakeFormation :: Lens.Lens' Table (Core.Maybe Core.Bool)
tIsRegisteredWithLakeFormation = Lens.field @"isRegisteredWithLakeFormation"
{-# DEPRECATED tIsRegisteredWithLakeFormation "Use generic-lens or generic-optics with 'isRegisteredWithLakeFormation' instead." #-}

-- | The last time that the table was accessed. This is usually taken from HDFS, and might not be reliable.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastAccessTime :: Lens.Lens' Table (Core.Maybe Core.NominalDiffTime)
tLastAccessTime = Lens.field @"lastAccessTime"
{-# DEPRECATED tLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | The last time that column statistics were computed for this table.
--
-- /Note:/ Consider using 'lastAnalyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastAnalyzedTime :: Lens.Lens' Table (Core.Maybe Core.NominalDiffTime)
tLastAnalyzedTime = Lens.field @"lastAnalyzedTime"
{-# DEPRECATED tLastAnalyzedTime "Use generic-lens or generic-optics with 'lastAnalyzedTime' instead." #-}

-- | The owner of the table.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOwner :: Lens.Lens' Table (Core.Maybe Types.NameString)
tOwner = Lens.field @"owner"
{-# DEPRECATED tOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | These key-value pairs define properties associated with the table.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tParameters :: Lens.Lens' Table (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
tParameters = Lens.field @"parameters"
{-# DEPRECATED tParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- When you create a table used by Amazon Athena, and you do not specify any @partitionKeys@ , you must at least set the value of @partitionKeys@ to an empty list. For example:
-- @"PartitionKeys": []@
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPartitionKeys :: Lens.Lens' Table (Core.Maybe [Types.Column])
tPartitionKeys = Lens.field @"partitionKeys"
{-# DEPRECATED tPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

-- | The retention time for this table.
--
-- /Note:/ Consider using 'retention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetention :: Lens.Lens' Table (Core.Maybe Core.Natural)
tRetention = Lens.field @"retention"
{-# DEPRECATED tRetention "Use generic-lens or generic-optics with 'retention' instead." #-}

-- | A storage descriptor containing information about the physical storage of this table.
--
-- /Note:/ Consider using 'storageDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStorageDescriptor :: Lens.Lens' Table (Core.Maybe Types.StorageDescriptor)
tStorageDescriptor = Lens.field @"storageDescriptor"
{-# DEPRECATED tStorageDescriptor "Use generic-lens or generic-optics with 'storageDescriptor' instead." #-}

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTableType :: Lens.Lens' Table (Core.Maybe Types.TableTypeString)
tTableType = Lens.field @"tableType"
{-# DEPRECATED tTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

-- | A @TableIdentifier@ structure that describes a target table for resource linking.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTargetTable :: Lens.Lens' Table (Core.Maybe Types.TableIdentifier)
tTargetTable = Lens.field @"targetTable"
{-# DEPRECATED tTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | The last time that the table was updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tUpdateTime :: Lens.Lens' Table (Core.Maybe Core.NominalDiffTime)
tUpdateTime = Lens.field @"updateTime"
{-# DEPRECATED tUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewExpandedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tViewExpandedText :: Lens.Lens' Table (Core.Maybe Types.ViewTextString)
tViewExpandedText = Lens.field @"viewExpandedText"
{-# DEPRECATED tViewExpandedText "Use generic-lens or generic-optics with 'viewExpandedText' instead." #-}

-- | If the table is a view, the original text of the view; otherwise @null@ .
--
-- /Note:/ Consider using 'viewOriginalText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tViewOriginalText :: Lens.Lens' Table (Core.Maybe Types.ViewTextString)
tViewOriginalText = Lens.field @"viewOriginalText"
{-# DEPRECATED tViewOriginalText "Use generic-lens or generic-optics with 'viewOriginalText' instead." #-}

instance Core.FromJSON Table where
  parseJSON =
    Core.withObject "Table" Core.$
      \x ->
        Table'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "CatalogId")
          Core.<*> (x Core..:? "CreateTime")
          Core.<*> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "DatabaseName")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsRegisteredWithLakeFormation")
          Core.<*> (x Core..:? "LastAccessTime")
          Core.<*> (x Core..:? "LastAnalyzedTime")
          Core.<*> (x Core..:? "Owner")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "PartitionKeys")
          Core.<*> (x Core..:? "Retention")
          Core.<*> (x Core..:? "StorageDescriptor")
          Core.<*> (x Core..:? "TableType")
          Core.<*> (x Core..:? "TargetTable")
          Core.<*> (x Core..:? "UpdateTime")
          Core.<*> (x Core..:? "ViewExpandedText")
          Core.<*> (x Core..:? "ViewOriginalText")

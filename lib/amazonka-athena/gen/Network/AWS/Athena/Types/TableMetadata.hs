{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.TableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.TableMetadata
  ( TableMetadata (..),

    -- * Smart constructor
    mkTableMetadata,

    -- * Lenses
    tmName,
    tmColumns,
    tmCreateTime,
    tmLastAccessTime,
    tmParameters,
    tmPartitionKeys,
    tmTableType,
  )
where

import qualified Network.AWS.Athena.Types.Column as Types
import qualified Network.AWS.Athena.Types.KeyString as Types
import qualified Network.AWS.Athena.Types.NameString as Types
import qualified Network.AWS.Athena.Types.ParametersMapValue as Types
import qualified Network.AWS.Athena.Types.TableType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains metadata for a table.
--
-- /See:/ 'mkTableMetadata' smart constructor.
data TableMetadata = TableMetadata'
  { -- | The name of the table.
    name :: Types.NameString,
    -- | A list of the columns in the table.
    columns :: Core.Maybe [Types.Column],
    -- | The time that the table was created.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time the table was accessed.
    lastAccessTime :: Core.Maybe Core.NominalDiffTime,
    -- | A set of custom key/value pairs for table properties.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | A list of the partition keys in the table.
    partitionKeys :: Core.Maybe [Types.Column],
    -- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
    tableType :: Core.Maybe Types.TableType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TableMetadata' value with any optional fields omitted.
mkTableMetadata ::
  -- | 'name'
  Types.NameString ->
  TableMetadata
mkTableMetadata name =
  TableMetadata'
    { name,
      columns = Core.Nothing,
      createTime = Core.Nothing,
      lastAccessTime = Core.Nothing,
      parameters = Core.Nothing,
      partitionKeys = Core.Nothing,
      tableType = Core.Nothing
    }

-- | The name of the table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmName :: Lens.Lens' TableMetadata Types.NameString
tmName = Lens.field @"name"
{-# DEPRECATED tmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of the columns in the table.
--
-- /Note:/ Consider using 'columns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmColumns :: Lens.Lens' TableMetadata (Core.Maybe [Types.Column])
tmColumns = Lens.field @"columns"
{-# DEPRECATED tmColumns "Use generic-lens or generic-optics with 'columns' instead." #-}

-- | The time that the table was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmCreateTime :: Lens.Lens' TableMetadata (Core.Maybe Core.NominalDiffTime)
tmCreateTime = Lens.field @"createTime"
{-# DEPRECATED tmCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The last time the table was accessed.
--
-- /Note:/ Consider using 'lastAccessTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmLastAccessTime :: Lens.Lens' TableMetadata (Core.Maybe Core.NominalDiffTime)
tmLastAccessTime = Lens.field @"lastAccessTime"
{-# DEPRECATED tmLastAccessTime "Use generic-lens or generic-optics with 'lastAccessTime' instead." #-}

-- | A set of custom key/value pairs for table properties.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmParameters :: Lens.Lens' TableMetadata (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
tmParameters = Lens.field @"parameters"
{-# DEPRECATED tmParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of the partition keys in the table.
--
-- /Note:/ Consider using 'partitionKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmPartitionKeys :: Lens.Lens' TableMetadata (Core.Maybe [Types.Column])
tmPartitionKeys = Lens.field @"partitionKeys"
{-# DEPRECATED tmPartitionKeys "Use generic-lens or generic-optics with 'partitionKeys' instead." #-}

-- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
--
-- /Note:/ Consider using 'tableType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTableType :: Lens.Lens' TableMetadata (Core.Maybe Types.TableType)
tmTableType = Lens.field @"tableType"
{-# DEPRECATED tmTableType "Use generic-lens or generic-optics with 'tableType' instead." #-}

instance Core.FromJSON TableMetadata where
  parseJSON =
    Core.withObject "TableMetadata" Core.$
      \x ->
        TableMetadata'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "Columns")
          Core.<*> (x Core..:? "CreateTime")
          Core.<*> (x Core..:? "LastAccessTime")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "PartitionKeys")
          Core.<*> (x Core..:? "TableType")

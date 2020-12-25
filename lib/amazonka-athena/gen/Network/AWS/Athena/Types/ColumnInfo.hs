{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ColumnInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ColumnInfo
  ( ColumnInfo (..),

    -- * Smart constructor
    mkColumnInfo,

    -- * Lenses
    ciName,
    ciType,
    ciCaseSensitive,
    ciCatalogName,
    ciLabel,
    ciNullable,
    ciPrecision,
    ciScale,
    ciSchemaName,
    ciTableName,
  )
where

import qualified Network.AWS.Athena.Types.ColumnNullable as Types
import qualified Network.AWS.Athena.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the columns in a query execution result.
--
-- /See:/ 'mkColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { -- | The name of the column.
    name :: Types.String,
    -- | The data type of the column.
    type' :: Types.String,
    -- | Indicates whether values in the column are case-sensitive.
    caseSensitive :: Core.Maybe Core.Bool,
    -- | The catalog to which the query results belong.
    catalogName :: Core.Maybe Types.String,
    -- | A column label.
    label :: Core.Maybe Types.String,
    -- | Indicates the column's nullable status.
    nullable :: Core.Maybe Types.ColumnNullable,
    -- | For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
    precision :: Core.Maybe Core.Int,
    -- | For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
    scale :: Core.Maybe Core.Int,
    -- | The schema name (database name) to which the query results belong.
    schemaName :: Core.Maybe Types.String,
    -- | The table name for the query results.
    tableName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ColumnInfo' value with any optional fields omitted.
mkColumnInfo ::
  -- | 'name'
  Types.String ->
  -- | 'type\''
  Types.String ->
  ColumnInfo
mkColumnInfo name type' =
  ColumnInfo'
    { name,
      type',
      caseSensitive = Core.Nothing,
      catalogName = Core.Nothing,
      label = Core.Nothing,
      nullable = Core.Nothing,
      precision = Core.Nothing,
      scale = Core.Nothing,
      schemaName = Core.Nothing,
      tableName = Core.Nothing
    }

-- | The name of the column.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' ColumnInfo Types.String
ciName = Lens.field @"name"
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data type of the column.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciType :: Lens.Lens' ColumnInfo Types.String
ciType = Lens.field @"type'"
{-# DEPRECATED ciType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Indicates whether values in the column are case-sensitive.
--
-- /Note:/ Consider using 'caseSensitive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCaseSensitive :: Lens.Lens' ColumnInfo (Core.Maybe Core.Bool)
ciCaseSensitive = Lens.field @"caseSensitive"
{-# DEPRECATED ciCaseSensitive "Use generic-lens or generic-optics with 'caseSensitive' instead." #-}

-- | The catalog to which the query results belong.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCatalogName :: Lens.Lens' ColumnInfo (Core.Maybe Types.String)
ciCatalogName = Lens.field @"catalogName"
{-# DEPRECATED ciCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | A column label.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLabel :: Lens.Lens' ColumnInfo (Core.Maybe Types.String)
ciLabel = Lens.field @"label"
{-# DEPRECATED ciLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | Indicates the column's nullable status.
--
-- /Note:/ Consider using 'nullable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciNullable :: Lens.Lens' ColumnInfo (Core.Maybe Types.ColumnNullable)
ciNullable = Lens.field @"nullable"
{-# DEPRECATED ciNullable "Use generic-lens or generic-optics with 'nullable' instead." #-}

-- | For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPrecision :: Lens.Lens' ColumnInfo (Core.Maybe Core.Int)
ciPrecision = Lens.field @"precision"
{-# DEPRECATED ciPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciScale :: Lens.Lens' ColumnInfo (Core.Maybe Core.Int)
ciScale = Lens.field @"scale"
{-# DEPRECATED ciScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | The schema name (database name) to which the query results belong.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSchemaName :: Lens.Lens' ColumnInfo (Core.Maybe Types.String)
ciSchemaName = Lens.field @"schemaName"
{-# DEPRECATED ciSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The table name for the query results.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTableName :: Lens.Lens' ColumnInfo (Core.Maybe Types.String)
ciTableName = Lens.field @"tableName"
{-# DEPRECATED ciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON ColumnInfo where
  parseJSON =
    Core.withObject "ColumnInfo" Core.$
      \x ->
        ColumnInfo'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..:? "CaseSensitive")
          Core.<*> (x Core..:? "CatalogName")
          Core.<*> (x Core..:? "Label")
          Core.<*> (x Core..:? "Nullable")
          Core.<*> (x Core..:? "Precision")
          Core.<*> (x Core..:? "Scale")
          Core.<*> (x Core..:? "SchemaName")
          Core.<*> (x Core..:? "TableName")

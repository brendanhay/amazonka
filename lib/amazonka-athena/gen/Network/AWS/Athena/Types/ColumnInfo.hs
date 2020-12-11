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
    ciScale,
    ciPrecision,
    ciSchemaName,
    ciCatalogName,
    ciCaseSensitive,
    ciLabel,
    ciTableName,
    ciNullable,
    ciName,
    ciType,
  )
where

import Network.AWS.Athena.Types.ColumnNullable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the columns in a query execution result.
--
-- /See:/ 'mkColumnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { scale :: Lude.Maybe Lude.Int,
    precision :: Lude.Maybe Lude.Int,
    schemaName :: Lude.Maybe Lude.Text,
    catalogName :: Lude.Maybe Lude.Text,
    caseSensitive :: Lude.Maybe Lude.Bool,
    label :: Lude.Maybe Lude.Text,
    tableName :: Lude.Maybe Lude.Text,
    nullable :: Lude.Maybe ColumnNullable,
    name :: Lude.Text,
    type' :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColumnInfo' with the minimum fields required to make a request.
--
-- * 'caseSensitive' - Indicates whether values in the column are case-sensitive.
-- * 'catalogName' - The catalog to which the query results belong.
-- * 'label' - A column label.
-- * 'name' - The name of the column.
-- * 'nullable' - Indicates the column's nullable status.
-- * 'precision' - For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
-- * 'scale' - For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
-- * 'schemaName' - The schema name (database name) to which the query results belong.
-- * 'tableName' - The table name for the query results.
-- * 'type'' - The data type of the column.
mkColumnInfo ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  ColumnInfo
mkColumnInfo pName_ pType_ =
  ColumnInfo'
    { scale = Lude.Nothing,
      precision = Lude.Nothing,
      schemaName = Lude.Nothing,
      catalogName = Lude.Nothing,
      caseSensitive = Lude.Nothing,
      label = Lude.Nothing,
      tableName = Lude.Nothing,
      nullable = Lude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciScale :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Int)
ciScale = Lens.lens (scale :: ColumnInfo -> Lude.Maybe Lude.Int) (\s a -> s {scale = a} :: ColumnInfo)
{-# DEPRECATED ciScale "Use generic-lens or generic-optics with 'scale' instead." #-}

-- | For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPrecision :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Int)
ciPrecision = Lens.lens (precision :: ColumnInfo -> Lude.Maybe Lude.Int) (\s a -> s {precision = a} :: ColumnInfo)
{-# DEPRECATED ciPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | The schema name (database name) to which the query results belong.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSchemaName :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Text)
ciSchemaName = Lens.lens (schemaName :: ColumnInfo -> Lude.Maybe Lude.Text) (\s a -> s {schemaName = a} :: ColumnInfo)
{-# DEPRECATED ciSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The catalog to which the query results belong.
--
-- /Note:/ Consider using 'catalogName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCatalogName :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Text)
ciCatalogName = Lens.lens (catalogName :: ColumnInfo -> Lude.Maybe Lude.Text) (\s a -> s {catalogName = a} :: ColumnInfo)
{-# DEPRECATED ciCatalogName "Use generic-lens or generic-optics with 'catalogName' instead." #-}

-- | Indicates whether values in the column are case-sensitive.
--
-- /Note:/ Consider using 'caseSensitive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCaseSensitive :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Bool)
ciCaseSensitive = Lens.lens (caseSensitive :: ColumnInfo -> Lude.Maybe Lude.Bool) (\s a -> s {caseSensitive = a} :: ColumnInfo)
{-# DEPRECATED ciCaseSensitive "Use generic-lens or generic-optics with 'caseSensitive' instead." #-}

-- | A column label.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLabel :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Text)
ciLabel = Lens.lens (label :: ColumnInfo -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: ColumnInfo)
{-# DEPRECATED ciLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The table name for the query results.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTableName :: Lens.Lens' ColumnInfo (Lude.Maybe Lude.Text)
ciTableName = Lens.lens (tableName :: ColumnInfo -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: ColumnInfo)
{-# DEPRECATED ciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Indicates the column's nullable status.
--
-- /Note:/ Consider using 'nullable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciNullable :: Lens.Lens' ColumnInfo (Lude.Maybe ColumnNullable)
ciNullable = Lens.lens (nullable :: ColumnInfo -> Lude.Maybe ColumnNullable) (\s a -> s {nullable = a} :: ColumnInfo)
{-# DEPRECATED ciNullable "Use generic-lens or generic-optics with 'nullable' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' ColumnInfo Lude.Text
ciName = Lens.lens (name :: ColumnInfo -> Lude.Text) (\s a -> s {name = a} :: ColumnInfo)
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data type of the column.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciType :: Lens.Lens' ColumnInfo Lude.Text
ciType = Lens.lens (type' :: ColumnInfo -> Lude.Text) (\s a -> s {type' = a} :: ColumnInfo)
{-# DEPRECATED ciType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ColumnInfo where
  parseJSON =
    Lude.withObject
      "ColumnInfo"
      ( \x ->
          ColumnInfo'
            Lude.<$> (x Lude..:? "Scale")
            Lude.<*> (x Lude..:? "Precision")
            Lude.<*> (x Lude..:? "SchemaName")
            Lude.<*> (x Lude..:? "CatalogName")
            Lude.<*> (x Lude..:? "CaseSensitive")
            Lude.<*> (x Lude..:? "Label")
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "Nullable")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Type")
      )

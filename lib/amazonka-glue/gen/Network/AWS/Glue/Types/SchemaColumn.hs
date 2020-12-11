-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaColumn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaColumn
  ( SchemaColumn (..),

    -- * Smart constructor
    mkSchemaColumn,

    -- * Lenses
    sName,
    sDataType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair representing a column and data type that this transform can run against. The @Schema@ parameter of the @MLTransform@ may contain up to 100 of these structures.
--
-- /See:/ 'mkSchemaColumn' smart constructor.
data SchemaColumn = SchemaColumn'
  { name :: Lude.Maybe Lude.Text,
    dataType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaColumn' with the minimum fields required to make a request.
--
-- * 'dataType' - The type of data in the column.
-- * 'name' - The name of the column.
mkSchemaColumn ::
  SchemaColumn
mkSchemaColumn =
  SchemaColumn' {name = Lude.Nothing, dataType = Lude.Nothing}

-- | The name of the column.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' SchemaColumn (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: SchemaColumn -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SchemaColumn)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of data in the column.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataType :: Lens.Lens' SchemaColumn (Lude.Maybe Lude.Text)
sDataType = Lens.lens (dataType :: SchemaColumn -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: SchemaColumn)
{-# DEPRECATED sDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

instance Lude.FromJSON SchemaColumn where
  parseJSON =
    Lude.withObject
      "SchemaColumn"
      ( \x ->
          SchemaColumn'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "DataType")
      )

instance Lude.ToJSON SchemaColumn where
  toJSON SchemaColumn' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("DataType" Lude..=) Lude.<$> dataType
          ]
      )

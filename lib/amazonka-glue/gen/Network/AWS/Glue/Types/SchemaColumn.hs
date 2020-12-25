{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    scDataType,
    scName,
  )
where

import qualified Network.AWS.Glue.Types.DataType as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key-value pair representing a column and data type that this transform can run against. The @Schema@ parameter of the @MLTransform@ may contain up to 100 of these structures.
--
-- /See:/ 'mkSchemaColumn' smart constructor.
data SchemaColumn = SchemaColumn'
  { -- | The type of data in the column.
    dataType :: Core.Maybe Types.DataType,
    -- | The name of the column.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaColumn' value with any optional fields omitted.
mkSchemaColumn ::
  SchemaColumn
mkSchemaColumn =
  SchemaColumn' {dataType = Core.Nothing, name = Core.Nothing}

-- | The type of data in the column.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDataType :: Lens.Lens' SchemaColumn (Core.Maybe Types.DataType)
scDataType = Lens.field @"dataType"
{-# DEPRECATED scDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | The name of the column.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' SchemaColumn (Core.Maybe Types.Name)
scName = Lens.field @"name"
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SchemaColumn where
  toJSON SchemaColumn {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataType" Core..=) Core.<$> dataType,
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.FromJSON SchemaColumn where
  parseJSON =
    Core.withObject "SchemaColumn" Core.$
      \x ->
        SchemaColumn'
          Core.<$> (x Core..:? "DataType") Core.<*> (x Core..:? "Name")

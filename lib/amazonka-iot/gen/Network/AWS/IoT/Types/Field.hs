{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Field
  ( Field (..),

    -- * Smart constructor
    mkField,

    -- * Lenses
    fName,
    fType,
  )
where

import qualified Network.AWS.IoT.Types.FieldType as Types
import qualified Network.AWS.IoT.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the name and data type at a field.
--
-- /See:/ 'mkField' smart constructor.
data Field = Field'
  { -- | The name of the field.
    name :: Core.Maybe Types.Name,
    -- | The datatype of the field.
    type' :: Core.Maybe Types.FieldType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Field' value with any optional fields omitted.
mkField ::
  Field
mkField = Field' {name = Core.Nothing, type' = Core.Nothing}

-- | The name of the field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Field (Core.Maybe Types.Name)
fName = Lens.field @"name"
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The datatype of the field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Field (Core.Maybe Types.FieldType)
fType = Lens.field @"type'"
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Field where
  toJSON Field {..} =
    Core.object
      ( Core.catMaybes
          [("name" Core..=) Core.<$> name, ("type" Core..=) Core.<$> type']
      )

instance Core.FromJSON Field where
  parseJSON =
    Core.withObject "Field" Core.$
      \x ->
        Field' Core.<$> (x Core..:? "name") Core.<*> (x Core..:? "type")

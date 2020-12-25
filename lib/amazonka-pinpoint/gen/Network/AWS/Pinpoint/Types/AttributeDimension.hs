{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributeDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributeDimension
  ( AttributeDimension (..),

    -- * Smart constructor
    mkAttributeDimension,

    -- * Lenses
    adValues,
    adAttributeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.AttributeType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies attribute-based criteria for including or excluding endpoints from a segment.
--
-- /See:/ 'mkAttributeDimension' smart constructor.
data AttributeDimension = AttributeDimension'
  { -- | The criteria values to use for the segment dimension. Depending on the value of the AttributeType property, endpoints are included or excluded from the segment if their attribute values match the criteria values.
    values :: [Core.Text],
    -- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
    attributeType :: Core.Maybe Types.AttributeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeDimension' value with any optional fields omitted.
mkAttributeDimension ::
  AttributeDimension
mkAttributeDimension =
  AttributeDimension'
    { values = Core.mempty,
      attributeType = Core.Nothing
    }

-- | The criteria values to use for the segment dimension. Depending on the value of the AttributeType property, endpoints are included or excluded from the segment if their attribute values match the criteria values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adValues :: Lens.Lens' AttributeDimension [Core.Text]
adValues = Lens.field @"values"
{-# DEPRECATED adValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttributeType :: Lens.Lens' AttributeDimension (Core.Maybe Types.AttributeType)
adAttributeType = Lens.field @"attributeType"
{-# DEPRECATED adAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

instance Core.FromJSON AttributeDimension where
  toJSON AttributeDimension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Values" Core..= values),
            ("AttributeType" Core..=) Core.<$> attributeType
          ]
      )

instance Core.FromJSON AttributeDimension where
  parseJSON =
    Core.withObject "AttributeDimension" Core.$
      \x ->
        AttributeDimension'
          Core.<$> (x Core..:? "Values" Core..!= Core.mempty)
          Core.<*> (x Core..:? "AttributeType")

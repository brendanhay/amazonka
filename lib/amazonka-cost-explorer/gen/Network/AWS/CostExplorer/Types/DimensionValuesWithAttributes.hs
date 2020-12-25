{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
  ( DimensionValuesWithAttributes (..),

    -- * Smart constructor
    mkDimensionValuesWithAttributes,

    -- * Lenses
    dvwaAttributes,
    dvwaValue,
  )
where

import qualified Network.AWS.CostExplorer.Types.AttributeType as Types
import qualified Network.AWS.CostExplorer.Types.AttributeValue as Types
import qualified Network.AWS.CostExplorer.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata of a specific type that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'mkDimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { -- | The attribute that applies to a specific @Dimension@ .
    attributes :: Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue),
    -- | The value of a dimension with a specific attribute.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DimensionValuesWithAttributes' value with any optional fields omitted.
mkDimensionValuesWithAttributes ::
  DimensionValuesWithAttributes
mkDimensionValuesWithAttributes =
  DimensionValuesWithAttributes'
    { attributes = Core.Nothing,
      value = Core.Nothing
    }

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvwaAttributes :: Lens.Lens' DimensionValuesWithAttributes (Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue))
dvwaAttributes = Lens.field @"attributes"
{-# DEPRECATED dvwaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The value of a dimension with a specific attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvwaValue :: Lens.Lens' DimensionValuesWithAttributes (Core.Maybe Types.Value)
dvwaValue = Lens.field @"value"
{-# DEPRECATED dvwaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON DimensionValuesWithAttributes where
  parseJSON =
    Core.withObject "DimensionValuesWithAttributes" Core.$
      \x ->
        DimensionValuesWithAttributes'
          Core.<$> (x Core..:? "Attributes") Core.<*> (x Core..:? "Value")

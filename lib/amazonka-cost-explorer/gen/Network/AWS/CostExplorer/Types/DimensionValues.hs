{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValues
  ( DimensionValues (..),

    -- * Smart constructor
    mkDimensionValues,

    -- * Lenses
    dvKey,
    dvMatchOptions,
    dvValues,
  )
where

import qualified Network.AWS.CostExplorer.Types.Dimension as Types
import qualified Network.AWS.CostExplorer.Types.MatchOption as Types
import qualified Network.AWS.CostExplorer.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The metadata that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /See:/ 'mkDimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { -- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
    key :: Core.Maybe Types.Dimension,
    -- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
    matchOptions :: Core.Maybe [Types.MatchOption],
    -- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
    values :: Core.Maybe [Types.Value]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DimensionValues' value with any optional fields omitted.
mkDimensionValues ::
  DimensionValues
mkDimensionValues =
  DimensionValues'
    { key = Core.Nothing,
      matchOptions = Core.Nothing,
      values = Core.Nothing
    }

-- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvKey :: Lens.Lens' DimensionValues (Core.Maybe Types.Dimension)
dvKey = Lens.field @"key"
{-# DEPRECATED dvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvMatchOptions :: Lens.Lens' DimensionValues (Core.Maybe [Types.MatchOption])
dvMatchOptions = Lens.field @"matchOptions"
{-# DEPRECATED dvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

-- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvValues :: Lens.Lens' DimensionValues (Core.Maybe [Types.Value])
dvValues = Lens.field @"values"
{-# DEPRECATED dvValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON DimensionValues where
  toJSON DimensionValues {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("MatchOptions" Core..=) Core.<$> matchOptions,
            ("Values" Core..=) Core.<$> values
          ]
      )

instance Core.FromJSON DimensionValues where
  parseJSON =
    Core.withObject "DimensionValues" Core.$
      \x ->
        DimensionValues'
          Core.<$> (x Core..:? "Key")
          Core.<*> (x Core..:? "MatchOptions")
          Core.<*> (x Core..:? "Values")

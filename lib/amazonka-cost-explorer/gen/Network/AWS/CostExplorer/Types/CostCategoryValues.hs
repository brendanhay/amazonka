{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryValues
  ( CostCategoryValues (..),

    -- * Smart constructor
    mkCostCategoryValues,

    -- * Lenses
    ccvKey,
    ccvMatchOptions,
    ccvValues,
  )
where

import qualified Network.AWS.CostExplorer.Types.Key as Types
import qualified Network.AWS.CostExplorer.Types.MatchOption as Types
import qualified Network.AWS.CostExplorer.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Cost Categories values used for filtering the costs.
--
-- /See:/ 'mkCostCategoryValues' smart constructor.
data CostCategoryValues = CostCategoryValues'
  { key :: Core.Maybe Types.Key,
    -- | The match options that you can use to filter your results. MatchOptions is only applicable for only applicable for actions related to cost category. The default values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@ .
    matchOptions :: Core.Maybe [Types.MatchOption],
    -- | The specific value of the Cost Category.
    values :: Core.Maybe [Types.Value]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostCategoryValues' value with any optional fields omitted.
mkCostCategoryValues ::
  CostCategoryValues
mkCostCategoryValues =
  CostCategoryValues'
    { key = Core.Nothing,
      matchOptions = Core.Nothing,
      values = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvKey :: Lens.Lens' CostCategoryValues (Core.Maybe Types.Key)
ccvKey = Lens.field @"key"
{-# DEPRECATED ccvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. MatchOptions is only applicable for only applicable for actions related to cost category. The default values for @MatchOptions@ is @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvMatchOptions :: Lens.Lens' CostCategoryValues (Core.Maybe [Types.MatchOption])
ccvMatchOptions = Lens.field @"matchOptions"
{-# DEPRECATED ccvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

-- | The specific value of the Cost Category.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccvValues :: Lens.Lens' CostCategoryValues (Core.Maybe [Types.Value])
ccvValues = Lens.field @"values"
{-# DEPRECATED ccvValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON CostCategoryValues where
  toJSON CostCategoryValues {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("MatchOptions" Core..=) Core.<$> matchOptions,
            ("Values" Core..=) Core.<$> values
          ]
      )

instance Core.FromJSON CostCategoryValues where
  parseJSON =
    Core.withObject "CostCategoryValues" Core.$
      \x ->
        CostCategoryValues'
          Core.<$> (x Core..:? "Key")
          Core.<*> (x Core..:? "MatchOptions")
          Core.<*> (x Core..:? "Values")

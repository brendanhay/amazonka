{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
  ( ListTagOptionsFilters (..),

    -- * Smart constructor
    mkListTagOptionsFilters,

    -- * Lenses
    ltofActive,
    ltofKey,
    ltofValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.TagOptionKey as Types
import qualified Network.AWS.ServiceCatalog.Types.TagOptionValue as Types

-- | Filters to use when listing TagOptions.
--
-- /See:/ 'mkListTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { -- | The active state.
    active :: Core.Maybe Core.Bool,
    -- | The TagOption key.
    key :: Core.Maybe Types.TagOptionKey,
    -- | The TagOption value.
    value :: Core.Maybe Types.TagOptionValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagOptionsFilters' value with any optional fields omitted.
mkListTagOptionsFilters ::
  ListTagOptionsFilters
mkListTagOptionsFilters =
  ListTagOptionsFilters'
    { active = Core.Nothing,
      key = Core.Nothing,
      value = Core.Nothing
    }

-- | The active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofActive :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Core.Bool)
ltofActive = Lens.field @"active"
{-# DEPRECATED ltofActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofKey :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Types.TagOptionKey)
ltofKey = Lens.field @"key"
{-# DEPRECATED ltofKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltofValue :: Lens.Lens' ListTagOptionsFilters (Core.Maybe Types.TagOptionValue)
ltofValue = Lens.field @"value"
{-# DEPRECATED ltofValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ListTagOptionsFilters where
  toJSON ListTagOptionsFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("Active" Core..=) Core.<$> active,
            ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TagValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TagValues
  ( TagValues (..),

    -- * Smart constructor
    mkTagValues,

    -- * Lenses
    tvKey,
    tvMatchOptions,
    tvValues,
  )
where

import qualified Network.AWS.CostExplorer.Types.Key as Types
import qualified Network.AWS.CostExplorer.Types.MatchOption as Types
import qualified Network.AWS.CostExplorer.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values that are available for a tag.
--
-- /See:/ 'mkTagValues' smart constructor.
data TagValues = TagValues'
  { -- | The key for the tag.
    key :: Core.Maybe Types.Key,
    -- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
    matchOptions :: Core.Maybe [Types.MatchOption],
    -- | The specific value of the tag.
    values :: Core.Maybe [Types.Value]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagValues' value with any optional fields omitted.
mkTagValues ::
  TagValues
mkTagValues =
  TagValues'
    { key = Core.Nothing,
      matchOptions = Core.Nothing,
      values = Core.Nothing
    }

-- | The key for the tag.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvKey :: Lens.Lens' TagValues (Core.Maybe Types.Key)
tvKey = Lens.field @"key"
{-# DEPRECATED tvKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
--
-- /Note:/ Consider using 'matchOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvMatchOptions :: Lens.Lens' TagValues (Core.Maybe [Types.MatchOption])
tvMatchOptions = Lens.field @"matchOptions"
{-# DEPRECATED tvMatchOptions "Use generic-lens or generic-optics with 'matchOptions' instead." #-}

-- | The specific value of the tag.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tvValues :: Lens.Lens' TagValues (Core.Maybe [Types.Value])
tvValues = Lens.field @"values"
{-# DEPRECATED tvValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON TagValues where
  toJSON TagValues {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("MatchOptions" Core..=) Core.<$> matchOptions,
            ("Values" Core..=) Core.<$> values
          ]
      )

instance Core.FromJSON TagValues where
  parseJSON =
    Core.withObject "TagValues" Core.$
      \x ->
        TagValues'
          Core.<$> (x Core..:? "Key")
          Core.<*> (x Core..:? "MatchOptions")
          Core.<*> (x Core..:? "Values")

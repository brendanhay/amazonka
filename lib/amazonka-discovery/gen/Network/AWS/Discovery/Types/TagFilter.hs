{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.TagFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.TagFilter
  ( TagFilter (..),

    -- * Smart constructor
    mkTagFilter,

    -- * Lenses
    tfName,
    tfValues,
  )
where

import qualified Network.AWS.Discovery.Types.FilterName as Types
import qualified Network.AWS.Discovery.Types.FilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The tag filter. Valid names are: @tagKey@ , @tagValue@ , @configurationId@ .
--
-- /See:/ 'mkTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | A name of the tag filter.
    name :: Types.FilterName,
    -- | Values for the tag filter.
    values :: [Types.FilterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagFilter' value with any optional fields omitted.
mkTagFilter ::
  -- | 'name'
  Types.FilterName ->
  TagFilter
mkTagFilter name = TagFilter' {name, values = Core.mempty}

-- | A name of the tag filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfName :: Lens.Lens' TagFilter Types.FilterName
tfName = Lens.field @"name"
{-# DEPRECATED tfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Values for the tag filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tfValues :: Lens.Lens' TagFilter [Types.FilterValue]
tfValues = Lens.field @"values"
{-# DEPRECATED tfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON TagFilter where
  toJSON TagFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("values" Core..= values)
          ]
      )

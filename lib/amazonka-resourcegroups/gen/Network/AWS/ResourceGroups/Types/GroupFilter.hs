{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilter
  ( GroupFilter (..),

    -- * Smart constructor
    mkGroupFilter,

    -- * Lenses
    gfName,
    gfValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.GroupFilterName as Types
import qualified Network.AWS.ResourceGroups.Types.GroupFilterValue as Types

-- | A filter collection that you can use to restrict the results from a @List@ operation to only those you want to include.
--
-- /See:/ 'mkGroupFilter' smart constructor.
data GroupFilter = GroupFilter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: Types.GroupFilterName,
    -- | One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
    values :: Core.NonEmpty Types.GroupFilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupFilter' value with any optional fields omitted.
mkGroupFilter ::
  -- | 'name'
  Types.GroupFilterName ->
  -- | 'values'
  Core.NonEmpty Types.GroupFilterValue ->
  GroupFilter
mkGroupFilter name values = GroupFilter' {name, values}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfName :: Lens.Lens' GroupFilter Types.GroupFilterName
gfName = Lens.field @"name"
{-# DEPRECATED gfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more filter values. Allowed filter values vary by group filter name, and are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfValues :: Lens.Lens' GroupFilter (Core.NonEmpty Types.GroupFilterValue)
gfValues = Lens.field @"values"
{-# DEPRECATED gfValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON GroupFilter where
  toJSON GroupFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Values" Core..= values)
          ]
      )

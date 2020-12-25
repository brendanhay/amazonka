{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilter
  ( DescribeActivationsFilter (..),

    -- * Smart constructor
    mkDescribeActivationsFilter,

    -- * Lenses
    dafFilterKey,
    dafFilterValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DescribeActivationsFilterKeys as Types
import qualified Network.AWS.SSM.Types.String as Types

-- | Filter for the DescribeActivation API.
--
-- /See:/ 'mkDescribeActivationsFilter' smart constructor.
data DescribeActivationsFilter = DescribeActivationsFilter'
  { -- | The name of the filter.
    filterKey :: Core.Maybe Types.DescribeActivationsFilterKeys,
    -- | The filter values.
    filterValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeActivationsFilter' value with any optional fields omitted.
mkDescribeActivationsFilter ::
  DescribeActivationsFilter
mkDescribeActivationsFilter =
  DescribeActivationsFilter'
    { filterKey = Core.Nothing,
      filterValues = Core.Nothing
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'filterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFilterKey :: Lens.Lens' DescribeActivationsFilter (Core.Maybe Types.DescribeActivationsFilterKeys)
dafFilterKey = Lens.field @"filterKey"
{-# DEPRECATED dafFilterKey "Use generic-lens or generic-optics with 'filterKey' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'filterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dafFilterValues :: Lens.Lens' DescribeActivationsFilter (Core.Maybe [Types.String])
dafFilterValues = Lens.field @"filterValues"
{-# DEPRECATED dafFilterValues "Use generic-lens or generic-optics with 'filterValues' instead." #-}

instance Core.FromJSON DescribeActivationsFilter where
  toJSON DescribeActivationsFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("FilterKey" Core..=) Core.<$> filterKey,
            ("FilterValues" Core..=) Core.<$> filterValues
          ]
      )

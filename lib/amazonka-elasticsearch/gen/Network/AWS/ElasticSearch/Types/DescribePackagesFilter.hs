{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DescribePackagesFilter
  ( DescribePackagesFilter (..),

    -- * Smart constructor
    mkDescribePackagesFilter,

    -- * Lenses
    dpfName,
    dpfValue,
  )
where

import qualified Network.AWS.ElasticSearch.Types.DescribePackagesFilterName as Types
import qualified Network.AWS.ElasticSearch.Types.DescribePackagesFilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filter to apply in @DescribePackage@ response.
--
-- /See:/ 'mkDescribePackagesFilter' smart constructor.
data DescribePackagesFilter = DescribePackagesFilter'
  { -- | Any field from @PackageDetails@ .
    name :: Core.Maybe Types.DescribePackagesFilterName,
    -- | A list of values for the specified field.
    value :: Core.Maybe [Types.DescribePackagesFilterValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePackagesFilter' value with any optional fields omitted.
mkDescribePackagesFilter ::
  DescribePackagesFilter
mkDescribePackagesFilter =
  DescribePackagesFilter'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | Any field from @PackageDetails@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfName :: Lens.Lens' DescribePackagesFilter (Core.Maybe Types.DescribePackagesFilterName)
dpfName = Lens.field @"name"
{-# DEPRECATED dpfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of values for the specified field.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfValue :: Lens.Lens' DescribePackagesFilter (Core.Maybe [Types.DescribePackagesFilterValue])
dpfValue = Lens.field @"value"
{-# DEPRECATED dpfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON DescribePackagesFilter where
  toJSON DescribePackagesFilter {..} =
    Core.object
      ( Core.catMaybes
          [("Name" Core..=) Core.<$> name, ("Value" Core..=) Core.<$> value]
      )

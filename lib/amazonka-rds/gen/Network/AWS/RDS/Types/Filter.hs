{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as IDs. The filters supported by a describe operation are documented with the describe operation.
--
-- The following actions can be filtered:
--
--     * @DescribeDBClusterBacktracks@
--
--
--     * @DescribeDBClusterEndpoints@
--
--
--     * @DescribeDBClusters@
--
--
--     * @DescribeDBInstances@
--
--
--     * @DescribePendingMaintenanceActions@
--
--
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter. Filter names are case-sensitive.
    name :: Types.String,
    -- | One or more filter values. Filter values are case-sensitive.
    values :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  -- | 'name'
  Types.String ->
  Filter
mkFilter name = Filter' {name, values = Core.mempty}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Types.String
fName = Lens.field @"name"
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | One or more filter values. Filter values are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Types.String]
fValues = Lens.field @"values"
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

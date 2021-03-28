{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Filter
  ( Filter (..)
  -- * Smart constructor
  , mkFilter
  -- * Lenses
  , fName
  , fValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as tags, attributes, or IDs. The filters supported by a describe operation are documented with the describe operation. For example:
--
--
--     * 'DescribeAvailabilityZones' 
--
--
--     * 'DescribeImages' 
--
--
--     * 'DescribeInstances' 
--
--
--     * 'DescribeKeyPairs' 
--
--
--     * 'DescribeSecurityGroups' 
--
--
--     * 'DescribeSnapshots' 
--
--
--     * 'DescribeSubnets' 
--
--
--     * 'DescribeTags' 
--
--
--     * 'DescribeVolumes' 
--
--
--     * 'DescribeVpcs' 
--
--
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { name :: Core.Text
    -- ^ The name of the filter. Filter names are case-sensitive.
  , values :: Core.Maybe [Core.Text]
    -- ^ The filter values. Filter values are case-sensitive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter
    :: Core.Text -- ^ 'name'
    -> Filter
mkFilter name = Filter'{name, values = Core.Nothing}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Core.Text
fName = Lens.field @"name"
{-# INLINEABLE fName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The filter values. Filter values are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Core.Maybe [Core.Text])
fValues = Lens.field @"values"
{-# INLINEABLE fValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery Filter where
        toQuery Filter{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Value") values

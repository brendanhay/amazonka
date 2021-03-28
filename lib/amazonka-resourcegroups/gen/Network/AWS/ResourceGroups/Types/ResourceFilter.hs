{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.ResourceFilter
  ( ResourceFilter (..)
  -- * Smart constructor
  , mkResourceFilter
  -- * Lenses
  , rfName
  , rfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.ResourceFilterName as Types
import qualified Network.AWS.ResourceGroups.Types.ResourceFilterValue as Types

-- | A filter name and value pair that is used to obtain more specific results from a list of resources.
--
-- /See:/ 'mkResourceFilter' smart constructor.
data ResourceFilter = ResourceFilter'
  { name :: Types.ResourceFilterName
    -- ^ The name of the filter. Filter names are case-sensitive.
  , values :: Core.NonEmpty Types.ResourceFilterValue
    -- ^ One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceFilter' value with any optional fields omitted.
mkResourceFilter
    :: Types.ResourceFilterName -- ^ 'name'
    -> Core.NonEmpty Types.ResourceFilterValue -- ^ 'values'
    -> ResourceFilter
mkResourceFilter name values = ResourceFilter'{name, values}

-- | The name of the filter. Filter names are case-sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfName :: Lens.Lens' ResourceFilter Types.ResourceFilterName
rfName = Lens.field @"name"
{-# INLINEABLE rfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | One or more filter values. Allowed filter values vary by resource filter name, and are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfValues :: Lens.Lens' ResourceFilter (Core.NonEmpty Types.ResourceFilterValue)
rfValues = Lens.field @"values"
{-# INLINEABLE rfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON ResourceFilter where
        toJSON ResourceFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Values" Core..= values)])

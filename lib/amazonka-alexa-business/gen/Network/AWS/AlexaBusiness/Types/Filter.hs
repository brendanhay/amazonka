{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Filter
  ( Filter (..)
  -- * Smart constructor
  , mkFilter
  -- * Lenses
  , fKey
  , fValues
  ) where

import qualified Network.AWS.AlexaBusiness.Types.FilterKey as Types
import qualified Network.AWS.AlexaBusiness.Types.FilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { key :: Types.FilterKey
    -- ^ The key of a filter.
  , values :: [Types.FilterValue]
    -- ^ The values of a filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter
    :: Types.FilterKey -- ^ 'key'
    -> Filter
mkFilter key = Filter'{key, values = Core.mempty}

-- | The key of a filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Filter Types.FilterKey
fKey = Lens.field @"key"
{-# INLINEABLE fKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The values of a filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Types.FilterValue]
fValues = Lens.field @"values"
{-# INLINEABLE fValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON Filter where
        toJSON Filter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values)])

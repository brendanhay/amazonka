{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ScheduledActionFilter
  ( ScheduledActionFilter (..)
  -- * Smart constructor
  , mkScheduledActionFilter
  -- * Lenses
  , safName
  , safValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ScheduledActionFilterName as Types

-- | A set of elements to filter the returned scheduled actions. 
--
-- /See:/ 'mkScheduledActionFilter' smart constructor.
data ScheduledActionFilter = ScheduledActionFilter'
  { name :: Types.ScheduledActionFilterName
    -- ^ The type of element to filter. 
  , values :: [Core.Text]
    -- ^ List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledActionFilter' value with any optional fields omitted.
mkScheduledActionFilter
    :: Types.ScheduledActionFilterName -- ^ 'name'
    -> ScheduledActionFilter
mkScheduledActionFilter name
  = ScheduledActionFilter'{name, values = Core.mempty}

-- | The type of element to filter. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safName :: Lens.Lens' ScheduledActionFilter Types.ScheduledActionFilterName
safName = Lens.field @"name"
{-# INLINEABLE safName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | List of values. Compare if the value (of type defined by @Name@ ) equals an item in the list of scheduled actions. 
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
safValues :: Lens.Lens' ScheduledActionFilter [Core.Text]
safValues = Lens.field @"values"
{-# INLINEABLE safValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery ScheduledActionFilter where
        toQuery ScheduledActionFilter{..}
          = Core.toQueryPair "Name" name Core.<>
              Core.toQueryPair "Values" (Core.toQueryList "item" values)

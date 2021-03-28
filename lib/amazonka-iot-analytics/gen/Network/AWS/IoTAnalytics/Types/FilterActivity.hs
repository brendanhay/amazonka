{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.FilterActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.FilterActivity
  ( FilterActivity (..)
  -- * Smart constructor
  , mkFilterActivity
  -- * Lenses
  , faName
  , faFilter
  , faNext
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.FilterExpression as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that filters a message based on its attributes.
--
-- /See:/ 'mkFilterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { name :: Types.ActivityName
    -- ^ The name of the filter activity.
  , filter :: Types.FilterExpression
    -- ^ An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity. 
  , next :: Core.Maybe Types.ActivityName
    -- ^ The next activity in the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FilterActivity' value with any optional fields omitted.
mkFilterActivity
    :: Types.ActivityName -- ^ 'name'
    -> Types.FilterExpression -- ^ 'filter'
    -> FilterActivity
mkFilterActivity name filter
  = FilterActivity'{name, filter, next = Core.Nothing}

-- | The name of the filter activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FilterActivity Types.ActivityName
faName = Lens.field @"name"
{-# INLINEABLE faName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity. 
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFilter :: Lens.Lens' FilterActivity Types.FilterExpression
faFilter = Lens.field @"filter"
{-# INLINEABLE faFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faNext :: Lens.Lens' FilterActivity (Core.Maybe Types.ActivityName)
faNext = Lens.field @"next"
{-# INLINEABLE faNext #-}
{-# DEPRECATED next "Use generic-lens or generic-optics with 'next' instead"  #-}

instance Core.FromJSON FilterActivity where
        toJSON FilterActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("filter" Core..= filter),
                  ("next" Core..=) Core.<$> next])

instance Core.FromJSON FilterActivity where
        parseJSON
          = Core.withObject "FilterActivity" Core.$
              \ x ->
                FilterActivity' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "filter" Core.<*>
                    x Core..:? "next"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.EventCategoriesMap
  ( EventCategoriesMap (..)
  -- * Smart constructor
  , mkEventCategoriesMap
  -- * Lenses
  , ecmEventCategories
  , ecmSourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the results of a successful invocation of the @DescribeEventCategories@ operation.
--
-- /See:/ 'mkEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { eventCategories :: Core.Maybe [Core.Text]
    -- ^ The event categories for the specified source type
  , sourceType :: Core.Maybe Core.Text
    -- ^ The source type that the returned categories belong to
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventCategoriesMap' value with any optional fields omitted.
mkEventCategoriesMap
    :: EventCategoriesMap
mkEventCategoriesMap
  = EventCategoriesMap'{eventCategories = Core.Nothing,
                        sourceType = Core.Nothing}

-- | The event categories for the specified source type
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmEventCategories :: Lens.Lens' EventCategoriesMap (Core.Maybe [Core.Text])
ecmEventCategories = Lens.field @"eventCategories"
{-# INLINEABLE ecmEventCategories #-}
{-# DEPRECATED eventCategories "Use generic-lens or generic-optics with 'eventCategories' instead"  #-}

-- | The source type that the returned categories belong to
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmSourceType :: Lens.Lens' EventCategoriesMap (Core.Maybe Core.Text)
ecmSourceType = Lens.field @"sourceType"
{-# INLINEABLE ecmSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' Core.<$>
              (x Core..@? "EventCategories" Core..<@>
                 Core.parseXMLList "EventCategory")
                Core.<*> x Core..@? "SourceType"

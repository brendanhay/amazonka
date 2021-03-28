{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.EventCategoriesMap
  ( EventCategoriesMap (..)
  -- * Smart constructor
  , mkEventCategoriesMap
  -- * Lenses
  , ecmEvents
  , ecmSourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.EventInfoMap as Types

-- | Describes event categories.
--
-- /See:/ 'mkEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { events :: Core.Maybe [Types.EventInfoMap]
    -- ^ The events in the event category.
  , sourceType :: Core.Maybe Core.Text
    -- ^ The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventCategoriesMap' value with any optional fields omitted.
mkEventCategoriesMap
    :: EventCategoriesMap
mkEventCategoriesMap
  = EventCategoriesMap'{events = Core.Nothing,
                        sourceType = Core.Nothing}

-- | The events in the event category.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmEvents :: Lens.Lens' EventCategoriesMap (Core.Maybe [Types.EventInfoMap])
ecmEvents = Lens.field @"events"
{-# INLINEABLE ecmEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmSourceType :: Lens.Lens' EventCategoriesMap (Core.Maybe Core.Text)
ecmSourceType = Lens.field @"sourceType"
{-# INLINEABLE ecmSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' Core.<$>
              (x Core..@? "Events" Core..<@> Core.parseXMLList "EventInfoMap")
                Core.<*> x Core..@? "SourceType"

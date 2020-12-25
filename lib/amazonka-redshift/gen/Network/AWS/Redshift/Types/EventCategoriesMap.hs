{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.EventCategoriesMap
  ( EventCategoriesMap (..),

    -- * Smart constructor
    mkEventCategoriesMap,

    -- * Lenses
    ecmEvents,
    ecmSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.EventInfoMap as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes event categories.
--
-- /See:/ 'mkEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { -- | The events in the event category.
    events :: Core.Maybe [Types.EventInfoMap],
    -- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
    sourceType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventCategoriesMap' value with any optional fields omitted.
mkEventCategoriesMap ::
  EventCategoriesMap
mkEventCategoriesMap =
  EventCategoriesMap'
    { events = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The events in the event category.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmEvents :: Lens.Lens' EventCategoriesMap (Core.Maybe [Types.EventInfoMap])
ecmEvents = Lens.field @"events"
{-# DEPRECATED ecmEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmSourceType :: Lens.Lens' EventCategoriesMap (Core.Maybe Types.String)
ecmSourceType = Lens.field @"sourceType"
{-# DEPRECATED ecmSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Core.<$> (x Core..@? "Events" Core..<@> Core.parseXMLList "EventInfoMap")
      Core.<*> (x Core..@? "SourceType")

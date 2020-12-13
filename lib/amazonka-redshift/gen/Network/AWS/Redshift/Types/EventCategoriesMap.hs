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
    ecmSourceType,
    ecmEvents,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EventInfoMap

-- | Describes event categories.
--
-- /See:/ 'mkEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { -- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
    sourceType :: Lude.Maybe Lude.Text,
    -- | The events in the event category.
    events :: Lude.Maybe [EventInfoMap]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- * 'sourceType' - The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
-- * 'events' - The events in the event category.
mkEventCategoriesMap ::
  EventCategoriesMap
mkEventCategoriesMap =
  EventCategoriesMap'
    { sourceType = Lude.Nothing,
      events = Lude.Nothing
    }

-- | The source type, such as cluster or cluster-snapshot, that the returned categories belong to.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmSourceType :: Lens.Lens' EventCategoriesMap (Lude.Maybe Lude.Text)
ecmSourceType = Lens.lens (sourceType :: EventCategoriesMap -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: EventCategoriesMap)
{-# DEPRECATED ecmSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The events in the event category.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmEvents :: Lens.Lens' EventCategoriesMap (Lude.Maybe [EventInfoMap])
ecmEvents = Lens.lens (events :: EventCategoriesMap -> Lude.Maybe [EventInfoMap]) (\s a -> s {events = a} :: EventCategoriesMap)
{-# DEPRECATED ecmEvents "Use generic-lens or generic-optics with 'events' instead." #-}

instance Lude.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Lude.<$> (x Lude..@? "SourceType")
      Lude.<*> ( x Lude..@? "Events" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventInfoMap")
               )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EventCategoriesMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EventCategoriesMap
  ( EventCategoriesMap (..),

    -- * Smart constructor
    mkEventCategoriesMap,

    -- * Lenses
    ecmSourceType,
    ecmEventCategories,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the results of a successful invocation of the @DescribeEventCategories@ operation.
--
-- /See:/ 'mkEventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { sourceType ::
      Lude.Maybe Lude.Text,
    eventCategories :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- * 'eventCategories' - The event categories for the specified source type
-- * 'sourceType' - The source type that the returned categories belong to
mkEventCategoriesMap ::
  EventCategoriesMap
mkEventCategoriesMap =
  EventCategoriesMap'
    { sourceType = Lude.Nothing,
      eventCategories = Lude.Nothing
    }

-- | The source type that the returned categories belong to
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmSourceType :: Lens.Lens' EventCategoriesMap (Lude.Maybe Lude.Text)
ecmSourceType = Lens.lens (sourceType :: EventCategoriesMap -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: EventCategoriesMap)
{-# DEPRECATED ecmSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The event categories for the specified source type
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecmEventCategories :: Lens.Lens' EventCategoriesMap (Lude.Maybe [Lude.Text])
ecmEventCategories = Lens.lens (eventCategories :: EventCategoriesMap -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: EventCategoriesMap)
{-# DEPRECATED ecmEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

instance Lude.FromXML EventCategoriesMap where
  parseXML x =
    EventCategoriesMap'
      Lude.<$> (x Lude..@? "SourceType")
      Lude.<*> ( x Lude..@? "EventCategories" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EventCategory")
               )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventFilter
  ( EventFilter (..),

    -- * Smart constructor
    mkEventFilter,

    -- * Lenses
    efFilterType,
    efDimensions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import Network.AWS.Pinpoint.Types.FilterType
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for an event that causes a campaign to be sent or a journey activity to be performed.
--
-- /See:/ 'mkEventFilter' smart constructor.
data EventFilter = EventFilter'
  { filterType :: FilterType,
    dimensions :: EventDimensions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventFilter' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimensions for the event filter to use for the campaign or the journey activity.
-- * 'filterType' - The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
mkEventFilter ::
  -- | 'filterType'
  FilterType ->
  -- | 'dimensions'
  EventDimensions ->
  EventFilter
mkEventFilter pFilterType_ pDimensions_ =
  EventFilter'
    { filterType = pFilterType_,
      dimensions = pDimensions_
    }

-- | The type of event that causes the campaign to be sent or the journey activity to be performed. Valid values are: SYSTEM, sends the campaign or performs the activity when a system event occurs; and, ENDPOINT, sends the campaign or performs the activity when an endpoint event (<link>Events resource) occurs.
--
-- /Note:/ Consider using 'filterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efFilterType :: Lens.Lens' EventFilter FilterType
efFilterType = Lens.lens (filterType :: EventFilter -> FilterType) (\s a -> s {filterType = a} :: EventFilter)
{-# DEPRECATED efFilterType "Use generic-lens or generic-optics with 'filterType' instead." #-}

-- | The dimensions for the event filter to use for the campaign or the journey activity.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efDimensions :: Lens.Lens' EventFilter EventDimensions
efDimensions = Lens.lens (dimensions :: EventFilter -> EventDimensions) (\s a -> s {dimensions = a} :: EventFilter)
{-# DEPRECATED efDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON EventFilter where
  parseJSON =
    Lude.withObject
      "EventFilter"
      ( \x ->
          EventFilter'
            Lude.<$> (x Lude..: "FilterType") Lude.<*> (x Lude..: "Dimensions")
      )

instance Lude.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FilterType" Lude..= filterType),
            Lude.Just ("Dimensions" Lude..= dimensions)
          ]
      )

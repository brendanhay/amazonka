{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.StartCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.StartCondition
  ( StartCondition (..),

    -- * Smart constructor
    mkStartCondition,

    -- * Lenses
    scSegmentStartCondition,
    scEventStartCondition,
    scDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventStartCondition
import Network.AWS.Pinpoint.Types.SegmentCondition
import qualified Network.AWS.Prelude as Lude

-- | Specifies the conditions for the first activity in a journey. This activity and its conditions determine which users are participants in a journey.
--
-- /See:/ 'mkStartCondition' smart constructor.
data StartCondition = StartCondition'
  { -- | The segment that's associated with the first activity in the journey. This segment determines which users are participants in the journey.
    segmentStartCondition :: Lude.Maybe SegmentCondition,
    eventStartCondition :: Lude.Maybe EventStartCondition,
    -- | The custom description of the condition.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartCondition' with the minimum fields required to make a request.
--
-- * 'segmentStartCondition' - The segment that's associated with the first activity in the journey. This segment determines which users are participants in the journey.
-- * 'eventStartCondition' -
-- * 'description' - The custom description of the condition.
mkStartCondition ::
  StartCondition
mkStartCondition =
  StartCondition'
    { segmentStartCondition = Lude.Nothing,
      eventStartCondition = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The segment that's associated with the first activity in the journey. This segment determines which users are participants in the journey.
--
-- /Note:/ Consider using 'segmentStartCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSegmentStartCondition :: Lens.Lens' StartCondition (Lude.Maybe SegmentCondition)
scSegmentStartCondition = Lens.lens (segmentStartCondition :: StartCondition -> Lude.Maybe SegmentCondition) (\s a -> s {segmentStartCondition = a} :: StartCondition)
{-# DEPRECATED scSegmentStartCondition "Use generic-lens or generic-optics with 'segmentStartCondition' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eventStartCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEventStartCondition :: Lens.Lens' StartCondition (Lude.Maybe EventStartCondition)
scEventStartCondition = Lens.lens (eventStartCondition :: StartCondition -> Lude.Maybe EventStartCondition) (\s a -> s {eventStartCondition = a} :: StartCondition)
{-# DEPRECATED scEventStartCondition "Use generic-lens or generic-optics with 'eventStartCondition' instead." #-}

-- | The custom description of the condition.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDescription :: Lens.Lens' StartCondition (Lude.Maybe Lude.Text)
scDescription = Lens.lens (description :: StartCondition -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StartCondition)
{-# DEPRECATED scDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON StartCondition where
  parseJSON =
    Lude.withObject
      "StartCondition"
      ( \x ->
          StartCondition'
            Lude.<$> (x Lude..:? "SegmentStartCondition")
            Lude.<*> (x Lude..:? "EventStartCondition")
            Lude.<*> (x Lude..:? "Description")
      )

instance Lude.ToJSON StartCondition where
  toJSON StartCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SegmentStartCondition" Lude..=) Lude.<$> segmentStartCondition,
            ("EventStartCondition" Lude..=) Lude.<$> eventStartCondition,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

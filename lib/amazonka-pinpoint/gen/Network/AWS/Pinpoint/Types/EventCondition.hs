{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventCondition
  ( EventCondition (..),

    -- * Smart constructor
    mkEventCondition,

    -- * Lenses
    ecDimensions,
    ecMessageActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EventDimensions
import qualified Network.AWS.Prelude as Lude

-- | Specifies the conditions to evaluate for an event that applies to an activity in a journey.
--
-- /See:/ 'mkEventCondition' smart constructor.
data EventCondition = EventCondition'
  { dimensions ::
      Lude.Maybe EventDimensions,
    messageActivity :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventCondition' with the minimum fields required to make a request.
--
-- * 'dimensions' - The dimensions for the event filter to use for the activity.
-- * 'messageActivity' - The message identifier (message_id) for the message to use when determining whether message events meet the condition.
mkEventCondition ::
  EventCondition
mkEventCondition =
  EventCondition'
    { dimensions = Lude.Nothing,
      messageActivity = Lude.Nothing
    }

-- | The dimensions for the event filter to use for the activity.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecDimensions :: Lens.Lens' EventCondition (Lude.Maybe EventDimensions)
ecDimensions = Lens.lens (dimensions :: EventCondition -> Lude.Maybe EventDimensions) (\s a -> s {dimensions = a} :: EventCondition)
{-# DEPRECATED ecDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The message identifier (message_id) for the message to use when determining whether message events meet the condition.
--
-- /Note:/ Consider using 'messageActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecMessageActivity :: Lens.Lens' EventCondition (Lude.Maybe Lude.Text)
ecMessageActivity = Lens.lens (messageActivity :: EventCondition -> Lude.Maybe Lude.Text) (\s a -> s {messageActivity = a} :: EventCondition)
{-# DEPRECATED ecMessageActivity "Use generic-lens or generic-optics with 'messageActivity' instead." #-}

instance Lude.FromJSON EventCondition where
  parseJSON =
    Lude.withObject
      "EventCondition"
      ( \x ->
          EventCondition'
            Lude.<$> (x Lude..:? "Dimensions") Lude.<*> (x Lude..:? "MessageActivity")
      )

instance Lude.ToJSON EventCondition where
  toJSON EventCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("MessageActivity" Lude..=) Lude.<$> messageActivity
          ]
      )

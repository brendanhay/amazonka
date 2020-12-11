-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventDimensions
  ( EventDimensions (..),

    -- * Smart constructor
    mkEventDimensions,

    -- * Lenses
    edMetrics,
    edEventType,
    edAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Lude

-- | Specifies the dimensions for an event filter that determines when a campaign is sent or a journey activity is performed.
--
-- /See:/ 'mkEventDimensions' smart constructor.
data EventDimensions = EventDimensions'
  { metrics ::
      Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension)),
    eventType :: Lude.Maybe SetDimension,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventDimensions' with the minimum fields required to make a request.
--
-- * 'attributes' - One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
-- * 'eventType' - The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
-- * 'metrics' - One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
mkEventDimensions ::
  EventDimensions
mkEventDimensions =
  EventDimensions'
    { metrics = Lude.Nothing,
      eventType = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMetrics :: Lens.Lens' EventDimensions (Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension)))
edMetrics = Lens.lens (metrics :: EventDimensions -> Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension))) (\s a -> s {metrics = a} :: EventDimensions)
{-# DEPRECATED edMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventType :: Lens.Lens' EventDimensions (Lude.Maybe SetDimension)
edEventType = Lens.lens (eventType :: EventDimensions -> Lude.Maybe SetDimension) (\s a -> s {eventType = a} :: EventDimensions)
{-# DEPRECATED edEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edAttributes :: Lens.Lens' EventDimensions (Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension)))
edAttributes = Lens.lens (attributes :: EventDimensions -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension))) (\s a -> s {attributes = a} :: EventDimensions)
{-# DEPRECATED edAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON EventDimensions where
  parseJSON =
    Lude.withObject
      "EventDimensions"
      ( \x ->
          EventDimensions'
            Lude.<$> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EventType")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON EventDimensions where
  toJSON EventDimensions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Metrics" Lude..=) Lude.<$> metrics,
            ("EventType" Lude..=) Lude.<$> eventType,
            ("Attributes" Lude..=) Lude.<$> attributes
          ]
      )

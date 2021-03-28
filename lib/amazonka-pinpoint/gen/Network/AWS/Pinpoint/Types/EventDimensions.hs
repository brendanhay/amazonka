{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EventDimensions
  ( EventDimensions (..)
  -- * Smart constructor
  , mkEventDimensions
  -- * Lenses
  , edAttributes
  , edEventType
  , edMetrics
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.AttributeDimension as Types
import qualified Network.AWS.Pinpoint.Types.MetricDimension as Types
import qualified Network.AWS.Pinpoint.Types.SetDimension as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the dimensions for an event filter that determines when a campaign is sent or a journey activity is performed.
--
-- /See:/ 'mkEventDimensions' smart constructor.
data EventDimensions = EventDimensions'
  { attributes :: Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension)
    -- ^ One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
  , eventType :: Core.Maybe Types.SetDimension
    -- ^ The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
  , metrics :: Core.Maybe (Core.HashMap Core.Text Types.MetricDimension)
    -- ^ One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventDimensions' value with any optional fields omitted.
mkEventDimensions
    :: EventDimensions
mkEventDimensions
  = EventDimensions'{attributes = Core.Nothing,
                     eventType = Core.Nothing, metrics = Core.Nothing}

-- | One or more custom attributes that your application reports to Amazon Pinpoint. You can use these attributes as selection criteria when you create an event filter.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edAttributes :: Lens.Lens' EventDimensions (Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension))
edAttributes = Lens.field @"attributes"
{-# INLINEABLE edAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The name of the event that causes the campaign to be sent or the journey activity to be performed. This can be a standard event that Amazon Pinpoint generates, such as _email.delivered. For campaigns, this can also be a custom event that's specific to your application. For information about standard events, see <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events> in the /Amazon Pinpoint Developer Guide/ .
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edEventType :: Lens.Lens' EventDimensions (Core.Maybe Types.SetDimension)
edEventType = Lens.field @"eventType"
{-# INLINEABLE edEventType #-}
{-# DEPRECATED eventType "Use generic-lens or generic-optics with 'eventType' instead"  #-}

-- | One or more custom metrics that your application reports to Amazon Pinpoint. You can use these metrics as selection criteria when you create an event filter.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMetrics :: Lens.Lens' EventDimensions (Core.Maybe (Core.HashMap Core.Text Types.MetricDimension))
edMetrics = Lens.field @"metrics"
{-# INLINEABLE edMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

instance Core.FromJSON EventDimensions where
        toJSON EventDimensions{..}
          = Core.object
              (Core.catMaybes
                 [("Attributes" Core..=) Core.<$> attributes,
                  ("EventType" Core..=) Core.<$> eventType,
                  ("Metrics" Core..=) Core.<$> metrics])

instance Core.FromJSON EventDimensions where
        parseJSON
          = Core.withObject "EventDimensions" Core.$
              \ x ->
                EventDimensions' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "EventType" Core.<*>
                    x Core..:? "Metrics"

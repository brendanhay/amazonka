{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
  ( PutEventsRequestEntry (..),

    -- * Smart constructor
    mkPutEventsRequestEntry,

    -- * Lenses
    pereDetail,
    pereDetailType,
    pereEventBusName,
    pereResources,
    pereSource,
    pereTime,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.EventResource as Types
import qualified Network.AWS.CloudWatchEvents.Types.NonPartnerEventBusNameOrArn as Types
import qualified Network.AWS.CloudWatchEvents.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an event to be submitted.
--
-- /See:/ 'mkPutEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
  { -- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
    detail :: Core.Maybe Types.String,
    -- | Free-form string used to decide what fields to expect in the event detail.
    detailType :: Core.Maybe Types.String,
    -- | The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.NonPartnerEventBusNameOrArn,
    -- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
    resources :: Core.Maybe [Types.EventResource],
    -- | The source of the event.
    source :: Core.Maybe Types.String,
    -- | The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
    time :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutEventsRequestEntry' value with any optional fields omitted.
mkPutEventsRequestEntry ::
  PutEventsRequestEntry
mkPutEventsRequestEntry =
  PutEventsRequestEntry'
    { detail = Core.Nothing,
      detailType = Core.Nothing,
      eventBusName = Core.Nothing,
      resources = Core.Nothing,
      source = Core.Nothing,
      time = Core.Nothing
    }

-- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereDetail :: Lens.Lens' PutEventsRequestEntry (Core.Maybe Types.String)
pereDetail = Lens.field @"detail"
{-# DEPRECATED pereDetail "Use generic-lens or generic-optics with 'detail' instead." #-}

-- | Free-form string used to decide what fields to expect in the event detail.
--
-- /Note:/ Consider using 'detailType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereDetailType :: Lens.Lens' PutEventsRequestEntry (Core.Maybe Types.String)
pereDetailType = Lens.field @"detailType"
{-# DEPRECATED pereDetailType "Use generic-lens or generic-optics with 'detailType' instead." #-}

-- | The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereEventBusName :: Lens.Lens' PutEventsRequestEntry (Core.Maybe Types.NonPartnerEventBusNameOrArn)
pereEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED pereEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereResources :: Lens.Lens' PutEventsRequestEntry (Core.Maybe [Types.EventResource])
pereResources = Lens.field @"resources"
{-# DEPRECATED pereResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The source of the event.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereSource :: Lens.Lens' PutEventsRequestEntry (Core.Maybe Types.String)
pereSource = Lens.field @"source"
{-# DEPRECATED pereSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereTime :: Lens.Lens' PutEventsRequestEntry (Core.Maybe Core.NominalDiffTime)
pereTime = Lens.field @"time"
{-# DEPRECATED pereTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Core.FromJSON PutEventsRequestEntry where
  toJSON PutEventsRequestEntry {..} =
    Core.object
      ( Core.catMaybes
          [ ("Detail" Core..=) Core.<$> detail,
            ("DetailType" Core..=) Core.<$> detailType,
            ("EventBusName" Core..=) Core.<$> eventBusName,
            ("Resources" Core..=) Core.<$> resources,
            ("Source" Core..=) Core.<$> source,
            ("Time" Core..=) Core.<$> time
          ]
      )

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
    pereTime,
    pereDetailType,
    pereResources,
    pereEventBusName,
    pereSource,
    pereDetail,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an event to be submitted.
--
-- /See:/ 'mkPutEventsRequestEntry' smart constructor.
data PutEventsRequestEntry = PutEventsRequestEntry'
  { time ::
      Lude.Maybe Lude.Timestamp,
    detailType :: Lude.Maybe Lude.Text,
    resources :: Lude.Maybe [Lude.Text],
    eventBusName :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe Lude.Text,
    detail :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutEventsRequestEntry' with the minimum fields required to make a request.
--
-- * 'detail' - A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
-- * 'detailType' - Free-form string used to decide what fields to expect in the event detail.
-- * 'eventBusName' - The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
-- * 'resources' - AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
-- * 'source' - The source of the event.
-- * 'time' - The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
mkPutEventsRequestEntry ::
  PutEventsRequestEntry
mkPutEventsRequestEntry =
  PutEventsRequestEntry'
    { time = Lude.Nothing,
      detailType = Lude.Nothing,
      resources = Lude.Nothing,
      eventBusName = Lude.Nothing,
      source = Lude.Nothing,
      detail = Lude.Nothing
    }

-- | The time stamp of the event, per <https://www.rfc-editor.org/rfc/rfc3339.txt RFC3339> . If no time stamp is provided, the time stamp of the 'PutEvents' call is used.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereTime :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe Lude.Timestamp)
pereTime = Lens.lens (time :: PutEventsRequestEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {time = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereTime "Use generic-lens or generic-optics with 'time' instead." #-}

-- | Free-form string used to decide what fields to expect in the event detail.
--
-- /Note:/ Consider using 'detailType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereDetailType :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe Lude.Text)
pereDetailType = Lens.lens (detailType :: PutEventsRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {detailType = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereDetailType "Use generic-lens or generic-optics with 'detailType' instead." #-}

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereResources :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe [Lude.Text])
pereResources = Lens.lens (resources :: PutEventsRequestEntry -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The name or ARN of the event bus to receive the event. Only the rules that are associated with this event bus are used to match the event. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereEventBusName :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe Lude.Text)
pereEventBusName = Lens.lens (eventBusName :: PutEventsRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The source of the event.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereSource :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe Lude.Text)
pereSource = Lens.lens (source :: PutEventsRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pereDetail :: Lens.Lens' PutEventsRequestEntry (Lude.Maybe Lude.Text)
pereDetail = Lens.lens (detail :: PutEventsRequestEntry -> Lude.Maybe Lude.Text) (\s a -> s {detail = a} :: PutEventsRequestEntry)
{-# DEPRECATED pereDetail "Use generic-lens or generic-optics with 'detail' instead." #-}

instance Lude.ToJSON PutEventsRequestEntry where
  toJSON PutEventsRequestEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Time" Lude..=) Lude.<$> time,
            ("DetailType" Lude..=) Lude.<$> detailType,
            ("Resources" Lude..=) Lude.<$> resources,
            ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("Source" Lude..=) Lude.<$> source,
            ("Detail" Lude..=) Lude.<$> detail
          ]
      )

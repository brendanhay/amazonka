{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventDimensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventDimensions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the dimensions for an event filter that determines when a
-- campaign is sent or a journey activity is performed.
--
-- /See:/ 'newEventDimensions' smart constructor.
data EventDimensions = EventDimensions'
  { -- | The name of the event that causes the campaign to be sent or the journey
    -- activity to be performed. This can be a standard event that Amazon
    -- Pinpoint generates, such as _email.delivered. For campaigns, this can
    -- also be a custom event that\'s specific to your application. For
    -- information about standard events, see
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events>
    -- in the /Amazon Pinpoint Developer Guide/.
    eventType :: Prelude.Maybe SetDimension,
    -- | One or more custom attributes that your application reports to Amazon
    -- Pinpoint. You can use these attributes as selection criteria when you
    -- create an event filter.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension),
    -- | One or more custom metrics that your application reports to Amazon
    -- Pinpoint. You can use these metrics as selection criteria when you
    -- create an event filter.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDimensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'eventDimensions_eventType' - The name of the event that causes the campaign to be sent or the journey
-- activity to be performed. This can be a standard event that Amazon
-- Pinpoint generates, such as _email.delivered. For campaigns, this can
-- also be a custom event that\'s specific to your application. For
-- information about standard events, see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- 'attributes', 'eventDimensions_attributes' - One or more custom attributes that your application reports to Amazon
-- Pinpoint. You can use these attributes as selection criteria when you
-- create an event filter.
--
-- 'metrics', 'eventDimensions_metrics' - One or more custom metrics that your application reports to Amazon
-- Pinpoint. You can use these metrics as selection criteria when you
-- create an event filter.
newEventDimensions ::
  EventDimensions
newEventDimensions =
  EventDimensions'
    { eventType = Prelude.Nothing,
      attributes = Prelude.Nothing,
      metrics = Prelude.Nothing
    }

-- | The name of the event that causes the campaign to be sent or the journey
-- activity to be performed. This can be a standard event that Amazon
-- Pinpoint generates, such as _email.delivered. For campaigns, this can
-- also be a custom event that\'s specific to your application. For
-- information about standard events, see
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/event-streams.html Streaming Amazon Pinpoint Events>
-- in the /Amazon Pinpoint Developer Guide/.
eventDimensions_eventType :: Lens.Lens' EventDimensions (Prelude.Maybe SetDimension)
eventDimensions_eventType = Lens.lens (\EventDimensions' {eventType} -> eventType) (\s@EventDimensions' {} a -> s {eventType = a} :: EventDimensions)

-- | One or more custom attributes that your application reports to Amazon
-- Pinpoint. You can use these attributes as selection criteria when you
-- create an event filter.
eventDimensions_attributes :: Lens.Lens' EventDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension))
eventDimensions_attributes = Lens.lens (\EventDimensions' {attributes} -> attributes) (\s@EventDimensions' {} a -> s {attributes = a} :: EventDimensions) Prelude.. Lens.mapping Lens._Coerce

-- | One or more custom metrics that your application reports to Amazon
-- Pinpoint. You can use these metrics as selection criteria when you
-- create an event filter.
eventDimensions_metrics :: Lens.Lens' EventDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension))
eventDimensions_metrics = Lens.lens (\EventDimensions' {metrics} -> metrics) (\s@EventDimensions' {} a -> s {metrics = a} :: EventDimensions) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON EventDimensions where
  parseJSON =
    Core.withObject
      "EventDimensions"
      ( \x ->
          EventDimensions'
            Prelude.<$> (x Core..:? "EventType")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Metrics" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable EventDimensions

instance Prelude.NFData EventDimensions

instance Core.ToJSON EventDimensions where
  toJSON EventDimensions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventType" Core..=) Prelude.<$> eventType,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Metrics" Core..=) Prelude.<$> metrics
          ]
      )

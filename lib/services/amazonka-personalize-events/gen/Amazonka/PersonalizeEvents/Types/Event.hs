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
-- Module      : Amazonka.PersonalizeEvents.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeEvents.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents user interaction event information sent using the @PutEvents@
-- API.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The ID of the recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The event value that corresponds to the @EVENT_VALUE@ field of the
    -- Interactions schema.
    eventValue :: Prelude.Maybe Prelude.Double,
    -- | The item ID key that corresponds to the @ITEM_ID@ field of the
    -- Interactions schema.
    itemId :: Prelude.Maybe Prelude.Text,
    -- | A list of item IDs that represents the sequence of items you have shown
    -- the user. For example, @[\"itemId1\", \"itemId2\", \"itemId3\"]@.
    impression :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An ID associated with the event. If an event ID is not provided, Amazon
    -- Personalize generates a unique ID for the event. An event ID is not used
    -- as an input to the model. Amazon Personalize uses the event ID to
    -- distinquish unique events. Any subsequent events after the first with
    -- the same event ID are not used in model training.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | A string map of event-specific data that you might choose to record. For
    -- example, if a user rates a movie on your site, other than movie ID
    -- (@itemId@) and rating (@eventValue@) , you might also send the number of
    -- movie ratings made by the user.
    --
    -- Each item in the map consists of a key-value pair. For example,
    --
    -- @{\"numberOfRatings\": \"12\"}@
    --
    -- The keys use camel case names that match the fields in the Interactions
    -- schema. In the above example, the @numberOfRatings@ would match the
    -- \'NUMBER_OF_RATINGS\' field defined in the Interactions schema.
    properties :: Prelude.Maybe Prelude.Text,
    -- | The type of event, such as click or download. This property corresponds
    -- to the @EVENT_TYPE@ field of your Interactions schema and depends on the
    -- types of events you are tracking.
    eventType :: Prelude.Text,
    -- | The timestamp (in Unix time) on the client side when the event occurred.
    sentAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationId', 'event_recommendationId' - The ID of the recommendation.
--
-- 'eventValue', 'event_eventValue' - The event value that corresponds to the @EVENT_VALUE@ field of the
-- Interactions schema.
--
-- 'itemId', 'event_itemId' - The item ID key that corresponds to the @ITEM_ID@ field of the
-- Interactions schema.
--
-- 'impression', 'event_impression' - A list of item IDs that represents the sequence of items you have shown
-- the user. For example, @[\"itemId1\", \"itemId2\", \"itemId3\"]@.
--
-- 'eventId', 'event_eventId' - An ID associated with the event. If an event ID is not provided, Amazon
-- Personalize generates a unique ID for the event. An event ID is not used
-- as an input to the model. Amazon Personalize uses the event ID to
-- distinquish unique events. Any subsequent events after the first with
-- the same event ID are not used in model training.
--
-- 'properties', 'event_properties' - A string map of event-specific data that you might choose to record. For
-- example, if a user rates a movie on your site, other than movie ID
-- (@itemId@) and rating (@eventValue@) , you might also send the number of
-- movie ratings made by the user.
--
-- Each item in the map consists of a key-value pair. For example,
--
-- @{\"numberOfRatings\": \"12\"}@
--
-- The keys use camel case names that match the fields in the Interactions
-- schema. In the above example, the @numberOfRatings@ would match the
-- \'NUMBER_OF_RATINGS\' field defined in the Interactions schema.
--
-- 'eventType', 'event_eventType' - The type of event, such as click or download. This property corresponds
-- to the @EVENT_TYPE@ field of your Interactions schema and depends on the
-- types of events you are tracking.
--
-- 'sentAt', 'event_sentAt' - The timestamp (in Unix time) on the client side when the event occurred.
newEvent ::
  -- | 'eventType'
  Prelude.Text ->
  -- | 'sentAt'
  Prelude.UTCTime ->
  Event
newEvent pEventType_ pSentAt_ =
  Event'
    { recommendationId = Prelude.Nothing,
      eventValue = Prelude.Nothing,
      itemId = Prelude.Nothing,
      impression = Prelude.Nothing,
      eventId = Prelude.Nothing,
      properties = Prelude.Nothing,
      eventType = pEventType_,
      sentAt = Core._Time Lens.# pSentAt_
    }

-- | The ID of the recommendation.
event_recommendationId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_recommendationId = Lens.lens (\Event' {recommendationId} -> recommendationId) (\s@Event' {} a -> s {recommendationId = a} :: Event)

-- | The event value that corresponds to the @EVENT_VALUE@ field of the
-- Interactions schema.
event_eventValue :: Lens.Lens' Event (Prelude.Maybe Prelude.Double)
event_eventValue = Lens.lens (\Event' {eventValue} -> eventValue) (\s@Event' {} a -> s {eventValue = a} :: Event)

-- | The item ID key that corresponds to the @ITEM_ID@ field of the
-- Interactions schema.
event_itemId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_itemId = Lens.lens (\Event' {itemId} -> itemId) (\s@Event' {} a -> s {itemId = a} :: Event)

-- | A list of item IDs that represents the sequence of items you have shown
-- the user. For example, @[\"itemId1\", \"itemId2\", \"itemId3\"]@.
event_impression :: Lens.Lens' Event (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
event_impression = Lens.lens (\Event' {impression} -> impression) (\s@Event' {} a -> s {impression = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | An ID associated with the event. If an event ID is not provided, Amazon
-- Personalize generates a unique ID for the event. An event ID is not used
-- as an input to the model. Amazon Personalize uses the event ID to
-- distinquish unique events. Any subsequent events after the first with
-- the same event ID are not used in model training.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | A string map of event-specific data that you might choose to record. For
-- example, if a user rates a movie on your site, other than movie ID
-- (@itemId@) and rating (@eventValue@) , you might also send the number of
-- movie ratings made by the user.
--
-- Each item in the map consists of a key-value pair. For example,
--
-- @{\"numberOfRatings\": \"12\"}@
--
-- The keys use camel case names that match the fields in the Interactions
-- schema. In the above example, the @numberOfRatings@ would match the
-- \'NUMBER_OF_RATINGS\' field defined in the Interactions schema.
event_properties :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_properties = Lens.lens (\Event' {properties} -> properties) (\s@Event' {} a -> s {properties = a} :: Event)

-- | The type of event, such as click or download. This property corresponds
-- to the @EVENT_TYPE@ field of your Interactions schema and depends on the
-- types of events you are tracking.
event_eventType :: Lens.Lens' Event Prelude.Text
event_eventType = Lens.lens (\Event' {eventType} -> eventType) (\s@Event' {} a -> s {eventType = a} :: Event)

-- | The timestamp (in Unix time) on the client side when the event occurred.
event_sentAt :: Lens.Lens' Event Prelude.UTCTime
event_sentAt = Lens.lens (\Event' {sentAt} -> sentAt) (\s@Event' {} a -> s {sentAt = a} :: Event) Prelude.. Core._Time

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` recommendationId
      `Prelude.hashWithSalt` eventValue
      `Prelude.hashWithSalt` itemId
      `Prelude.hashWithSalt` impression
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` sentAt

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf eventValue
      `Prelude.seq` Prelude.rnf itemId
      `Prelude.seq` Prelude.rnf impression
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf sentAt

instance Core.ToJSON Event where
  toJSON Event' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("recommendationId" Core..=)
              Prelude.<$> recommendationId,
            ("eventValue" Core..=) Prelude.<$> eventValue,
            ("itemId" Core..=) Prelude.<$> itemId,
            ("impression" Core..=) Prelude.<$> impression,
            ("eventId" Core..=) Prelude.<$> eventId,
            ("properties" Core..=) Prelude.<$> properties,
            Prelude.Just ("eventType" Core..= eventType),
            Prelude.Just ("sentAt" Core..= sentAt)
          ]
      )

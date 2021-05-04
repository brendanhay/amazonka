{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DMS.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Event where

import Network.AWS.DMS.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an identifiable significant activity that affects a
-- replication instance or task. This object can provide the message, the
-- available event categories, the date and source of the event, and the
-- AWS DMS resource type.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The event message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The event categories available for the specified source type.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The date of the event.
    date :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of an event source.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | endpoint | replication-task
    sourceType :: Prelude.Maybe SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'event_message' - The event message.
--
-- 'eventCategories', 'event_eventCategories' - The event categories available for the specified source type.
--
-- 'date', 'event_date' - The date of the event.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - The identifier of an event source.
--
-- 'sourceType', 'event_sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
newEvent ::
  Event
newEvent =
  Event'
    { message = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      date = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The event message.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The event categories available for the specified source type.
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Prelude._Coerce

-- | The date of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of an event source.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | endpoint | replication-task
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Prelude.FromJSON Event where
  parseJSON =
    Prelude.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> ( x Prelude..:? "EventCategories"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Date")
            Prelude.<*> (x Prelude..:? "SourceIdentifier")
            Prelude.<*> (x Prelude..:? "SourceType")
      )

instance Prelude.Hashable Event

instance Prelude.NFData Event

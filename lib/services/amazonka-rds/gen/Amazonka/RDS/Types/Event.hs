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
-- Module      : Amazonka.RDS.Types.Event
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Event where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.SourceType

-- | This data type is used as a response element in the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEvents.html DescribeEvents>
-- action.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | Specifies the date and time of the event.
    date :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the category for the event.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | Provides the text of this event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the event.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | Provides the identifier for the source of the event.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the source type for this event.
    sourceType :: Prelude.Maybe SourceType
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
-- 'date', 'event_date' - Specifies the date and time of the event.
--
-- 'eventCategories', 'event_eventCategories' - Specifies the category for the event.
--
-- 'message', 'event_message' - Provides the text of this event.
--
-- 'sourceArn', 'event_sourceArn' - The Amazon Resource Name (ARN) for the event.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - Provides the identifier for the source of the event.
--
-- 'sourceType', 'event_sourceType' - Specifies the source type for this event.
newEvent ::
  Event
newEvent =
  Event'
    { date = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      message = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | Specifies the date and time of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | Specifies the category for the event.
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | Provides the text of this event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | The Amazon Resource Name (ARN) for the event.
event_sourceArn :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceArn = Lens.lens (\Event' {sourceArn} -> sourceArn) (\s@Event' {} a -> s {sourceArn = a} :: Event)

-- | Provides the identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | Specifies the source type for this event.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Data.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Data..@? "Date")
      Prelude.<*> ( x Data..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "SourceArn")
      Prelude.<*> (x Data..@? "SourceIdentifier")
      Prelude.<*> (x Data..@? "SourceType")

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` eventCategories
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf date
      `Prelude.seq` Prelude.rnf eventCategories
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceIdentifier
      `Prelude.seq` Prelude.rnf sourceType

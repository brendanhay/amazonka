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
-- Module      : Network.AWS.RDS.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Event where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.SourceType

-- | This data type is used as a response element in the @DescribeEvents@
-- action.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | Provides the text of this event.
    message :: Prelude.Maybe Prelude.Text,
    -- | Specifies the category for the event.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the date and time of the event.
    date :: Prelude.Maybe Core.ISO8601,
    -- | Provides the identifier for the source of the event.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the event.
    sourceArn :: Prelude.Maybe Prelude.Text,
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
-- 'message', 'event_message' - Provides the text of this event.
--
-- 'eventCategories', 'event_eventCategories' - Specifies the category for the event.
--
-- 'date', 'event_date' - Specifies the date and time of the event.
--
-- 'sourceIdentifier', 'event_sourceIdentifier' - Provides the identifier for the source of the event.
--
-- 'sourceArn', 'event_sourceArn' - The Amazon Resource Name (ARN) for the event.
--
-- 'sourceType', 'event_sourceType' - Specifies the source type for this event.
newEvent ::
  Event
newEvent =
  Event'
    { message = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      date = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | Provides the text of this event.
event_message :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_message = Lens.lens (\Event' {message} -> message) (\s@Event' {} a -> s {message = a} :: Event)

-- | Specifies the category for the event.
event_eventCategories :: Lens.Lens' Event (Prelude.Maybe [Prelude.Text])
event_eventCategories = Lens.lens (\Event' {eventCategories} -> eventCategories) (\s@Event' {} a -> s {eventCategories = a} :: Event) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the date and time of the event.
event_date :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_date = Lens.lens (\Event' {date} -> date) (\s@Event' {} a -> s {date = a} :: Event) Prelude.. Lens.mapping Core._Time

-- | Provides the identifier for the source of the event.
event_sourceIdentifier :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceIdentifier = Lens.lens (\Event' {sourceIdentifier} -> sourceIdentifier) (\s@Event' {} a -> s {sourceIdentifier = a} :: Event)

-- | The Amazon Resource Name (ARN) for the event.
event_sourceArn :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_sourceArn = Lens.lens (\Event' {sourceArn} -> sourceArn) (\s@Event' {} a -> s {sourceArn = a} :: Event)

-- | Specifies the source type for this event.
event_sourceType :: Lens.Lens' Event (Prelude.Maybe SourceType)
event_sourceType = Lens.lens (\Event' {sourceType} -> sourceType) (\s@Event' {} a -> s {sourceType = a} :: Event)

instance Core.FromXML Event where
  parseXML x =
    Event'
      Prelude.<$> (x Core..@? "Message")
      Prelude.<*> ( x Core..@? "EventCategories" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EventCategory")
                  )
      Prelude.<*> (x Core..@? "Date")
      Prelude.<*> (x Core..@? "SourceIdentifier")
      Prelude.<*> (x Core..@? "SourceArn")
      Prelude.<*> (x Core..@? "SourceType")

instance Prelude.Hashable Event

instance Prelude.NFData Event

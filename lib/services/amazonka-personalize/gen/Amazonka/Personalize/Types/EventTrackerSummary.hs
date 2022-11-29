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
-- Module      : Amazonka.Personalize.Types.EventTrackerSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.EventTrackerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of an event tracker. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeEventTracker.html DescribeEventTracker>
-- API.
--
-- /See:/ 'newEventTrackerSummary' smart constructor.
data EventTrackerSummary = EventTrackerSummary'
  { -- | The name of the event tracker.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the event tracker was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the event tracker.
    eventTrackerArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the event tracker.
    --
    -- An event tracker can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the event tracker was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventTrackerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventTrackerSummary_name' - The name of the event tracker.
--
-- 'creationDateTime', 'eventTrackerSummary_creationDateTime' - The date and time (in Unix time) that the event tracker was created.
--
-- 'eventTrackerArn', 'eventTrackerSummary_eventTrackerArn' - The Amazon Resource Name (ARN) of the event tracker.
--
-- 'status', 'eventTrackerSummary_status' - The status of the event tracker.
--
-- An event tracker can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'lastUpdatedDateTime', 'eventTrackerSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the event tracker was last
-- updated.
newEventTrackerSummary ::
  EventTrackerSummary
newEventTrackerSummary =
  EventTrackerSummary'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      eventTrackerArn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the event tracker.
eventTrackerSummary_name :: Lens.Lens' EventTrackerSummary (Prelude.Maybe Prelude.Text)
eventTrackerSummary_name = Lens.lens (\EventTrackerSummary' {name} -> name) (\s@EventTrackerSummary' {} a -> s {name = a} :: EventTrackerSummary)

-- | The date and time (in Unix time) that the event tracker was created.
eventTrackerSummary_creationDateTime :: Lens.Lens' EventTrackerSummary (Prelude.Maybe Prelude.UTCTime)
eventTrackerSummary_creationDateTime = Lens.lens (\EventTrackerSummary' {creationDateTime} -> creationDateTime) (\s@EventTrackerSummary' {} a -> s {creationDateTime = a} :: EventTrackerSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the event tracker.
eventTrackerSummary_eventTrackerArn :: Lens.Lens' EventTrackerSummary (Prelude.Maybe Prelude.Text)
eventTrackerSummary_eventTrackerArn = Lens.lens (\EventTrackerSummary' {eventTrackerArn} -> eventTrackerArn) (\s@EventTrackerSummary' {} a -> s {eventTrackerArn = a} :: EventTrackerSummary)

-- | The status of the event tracker.
--
-- An event tracker can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
eventTrackerSummary_status :: Lens.Lens' EventTrackerSummary (Prelude.Maybe Prelude.Text)
eventTrackerSummary_status = Lens.lens (\EventTrackerSummary' {status} -> status) (\s@EventTrackerSummary' {} a -> s {status = a} :: EventTrackerSummary)

-- | The date and time (in Unix time) that the event tracker was last
-- updated.
eventTrackerSummary_lastUpdatedDateTime :: Lens.Lens' EventTrackerSummary (Prelude.Maybe Prelude.UTCTime)
eventTrackerSummary_lastUpdatedDateTime = Lens.lens (\EventTrackerSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@EventTrackerSummary' {} a -> s {lastUpdatedDateTime = a} :: EventTrackerSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON EventTrackerSummary where
  parseJSON =
    Core.withObject
      "EventTrackerSummary"
      ( \x ->
          EventTrackerSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "eventTrackerArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable EventTrackerSummary where
  hashWithSalt _salt EventTrackerSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` eventTrackerArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData EventTrackerSummary where
  rnf EventTrackerSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf eventTrackerArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime

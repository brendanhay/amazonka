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
-- Module      : Amazonka.Personalize.Types.EventTracker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.EventTracker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an event tracker.
--
-- /See:/ 'newEventTracker' smart constructor.
data EventTracker = EventTracker'
  { -- | The Amazon Web Services account that owns the event tracker.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix format) that the event tracker was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group that receives the
    -- event data.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event tracker.
    eventTrackerArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the event tracker was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the event tracker.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the event tracker.
    --
    -- An event tracker can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event tracker. Include this ID in requests to the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
    -- API.
    trackingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'eventTracker_accountId' - The Amazon Web Services account that owns the event tracker.
--
-- 'creationDateTime', 'eventTracker_creationDateTime' - The date and time (in Unix format) that the event tracker was created.
--
-- 'datasetGroupArn', 'eventTracker_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that receives the
-- event data.
--
-- 'eventTrackerArn', 'eventTracker_eventTrackerArn' - The ARN of the event tracker.
--
-- 'lastUpdatedDateTime', 'eventTracker_lastUpdatedDateTime' - The date and time (in Unix time) that the event tracker was last
-- updated.
--
-- 'name', 'eventTracker_name' - The name of the event tracker.
--
-- 'status', 'eventTracker_status' - The status of the event tracker.
--
-- An event tracker can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'trackingId', 'eventTracker_trackingId' - The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
newEventTracker ::
  EventTracker
newEventTracker =
  EventTracker'
    { accountId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      eventTrackerArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      trackingId = Prelude.Nothing
    }

-- | The Amazon Web Services account that owns the event tracker.
eventTracker_accountId :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_accountId = Lens.lens (\EventTracker' {accountId} -> accountId) (\s@EventTracker' {} a -> s {accountId = a} :: EventTracker)

-- | The date and time (in Unix format) that the event tracker was created.
eventTracker_creationDateTime :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.UTCTime)
eventTracker_creationDateTime = Lens.lens (\EventTracker' {creationDateTime} -> creationDateTime) (\s@EventTracker' {} a -> s {creationDateTime = a} :: EventTracker) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the dataset group that receives the
-- event data.
eventTracker_datasetGroupArn :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_datasetGroupArn = Lens.lens (\EventTracker' {datasetGroupArn} -> datasetGroupArn) (\s@EventTracker' {} a -> s {datasetGroupArn = a} :: EventTracker)

-- | The ARN of the event tracker.
eventTracker_eventTrackerArn :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_eventTrackerArn = Lens.lens (\EventTracker' {eventTrackerArn} -> eventTrackerArn) (\s@EventTracker' {} a -> s {eventTrackerArn = a} :: EventTracker)

-- | The date and time (in Unix time) that the event tracker was last
-- updated.
eventTracker_lastUpdatedDateTime :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.UTCTime)
eventTracker_lastUpdatedDateTime = Lens.lens (\EventTracker' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@EventTracker' {} a -> s {lastUpdatedDateTime = a} :: EventTracker) Prelude.. Lens.mapping Data._Time

-- | The name of the event tracker.
eventTracker_name :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_name = Lens.lens (\EventTracker' {name} -> name) (\s@EventTracker' {} a -> s {name = a} :: EventTracker)

-- | The status of the event tracker.
--
-- An event tracker can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
eventTracker_status :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_status = Lens.lens (\EventTracker' {status} -> status) (\s@EventTracker' {} a -> s {status = a} :: EventTracker)

-- | The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
eventTracker_trackingId :: Lens.Lens' EventTracker (Prelude.Maybe Prelude.Text)
eventTracker_trackingId = Lens.lens (\EventTracker' {trackingId} -> trackingId) (\s@EventTracker' {} a -> s {trackingId = a} :: EventTracker)

instance Data.FromJSON EventTracker where
  parseJSON =
    Data.withObject
      "EventTracker"
      ( \x ->
          EventTracker'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "datasetGroupArn")
            Prelude.<*> (x Data..:? "eventTrackerArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "trackingId")
      )

instance Prelude.Hashable EventTracker where
  hashWithSalt _salt EventTracker' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` eventTrackerArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trackingId

instance Prelude.NFData EventTracker where
  rnf EventTracker' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf eventTrackerArn
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf trackingId

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
-- Module      : Amazonka.CloudTrail.Types.EventDataStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.EventDataStore where

import Amazonka.CloudTrail.Types.AdvancedEventSelector
import Amazonka.CloudTrail.Types.EventDataStoreStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A storage lake of event data against which you can run complex SQL-based
-- queries. An event data store can include events that you have logged on
-- your account from the last 90 to 2557 days (about three months to up to
-- seven years). To select events for an event data store, use
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html#creating-data-event-selectors-advanced advanced event selectors>.
--
-- /See:/ 'newEventDataStore' smart constructor.
data EventDataStore = EventDataStore'
  { -- | This field is being deprecated. The advanced event selectors that were
    -- used to select events for the data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | This field is being deprecated. The timestamp of the event data store\'s
    -- creation.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the event data store.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | This field is being deprecated. Indicates whether the event data store
    -- includes events from all regions, or only from the region in which it
    -- was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | This field is being deprecated. Indicates that an event data store is
    -- collecting logged events for an organization.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This field is being deprecated. The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | This field is being deprecated. The status of an event data store.
    -- Values are @ENABLED@ and @PENDING_DELETION@.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | This field is being deprecated. Indicates whether the event data store
    -- is protected from termination.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This field is being deprecated. The timestamp showing when an event data
    -- store was updated, if applicable. @UpdatedTimestamp@ is always either
    -- the same or newer than the time shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventDataStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedEventSelectors', 'eventDataStore_advancedEventSelectors' - This field is being deprecated. The advanced event selectors that were
-- used to select events for the data store.
--
-- 'createdTimestamp', 'eventDataStore_createdTimestamp' - This field is being deprecated. The timestamp of the event data store\'s
-- creation.
--
-- 'eventDataStoreArn', 'eventDataStore_eventDataStoreArn' - The ARN of the event data store.
--
-- 'multiRegionEnabled', 'eventDataStore_multiRegionEnabled' - This field is being deprecated. Indicates whether the event data store
-- includes events from all regions, or only from the region in which it
-- was created.
--
-- 'name', 'eventDataStore_name' - The name of the event data store.
--
-- 'organizationEnabled', 'eventDataStore_organizationEnabled' - This field is being deprecated. Indicates that an event data store is
-- collecting logged events for an organization.
--
-- 'retentionPeriod', 'eventDataStore_retentionPeriod' - This field is being deprecated. The retention period, in days.
--
-- 'status', 'eventDataStore_status' - This field is being deprecated. The status of an event data store.
-- Values are @ENABLED@ and @PENDING_DELETION@.
--
-- 'terminationProtectionEnabled', 'eventDataStore_terminationProtectionEnabled' - This field is being deprecated. Indicates whether the event data store
-- is protected from termination.
--
-- 'updatedTimestamp', 'eventDataStore_updatedTimestamp' - This field is being deprecated. The timestamp showing when an event data
-- store was updated, if applicable. @UpdatedTimestamp@ is always either
-- the same or newer than the time shown in @CreatedTimestamp@.
newEventDataStore ::
  EventDataStore
newEventDataStore =
  EventDataStore'
    { advancedEventSelectors =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | This field is being deprecated. The advanced event selectors that were
-- used to select events for the data store.
eventDataStore_advancedEventSelectors :: Lens.Lens' EventDataStore (Prelude.Maybe [AdvancedEventSelector])
eventDataStore_advancedEventSelectors = Lens.lens (\EventDataStore' {advancedEventSelectors} -> advancedEventSelectors) (\s@EventDataStore' {} a -> s {advancedEventSelectors = a} :: EventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | This field is being deprecated. The timestamp of the event data store\'s
-- creation.
eventDataStore_createdTimestamp :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.UTCTime)
eventDataStore_createdTimestamp = Lens.lens (\EventDataStore' {createdTimestamp} -> createdTimestamp) (\s@EventDataStore' {} a -> s {createdTimestamp = a} :: EventDataStore) Prelude.. Lens.mapping Data._Time

-- | The ARN of the event data store.
eventDataStore_eventDataStoreArn :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Text)
eventDataStore_eventDataStoreArn = Lens.lens (\EventDataStore' {eventDataStoreArn} -> eventDataStoreArn) (\s@EventDataStore' {} a -> s {eventDataStoreArn = a} :: EventDataStore)

-- | This field is being deprecated. Indicates whether the event data store
-- includes events from all regions, or only from the region in which it
-- was created.
eventDataStore_multiRegionEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_multiRegionEnabled = Lens.lens (\EventDataStore' {multiRegionEnabled} -> multiRegionEnabled) (\s@EventDataStore' {} a -> s {multiRegionEnabled = a} :: EventDataStore)

-- | The name of the event data store.
eventDataStore_name :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Text)
eventDataStore_name = Lens.lens (\EventDataStore' {name} -> name) (\s@EventDataStore' {} a -> s {name = a} :: EventDataStore)

-- | This field is being deprecated. Indicates that an event data store is
-- collecting logged events for an organization.
eventDataStore_organizationEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_organizationEnabled = Lens.lens (\EventDataStore' {organizationEnabled} -> organizationEnabled) (\s@EventDataStore' {} a -> s {organizationEnabled = a} :: EventDataStore)

-- | This field is being deprecated. The retention period, in days.
eventDataStore_retentionPeriod :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Natural)
eventDataStore_retentionPeriod = Lens.lens (\EventDataStore' {retentionPeriod} -> retentionPeriod) (\s@EventDataStore' {} a -> s {retentionPeriod = a} :: EventDataStore)

-- | This field is being deprecated. The status of an event data store.
-- Values are @ENABLED@ and @PENDING_DELETION@.
eventDataStore_status :: Lens.Lens' EventDataStore (Prelude.Maybe EventDataStoreStatus)
eventDataStore_status = Lens.lens (\EventDataStore' {status} -> status) (\s@EventDataStore' {} a -> s {status = a} :: EventDataStore)

-- | This field is being deprecated. Indicates whether the event data store
-- is protected from termination.
eventDataStore_terminationProtectionEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_terminationProtectionEnabled = Lens.lens (\EventDataStore' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@EventDataStore' {} a -> s {terminationProtectionEnabled = a} :: EventDataStore)

-- | This field is being deprecated. The timestamp showing when an event data
-- store was updated, if applicable. @UpdatedTimestamp@ is always either
-- the same or newer than the time shown in @CreatedTimestamp@.
eventDataStore_updatedTimestamp :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.UTCTime)
eventDataStore_updatedTimestamp = Lens.lens (\EventDataStore' {updatedTimestamp} -> updatedTimestamp) (\s@EventDataStore' {} a -> s {updatedTimestamp = a} :: EventDataStore) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EventDataStore where
  parseJSON =
    Data.withObject
      "EventDataStore"
      ( \x ->
          EventDataStore'
            Prelude.<$> ( x Data..:? "AdvancedEventSelectors"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "EventDataStoreArn")
            Prelude.<*> (x Data..:? "MultiRegionEnabled")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OrganizationEnabled")
            Prelude.<*> (x Data..:? "RetentionPeriod")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "TerminationProtectionEnabled")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable EventDataStore where
  hashWithSalt _salt EventDataStore' {..} =
    _salt `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` eventDataStoreArn
      `Prelude.hashWithSalt` multiRegionEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` organizationEnabled
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` terminationProtectionEnabled
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData EventDataStore where
  rnf EventDataStore' {..} =
    Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf eventDataStoreArn
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf updatedTimestamp

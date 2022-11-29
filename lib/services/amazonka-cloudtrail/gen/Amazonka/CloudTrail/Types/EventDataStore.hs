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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.EventDataStore where

import Amazonka.CloudTrail.Types.AdvancedEventSelector
import Amazonka.CloudTrail.Types.EventDataStoreStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A storage lake of event data against which you can run complex SQL-based
-- queries. An event data store can include events that you have logged on
-- your account from the last 90 to 2557 days (about three months to up to
-- seven years). To select events for an event data store, use
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html#creating-data-event-selectors-advanced advanced event selectors>.
--
-- /See:/ 'newEventDataStore' smart constructor.
data EventDataStore = EventDataStore'
  { -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event data store.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | This field is being deprecated. The advanced event selectors that were
    -- used to select events for the data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | This field is being deprecated. The timestamp of the event data store\'s
    -- creation.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | This field is being deprecated. Indicates whether the event data store
    -- includes events from all regions, or only from the region in which it
    -- was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This field is being deprecated. The timestamp showing when an event data
    -- store was updated, if applicable. @UpdatedTimestamp@ is always either
    -- the same or newer than the time shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | This field is being deprecated. The status of an event data store.
    -- Values are @ENABLED@ and @PENDING_DELETION@.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | This field is being deprecated. The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | This field is being deprecated. Indicates that an event data store is
    -- collecting logged events for an organization.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This field is being deprecated. Indicates whether the event data store
    -- is protected from termination.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'name', 'eventDataStore_name' - The name of the event data store.
--
-- 'eventDataStoreArn', 'eventDataStore_eventDataStoreArn' - The ARN of the event data store.
--
-- 'advancedEventSelectors', 'eventDataStore_advancedEventSelectors' - This field is being deprecated. The advanced event selectors that were
-- used to select events for the data store.
--
-- 'createdTimestamp', 'eventDataStore_createdTimestamp' - This field is being deprecated. The timestamp of the event data store\'s
-- creation.
--
-- 'multiRegionEnabled', 'eventDataStore_multiRegionEnabled' - This field is being deprecated. Indicates whether the event data store
-- includes events from all regions, or only from the region in which it
-- was created.
--
-- 'updatedTimestamp', 'eventDataStore_updatedTimestamp' - This field is being deprecated. The timestamp showing when an event data
-- store was updated, if applicable. @UpdatedTimestamp@ is always either
-- the same or newer than the time shown in @CreatedTimestamp@.
--
-- 'status', 'eventDataStore_status' - This field is being deprecated. The status of an event data store.
-- Values are @ENABLED@ and @PENDING_DELETION@.
--
-- 'retentionPeriod', 'eventDataStore_retentionPeriod' - This field is being deprecated. The retention period, in days.
--
-- 'organizationEnabled', 'eventDataStore_organizationEnabled' - This field is being deprecated. Indicates that an event data store is
-- collecting logged events for an organization.
--
-- 'terminationProtectionEnabled', 'eventDataStore_terminationProtectionEnabled' - This field is being deprecated. Indicates whether the event data store
-- is protected from termination.
newEventDataStore ::
  EventDataStore
newEventDataStore =
  EventDataStore'
    { name = Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing
    }

-- | The name of the event data store.
eventDataStore_name :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Text)
eventDataStore_name = Lens.lens (\EventDataStore' {name} -> name) (\s@EventDataStore' {} a -> s {name = a} :: EventDataStore)

-- | The ARN of the event data store.
eventDataStore_eventDataStoreArn :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Text)
eventDataStore_eventDataStoreArn = Lens.lens (\EventDataStore' {eventDataStoreArn} -> eventDataStoreArn) (\s@EventDataStore' {} a -> s {eventDataStoreArn = a} :: EventDataStore)

-- | This field is being deprecated. The advanced event selectors that were
-- used to select events for the data store.
eventDataStore_advancedEventSelectors :: Lens.Lens' EventDataStore (Prelude.Maybe [AdvancedEventSelector])
eventDataStore_advancedEventSelectors = Lens.lens (\EventDataStore' {advancedEventSelectors} -> advancedEventSelectors) (\s@EventDataStore' {} a -> s {advancedEventSelectors = a} :: EventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | This field is being deprecated. The timestamp of the event data store\'s
-- creation.
eventDataStore_createdTimestamp :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.UTCTime)
eventDataStore_createdTimestamp = Lens.lens (\EventDataStore' {createdTimestamp} -> createdTimestamp) (\s@EventDataStore' {} a -> s {createdTimestamp = a} :: EventDataStore) Prelude.. Lens.mapping Core._Time

-- | This field is being deprecated. Indicates whether the event data store
-- includes events from all regions, or only from the region in which it
-- was created.
eventDataStore_multiRegionEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_multiRegionEnabled = Lens.lens (\EventDataStore' {multiRegionEnabled} -> multiRegionEnabled) (\s@EventDataStore' {} a -> s {multiRegionEnabled = a} :: EventDataStore)

-- | This field is being deprecated. The timestamp showing when an event data
-- store was updated, if applicable. @UpdatedTimestamp@ is always either
-- the same or newer than the time shown in @CreatedTimestamp@.
eventDataStore_updatedTimestamp :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.UTCTime)
eventDataStore_updatedTimestamp = Lens.lens (\EventDataStore' {updatedTimestamp} -> updatedTimestamp) (\s@EventDataStore' {} a -> s {updatedTimestamp = a} :: EventDataStore) Prelude.. Lens.mapping Core._Time

-- | This field is being deprecated. The status of an event data store.
-- Values are @ENABLED@ and @PENDING_DELETION@.
eventDataStore_status :: Lens.Lens' EventDataStore (Prelude.Maybe EventDataStoreStatus)
eventDataStore_status = Lens.lens (\EventDataStore' {status} -> status) (\s@EventDataStore' {} a -> s {status = a} :: EventDataStore)

-- | This field is being deprecated. The retention period, in days.
eventDataStore_retentionPeriod :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Natural)
eventDataStore_retentionPeriod = Lens.lens (\EventDataStore' {retentionPeriod} -> retentionPeriod) (\s@EventDataStore' {} a -> s {retentionPeriod = a} :: EventDataStore)

-- | This field is being deprecated. Indicates that an event data store is
-- collecting logged events for an organization.
eventDataStore_organizationEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_organizationEnabled = Lens.lens (\EventDataStore' {organizationEnabled} -> organizationEnabled) (\s@EventDataStore' {} a -> s {organizationEnabled = a} :: EventDataStore)

-- | This field is being deprecated. Indicates whether the event data store
-- is protected from termination.
eventDataStore_terminationProtectionEnabled :: Lens.Lens' EventDataStore (Prelude.Maybe Prelude.Bool)
eventDataStore_terminationProtectionEnabled = Lens.lens (\EventDataStore' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@EventDataStore' {} a -> s {terminationProtectionEnabled = a} :: EventDataStore)

instance Core.FromJSON EventDataStore where
  parseJSON =
    Core.withObject
      "EventDataStore"
      ( \x ->
          EventDataStore'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "EventDataStoreArn")
            Prelude.<*> ( x Core..:? "AdvancedEventSelectors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "MultiRegionEnabled")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RetentionPeriod")
            Prelude.<*> (x Core..:? "OrganizationEnabled")
            Prelude.<*> (x Core..:? "TerminationProtectionEnabled")
      )

instance Prelude.Hashable EventDataStore where
  hashWithSalt _salt EventDataStore' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eventDataStoreArn
      `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` multiRegionEnabled
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` organizationEnabled
      `Prelude.hashWithSalt` terminationProtectionEnabled

instance Prelude.NFData EventDataStore where
  rnf EventDataStore' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventDataStoreArn
      `Prelude.seq` Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled

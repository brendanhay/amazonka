{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.UpdateEventDataStore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event data store. The required @EventDataStore@ value is an
-- ARN or the ID portion of the ARN. Other parameters are optional, but at
-- least one optional parameter must be specified, or CloudTrail throws an
-- error. @RetentionPeriod@ is in days, and valid values are integers
-- between 90 and 2555. By default, @TerminationProtection@ is enabled.
-- @AdvancedEventSelectors@ includes or excludes management and data events
-- in your event data store; for more information about
-- @AdvancedEventSelectors@, see
-- PutEventSelectorsRequest$AdvancedEventSelectors.
module Amazonka.CloudTrail.UpdateEventDataStore
  ( -- * Creating a Request
    UpdateEventDataStore (..),
    newUpdateEventDataStore,

    -- * Request Lenses
    updateEventDataStore_name,
    updateEventDataStore_advancedEventSelectors,
    updateEventDataStore_multiRegionEnabled,
    updateEventDataStore_retentionPeriod,
    updateEventDataStore_organizationEnabled,
    updateEventDataStore_terminationProtectionEnabled,
    updateEventDataStore_eventDataStore,

    -- * Destructuring the Response
    UpdateEventDataStoreResponse (..),
    newUpdateEventDataStoreResponse,

    -- * Response Lenses
    updateEventDataStoreResponse_name,
    updateEventDataStoreResponse_eventDataStoreArn,
    updateEventDataStoreResponse_advancedEventSelectors,
    updateEventDataStoreResponse_createdTimestamp,
    updateEventDataStoreResponse_multiRegionEnabled,
    updateEventDataStoreResponse_updatedTimestamp,
    updateEventDataStoreResponse_status,
    updateEventDataStoreResponse_retentionPeriod,
    updateEventDataStoreResponse_organizationEnabled,
    updateEventDataStoreResponse_terminationProtectionEnabled,
    updateEventDataStoreResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventDataStore' smart constructor.
data UpdateEventDataStore = UpdateEventDataStore'
  { -- | The event data store name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The advanced event selectors used to select events for the event data
    -- store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | Specifies whether an event data store collects events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether an event data store collects events logged for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that termination protection is enabled and the event data
    -- store cannot be automatically deleted.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN (or the ID suffix of the ARN) of the event data store that you
    -- want to update.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventDataStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEventDataStore_name' - The event data store name.
--
-- 'advancedEventSelectors', 'updateEventDataStore_advancedEventSelectors' - The advanced event selectors used to select events for the event data
-- store.
--
-- 'multiRegionEnabled', 'updateEventDataStore_multiRegionEnabled' - Specifies whether an event data store collects events from all regions,
-- or only from the region in which it was created.
--
-- 'retentionPeriod', 'updateEventDataStore_retentionPeriod' - The retention period, in days.
--
-- 'organizationEnabled', 'updateEventDataStore_organizationEnabled' - Specifies whether an event data store collects events logged for an
-- organization in Organizations.
--
-- 'terminationProtectionEnabled', 'updateEventDataStore_terminationProtectionEnabled' - Indicates that termination protection is enabled and the event data
-- store cannot be automatically deleted.
--
-- 'eventDataStore', 'updateEventDataStore_eventDataStore' - The ARN (or the ID suffix of the ARN) of the event data store that you
-- want to update.
newUpdateEventDataStore ::
  -- | 'eventDataStore'
  Prelude.Text ->
  UpdateEventDataStore
newUpdateEventDataStore pEventDataStore_ =
  UpdateEventDataStore'
    { name = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing,
      eventDataStore = pEventDataStore_
    }

-- | The event data store name.
updateEventDataStore_name :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Text)
updateEventDataStore_name = Lens.lens (\UpdateEventDataStore' {name} -> name) (\s@UpdateEventDataStore' {} a -> s {name = a} :: UpdateEventDataStore)

-- | The advanced event selectors used to select events for the event data
-- store.
updateEventDataStore_advancedEventSelectors :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe [AdvancedEventSelector])
updateEventDataStore_advancedEventSelectors = Lens.lens (\UpdateEventDataStore' {advancedEventSelectors} -> advancedEventSelectors) (\s@UpdateEventDataStore' {} a -> s {advancedEventSelectors = a} :: UpdateEventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether an event data store collects events from all regions,
-- or only from the region in which it was created.
updateEventDataStore_multiRegionEnabled :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Bool)
updateEventDataStore_multiRegionEnabled = Lens.lens (\UpdateEventDataStore' {multiRegionEnabled} -> multiRegionEnabled) (\s@UpdateEventDataStore' {} a -> s {multiRegionEnabled = a} :: UpdateEventDataStore)

-- | The retention period, in days.
updateEventDataStore_retentionPeriod :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Natural)
updateEventDataStore_retentionPeriod = Lens.lens (\UpdateEventDataStore' {retentionPeriod} -> retentionPeriod) (\s@UpdateEventDataStore' {} a -> s {retentionPeriod = a} :: UpdateEventDataStore)

-- | Specifies whether an event data store collects events logged for an
-- organization in Organizations.
updateEventDataStore_organizationEnabled :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Bool)
updateEventDataStore_organizationEnabled = Lens.lens (\UpdateEventDataStore' {organizationEnabled} -> organizationEnabled) (\s@UpdateEventDataStore' {} a -> s {organizationEnabled = a} :: UpdateEventDataStore)

-- | Indicates that termination protection is enabled and the event data
-- store cannot be automatically deleted.
updateEventDataStore_terminationProtectionEnabled :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Bool)
updateEventDataStore_terminationProtectionEnabled = Lens.lens (\UpdateEventDataStore' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@UpdateEventDataStore' {} a -> s {terminationProtectionEnabled = a} :: UpdateEventDataStore)

-- | The ARN (or the ID suffix of the ARN) of the event data store that you
-- want to update.
updateEventDataStore_eventDataStore :: Lens.Lens' UpdateEventDataStore Prelude.Text
updateEventDataStore_eventDataStore = Lens.lens (\UpdateEventDataStore' {eventDataStore} -> eventDataStore) (\s@UpdateEventDataStore' {} a -> s {eventDataStore = a} :: UpdateEventDataStore)

instance Core.AWSRequest UpdateEventDataStore where
  type
    AWSResponse UpdateEventDataStore =
      UpdateEventDataStoreResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEventDataStoreResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "EventDataStoreArn")
            Prelude.<*> ( x Core..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "MultiRegionEnabled")
            Prelude.<*> (x Core..?> "UpdatedTimestamp")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "RetentionPeriod")
            Prelude.<*> (x Core..?> "OrganizationEnabled")
            Prelude.<*> (x Core..?> "TerminationProtectionEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventDataStore where
  hashWithSalt _salt UpdateEventDataStore' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` multiRegionEnabled
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` organizationEnabled
      `Prelude.hashWithSalt` terminationProtectionEnabled
      `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData UpdateEventDataStore where
  rnf UpdateEventDataStore' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf eventDataStore

instance Core.ToHeaders UpdateEventDataStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateEventDataStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEventDataStore where
  toJSON UpdateEventDataStore' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("AdvancedEventSelectors" Core..=)
              Prelude.<$> advancedEventSelectors,
            ("MultiRegionEnabled" Core..=)
              Prelude.<$> multiRegionEnabled,
            ("RetentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            ("OrganizationEnabled" Core..=)
              Prelude.<$> organizationEnabled,
            ("TerminationProtectionEnabled" Core..=)
              Prelude.<$> terminationProtectionEnabled,
            Prelude.Just
              ("EventDataStore" Core..= eventDataStore)
          ]
      )

instance Core.ToPath UpdateEventDataStore where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateEventDataStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventDataStoreResponse' smart constructor.
data UpdateEventDataStoreResponse = UpdateEventDataStoreResponse'
  { -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event data store.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The advanced event selectors that are applied to the event data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The timestamp that shows when an event data store was first created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the event data store includes events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp that shows when the event data store was last updated.
    -- @UpdatedTimestamp@ is always either the same or newer than the time
    -- shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of an event data store. Values can be @ENABLED@ and
    -- @PENDING_DELETION@.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether an event data store is collecting logged events for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether termination protection is enabled for the event data
    -- store.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventDataStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEventDataStoreResponse_name' - The name of the event data store.
--
-- 'eventDataStoreArn', 'updateEventDataStoreResponse_eventDataStoreArn' - The ARN of the event data store.
--
-- 'advancedEventSelectors', 'updateEventDataStoreResponse_advancedEventSelectors' - The advanced event selectors that are applied to the event data store.
--
-- 'createdTimestamp', 'updateEventDataStoreResponse_createdTimestamp' - The timestamp that shows when an event data store was first created.
--
-- 'multiRegionEnabled', 'updateEventDataStoreResponse_multiRegionEnabled' - Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
--
-- 'updatedTimestamp', 'updateEventDataStoreResponse_updatedTimestamp' - The timestamp that shows when the event data store was last updated.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
--
-- 'status', 'updateEventDataStoreResponse_status' - The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
--
-- 'retentionPeriod', 'updateEventDataStoreResponse_retentionPeriod' - The retention period, in days.
--
-- 'organizationEnabled', 'updateEventDataStoreResponse_organizationEnabled' - Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
--
-- 'terminationProtectionEnabled', 'updateEventDataStoreResponse_terminationProtectionEnabled' - Indicates whether termination protection is enabled for the event data
-- store.
--
-- 'httpStatus', 'updateEventDataStoreResponse_httpStatus' - The response's http status code.
newUpdateEventDataStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventDataStoreResponse
newUpdateEventDataStoreResponse pHttpStatus_ =
  UpdateEventDataStoreResponse'
    { name =
        Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the event data store.
updateEventDataStoreResponse_name :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
updateEventDataStoreResponse_name = Lens.lens (\UpdateEventDataStoreResponse' {name} -> name) (\s@UpdateEventDataStoreResponse' {} a -> s {name = a} :: UpdateEventDataStoreResponse)

-- | The ARN of the event data store.
updateEventDataStoreResponse_eventDataStoreArn :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
updateEventDataStoreResponse_eventDataStoreArn = Lens.lens (\UpdateEventDataStoreResponse' {eventDataStoreArn} -> eventDataStoreArn) (\s@UpdateEventDataStoreResponse' {} a -> s {eventDataStoreArn = a} :: UpdateEventDataStoreResponse)

-- | The advanced event selectors that are applied to the event data store.
updateEventDataStoreResponse_advancedEventSelectors :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe [AdvancedEventSelector])
updateEventDataStoreResponse_advancedEventSelectors = Lens.lens (\UpdateEventDataStoreResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@UpdateEventDataStoreResponse' {} a -> s {advancedEventSelectors = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that shows when an event data store was first created.
updateEventDataStoreResponse_createdTimestamp :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
updateEventDataStoreResponse_createdTimestamp = Lens.lens (\UpdateEventDataStoreResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdateEventDataStoreResponse' {} a -> s {createdTimestamp = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
updateEventDataStoreResponse_multiRegionEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_multiRegionEnabled = Lens.lens (\UpdateEventDataStoreResponse' {multiRegionEnabled} -> multiRegionEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {multiRegionEnabled = a} :: UpdateEventDataStoreResponse)

-- | The timestamp that shows when the event data store was last updated.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
updateEventDataStoreResponse_updatedTimestamp :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
updateEventDataStoreResponse_updatedTimestamp = Lens.lens (\UpdateEventDataStoreResponse' {updatedTimestamp} -> updatedTimestamp) (\s@UpdateEventDataStoreResponse' {} a -> s {updatedTimestamp = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
updateEventDataStoreResponse_status :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe EventDataStoreStatus)
updateEventDataStoreResponse_status = Lens.lens (\UpdateEventDataStoreResponse' {status} -> status) (\s@UpdateEventDataStoreResponse' {} a -> s {status = a} :: UpdateEventDataStoreResponse)

-- | The retention period, in days.
updateEventDataStoreResponse_retentionPeriod :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Natural)
updateEventDataStoreResponse_retentionPeriod = Lens.lens (\UpdateEventDataStoreResponse' {retentionPeriod} -> retentionPeriod) (\s@UpdateEventDataStoreResponse' {} a -> s {retentionPeriod = a} :: UpdateEventDataStoreResponse)

-- | Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
updateEventDataStoreResponse_organizationEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_organizationEnabled = Lens.lens (\UpdateEventDataStoreResponse' {organizationEnabled} -> organizationEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {organizationEnabled = a} :: UpdateEventDataStoreResponse)

-- | Indicates whether termination protection is enabled for the event data
-- store.
updateEventDataStoreResponse_terminationProtectionEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_terminationProtectionEnabled = Lens.lens (\UpdateEventDataStoreResponse' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {terminationProtectionEnabled = a} :: UpdateEventDataStoreResponse)

-- | The response's http status code.
updateEventDataStoreResponse_httpStatus :: Lens.Lens' UpdateEventDataStoreResponse Prelude.Int
updateEventDataStoreResponse_httpStatus = Lens.lens (\UpdateEventDataStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateEventDataStoreResponse' {} a -> s {httpStatus = a} :: UpdateEventDataStoreResponse)

instance Prelude.NFData UpdateEventDataStoreResponse where
  rnf UpdateEventDataStoreResponse' {..} =
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
      `Prelude.seq` Prelude.rnf httpStatus

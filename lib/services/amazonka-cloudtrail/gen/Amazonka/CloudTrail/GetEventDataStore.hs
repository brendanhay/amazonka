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
-- Module      : Amazonka.CloudTrail.GetEventDataStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an event data store specified as either an ARN
-- or the ID portion of the ARN.
module Amazonka.CloudTrail.GetEventDataStore
  ( -- * Creating a Request
    GetEventDataStore (..),
    newGetEventDataStore,

    -- * Request Lenses
    getEventDataStore_eventDataStore,

    -- * Destructuring the Response
    GetEventDataStoreResponse (..),
    newGetEventDataStoreResponse,

    -- * Response Lenses
    getEventDataStoreResponse_name,
    getEventDataStoreResponse_eventDataStoreArn,
    getEventDataStoreResponse_advancedEventSelectors,
    getEventDataStoreResponse_createdTimestamp,
    getEventDataStoreResponse_multiRegionEnabled,
    getEventDataStoreResponse_updatedTimestamp,
    getEventDataStoreResponse_status,
    getEventDataStoreResponse_retentionPeriod,
    getEventDataStoreResponse_kmsKeyId,
    getEventDataStoreResponse_organizationEnabled,
    getEventDataStoreResponse_terminationProtectionEnabled,
    getEventDataStoreResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventDataStore' smart constructor.
data GetEventDataStore = GetEventDataStore'
  { -- | The ARN (or ID suffix of the ARN) of the event data store about which
    -- you want information.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventDataStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDataStore', 'getEventDataStore_eventDataStore' - The ARN (or ID suffix of the ARN) of the event data store about which
-- you want information.
newGetEventDataStore ::
  -- | 'eventDataStore'
  Prelude.Text ->
  GetEventDataStore
newGetEventDataStore pEventDataStore_ =
  GetEventDataStore'
    { eventDataStore =
        pEventDataStore_
    }

-- | The ARN (or ID suffix of the ARN) of the event data store about which
-- you want information.
getEventDataStore_eventDataStore :: Lens.Lens' GetEventDataStore Prelude.Text
getEventDataStore_eventDataStore = Lens.lens (\GetEventDataStore' {eventDataStore} -> eventDataStore) (\s@GetEventDataStore' {} a -> s {eventDataStore = a} :: GetEventDataStore)

instance Core.AWSRequest GetEventDataStore where
  type
    AWSResponse GetEventDataStore =
      GetEventDataStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventDataStoreResponse'
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
            Prelude.<*> (x Core..?> "KmsKeyId")
            Prelude.<*> (x Core..?> "OrganizationEnabled")
            Prelude.<*> (x Core..?> "TerminationProtectionEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventDataStore where
  hashWithSalt _salt GetEventDataStore' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData GetEventDataStore where
  rnf GetEventDataStore' {..} =
    Prelude.rnf eventDataStore

instance Core.ToHeaders GetEventDataStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventDataStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEventDataStore where
  toJSON GetEventDataStore' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EventDataStore" Core..= eventDataStore)
          ]
      )

instance Core.ToPath GetEventDataStore where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEventDataStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventDataStoreResponse' smart constructor.
data GetEventDataStoreResponse = GetEventDataStoreResponse'
  { -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The event data store Amazon Resource Number (ARN).
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The advanced event selectors used to select events for the data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The timestamp of the event data store\'s creation.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the event data store includes events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Shows the time that an event data store was updated, if applicable.
    -- @UpdatedTimestamp@ is always either the same or newer than the time
    -- shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of an event data store. Values can be @ENABLED@ and
    -- @PENDING_DELETION@.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | The retention period of the event data store, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the KMS key ID that encrypts the events delivered by
    -- CloudTrail. The value is a fully specified ARN to a KMS key in the
    -- following format.
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an event data store is collecting logged events for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that termination protection is enabled.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventDataStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getEventDataStoreResponse_name' - The name of the event data store.
--
-- 'eventDataStoreArn', 'getEventDataStoreResponse_eventDataStoreArn' - The event data store Amazon Resource Number (ARN).
--
-- 'advancedEventSelectors', 'getEventDataStoreResponse_advancedEventSelectors' - The advanced event selectors used to select events for the data store.
--
-- 'createdTimestamp', 'getEventDataStoreResponse_createdTimestamp' - The timestamp of the event data store\'s creation.
--
-- 'multiRegionEnabled', 'getEventDataStoreResponse_multiRegionEnabled' - Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
--
-- 'updatedTimestamp', 'getEventDataStoreResponse_updatedTimestamp' - Shows the time that an event data store was updated, if applicable.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
--
-- 'status', 'getEventDataStoreResponse_status' - The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
--
-- 'retentionPeriod', 'getEventDataStoreResponse_retentionPeriod' - The retention period of the event data store, in days.
--
-- 'kmsKeyId', 'getEventDataStoreResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 'organizationEnabled', 'getEventDataStoreResponse_organizationEnabled' - Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
--
-- 'terminationProtectionEnabled', 'getEventDataStoreResponse_terminationProtectionEnabled' - Indicates that termination protection is enabled.
--
-- 'httpStatus', 'getEventDataStoreResponse_httpStatus' - The response's http status code.
newGetEventDataStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventDataStoreResponse
newGetEventDataStoreResponse pHttpStatus_ =
  GetEventDataStoreResponse'
    { name = Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the event data store.
getEventDataStoreResponse_name :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Text)
getEventDataStoreResponse_name = Lens.lens (\GetEventDataStoreResponse' {name} -> name) (\s@GetEventDataStoreResponse' {} a -> s {name = a} :: GetEventDataStoreResponse)

-- | The event data store Amazon Resource Number (ARN).
getEventDataStoreResponse_eventDataStoreArn :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Text)
getEventDataStoreResponse_eventDataStoreArn = Lens.lens (\GetEventDataStoreResponse' {eventDataStoreArn} -> eventDataStoreArn) (\s@GetEventDataStoreResponse' {} a -> s {eventDataStoreArn = a} :: GetEventDataStoreResponse)

-- | The advanced event selectors used to select events for the data store.
getEventDataStoreResponse_advancedEventSelectors :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe [AdvancedEventSelector])
getEventDataStoreResponse_advancedEventSelectors = Lens.lens (\GetEventDataStoreResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@GetEventDataStoreResponse' {} a -> s {advancedEventSelectors = a} :: GetEventDataStoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of the event data store\'s creation.
getEventDataStoreResponse_createdTimestamp :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
getEventDataStoreResponse_createdTimestamp = Lens.lens (\GetEventDataStoreResponse' {createdTimestamp} -> createdTimestamp) (\s@GetEventDataStoreResponse' {} a -> s {createdTimestamp = a} :: GetEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
getEventDataStoreResponse_multiRegionEnabled :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
getEventDataStoreResponse_multiRegionEnabled = Lens.lens (\GetEventDataStoreResponse' {multiRegionEnabled} -> multiRegionEnabled) (\s@GetEventDataStoreResponse' {} a -> s {multiRegionEnabled = a} :: GetEventDataStoreResponse)

-- | Shows the time that an event data store was updated, if applicable.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
getEventDataStoreResponse_updatedTimestamp :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
getEventDataStoreResponse_updatedTimestamp = Lens.lens (\GetEventDataStoreResponse' {updatedTimestamp} -> updatedTimestamp) (\s@GetEventDataStoreResponse' {} a -> s {updatedTimestamp = a} :: GetEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
getEventDataStoreResponse_status :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe EventDataStoreStatus)
getEventDataStoreResponse_status = Lens.lens (\GetEventDataStoreResponse' {status} -> status) (\s@GetEventDataStoreResponse' {} a -> s {status = a} :: GetEventDataStoreResponse)

-- | The retention period of the event data store, in days.
getEventDataStoreResponse_retentionPeriod :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Natural)
getEventDataStoreResponse_retentionPeriod = Lens.lens (\GetEventDataStoreResponse' {retentionPeriod} -> retentionPeriod) (\s@GetEventDataStoreResponse' {} a -> s {retentionPeriod = a} :: GetEventDataStoreResponse)

-- | Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
getEventDataStoreResponse_kmsKeyId :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Text)
getEventDataStoreResponse_kmsKeyId = Lens.lens (\GetEventDataStoreResponse' {kmsKeyId} -> kmsKeyId) (\s@GetEventDataStoreResponse' {} a -> s {kmsKeyId = a} :: GetEventDataStoreResponse)

-- | Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
getEventDataStoreResponse_organizationEnabled :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
getEventDataStoreResponse_organizationEnabled = Lens.lens (\GetEventDataStoreResponse' {organizationEnabled} -> organizationEnabled) (\s@GetEventDataStoreResponse' {} a -> s {organizationEnabled = a} :: GetEventDataStoreResponse)

-- | Indicates that termination protection is enabled.
getEventDataStoreResponse_terminationProtectionEnabled :: Lens.Lens' GetEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
getEventDataStoreResponse_terminationProtectionEnabled = Lens.lens (\GetEventDataStoreResponse' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@GetEventDataStoreResponse' {} a -> s {terminationProtectionEnabled = a} :: GetEventDataStoreResponse)

-- | The response's http status code.
getEventDataStoreResponse_httpStatus :: Lens.Lens' GetEventDataStoreResponse Prelude.Int
getEventDataStoreResponse_httpStatus = Lens.lens (\GetEventDataStoreResponse' {httpStatus} -> httpStatus) (\s@GetEventDataStoreResponse' {} a -> s {httpStatus = a} :: GetEventDataStoreResponse)

instance Prelude.NFData GetEventDataStoreResponse where
  rnf GetEventDataStoreResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventDataStoreArn
      `Prelude.seq` Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf httpStatus

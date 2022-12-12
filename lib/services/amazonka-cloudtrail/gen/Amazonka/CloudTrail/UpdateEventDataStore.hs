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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event data store. The required @EventDataStore@ value is an
-- ARN or the ID portion of the ARN. Other parameters are optional, but at
-- least one optional parameter must be specified, or CloudTrail throws an
-- error. @RetentionPeriod@ is in days, and valid values are integers
-- between 90 and 2557. By default, @TerminationProtection@ is enabled.
-- @AdvancedEventSelectors@ includes or excludes management and data events
-- in your event data store; for more information about
-- @AdvancedEventSelectors@, see
-- PutEventSelectorsRequest$AdvancedEventSelectors.
module Amazonka.CloudTrail.UpdateEventDataStore
  ( -- * Creating a Request
    UpdateEventDataStore (..),
    newUpdateEventDataStore,

    -- * Request Lenses
    updateEventDataStore_advancedEventSelectors,
    updateEventDataStore_kmsKeyId,
    updateEventDataStore_multiRegionEnabled,
    updateEventDataStore_name,
    updateEventDataStore_organizationEnabled,
    updateEventDataStore_retentionPeriod,
    updateEventDataStore_terminationProtectionEnabled,
    updateEventDataStore_eventDataStore,

    -- * Destructuring the Response
    UpdateEventDataStoreResponse (..),
    newUpdateEventDataStoreResponse,

    -- * Response Lenses
    updateEventDataStoreResponse_advancedEventSelectors,
    updateEventDataStoreResponse_createdTimestamp,
    updateEventDataStoreResponse_eventDataStoreArn,
    updateEventDataStoreResponse_kmsKeyId,
    updateEventDataStoreResponse_multiRegionEnabled,
    updateEventDataStoreResponse_name,
    updateEventDataStoreResponse_organizationEnabled,
    updateEventDataStoreResponse_retentionPeriod,
    updateEventDataStoreResponse_status,
    updateEventDataStoreResponse_terminationProtectionEnabled,
    updateEventDataStoreResponse_updatedTimestamp,
    updateEventDataStoreResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventDataStore' smart constructor.
data UpdateEventDataStore = UpdateEventDataStore'
  { -- | The advanced event selectors used to select events for the event data
    -- store. You can configure up to five advanced event selectors for each
    -- event data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | Specifies the KMS key ID to use to encrypt the events delivered by
    -- CloudTrail. The value can be an alias name prefixed by @alias\/@, a
    -- fully specified ARN to an alias, a fully specified ARN to a key, or a
    -- globally unique identifier.
    --
    -- Disabling or deleting the KMS key, or removing CloudTrail permissions on
    -- the key, prevents CloudTrail from logging events to the event data
    -- store, and prevents users from querying the data in the event data store
    -- that was encrypted with the key. After you associate an event data store
    -- with a KMS key, the KMS key cannot be removed or changed. Before you
    -- disable or delete a KMS key that you are using with an event data store,
    -- delete or back up your event data store.
    --
    -- CloudTrail also supports KMS multi-Region keys. For more information
    -- about multi-Region keys, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
    -- in the /Key Management Service Developer Guide/.
    --
    -- Examples:
    --
    -- -   @alias\/MyAliasName@
    --
    -- -   @arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName@
    --
    -- -   @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    --
    -- -   @12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an event data store collects events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The event data store name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an event data store collects events logged for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
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
-- 'advancedEventSelectors', 'updateEventDataStore_advancedEventSelectors' - The advanced event selectors used to select events for the event data
-- store. You can configure up to five advanced event selectors for each
-- event data store.
--
-- 'kmsKeyId', 'updateEventDataStore_kmsKeyId' - Specifies the KMS key ID to use to encrypt the events delivered by
-- CloudTrail. The value can be an alias name prefixed by @alias\/@, a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- Disabling or deleting the KMS key, or removing CloudTrail permissions on
-- the key, prevents CloudTrail from logging events to the event data
-- store, and prevents users from querying the data in the event data store
-- that was encrypted with the key. After you associate an event data store
-- with a KMS key, the KMS key cannot be removed or changed. Before you
-- disable or delete a KMS key that you are using with an event data store,
-- delete or back up your event data store.
--
-- CloudTrail also supports KMS multi-Region keys. For more information
-- about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
-- in the /Key Management Service Developer Guide/.
--
-- Examples:
--
-- -   @alias\/MyAliasName@
--
-- -   @arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName@
--
-- -   @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   @12345678-1234-1234-1234-123456789012@
--
-- 'multiRegionEnabled', 'updateEventDataStore_multiRegionEnabled' - Specifies whether an event data store collects events from all regions,
-- or only from the region in which it was created.
--
-- 'name', 'updateEventDataStore_name' - The event data store name.
--
-- 'organizationEnabled', 'updateEventDataStore_organizationEnabled' - Specifies whether an event data store collects events logged for an
-- organization in Organizations.
--
-- 'retentionPeriod', 'updateEventDataStore_retentionPeriod' - The retention period, in days.
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
    { advancedEventSelectors =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing,
      eventDataStore = pEventDataStore_
    }

-- | The advanced event selectors used to select events for the event data
-- store. You can configure up to five advanced event selectors for each
-- event data store.
updateEventDataStore_advancedEventSelectors :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe [AdvancedEventSelector])
updateEventDataStore_advancedEventSelectors = Lens.lens (\UpdateEventDataStore' {advancedEventSelectors} -> advancedEventSelectors) (\s@UpdateEventDataStore' {} a -> s {advancedEventSelectors = a} :: UpdateEventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the KMS key ID to use to encrypt the events delivered by
-- CloudTrail. The value can be an alias name prefixed by @alias\/@, a
-- fully specified ARN to an alias, a fully specified ARN to a key, or a
-- globally unique identifier.
--
-- Disabling or deleting the KMS key, or removing CloudTrail permissions on
-- the key, prevents CloudTrail from logging events to the event data
-- store, and prevents users from querying the data in the event data store
-- that was encrypted with the key. After you associate an event data store
-- with a KMS key, the KMS key cannot be removed or changed. Before you
-- disable or delete a KMS key that you are using with an event data store,
-- delete or back up your event data store.
--
-- CloudTrail also supports KMS multi-Region keys. For more information
-- about multi-Region keys, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-overview.html Using multi-Region keys>
-- in the /Key Management Service Developer Guide/.
--
-- Examples:
--
-- -   @alias\/MyAliasName@
--
-- -   @arn:aws:kms:us-east-2:123456789012:alias\/MyAliasName@
--
-- -   @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   @12345678-1234-1234-1234-123456789012@
updateEventDataStore_kmsKeyId :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Text)
updateEventDataStore_kmsKeyId = Lens.lens (\UpdateEventDataStore' {kmsKeyId} -> kmsKeyId) (\s@UpdateEventDataStore' {} a -> s {kmsKeyId = a} :: UpdateEventDataStore)

-- | Specifies whether an event data store collects events from all regions,
-- or only from the region in which it was created.
updateEventDataStore_multiRegionEnabled :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Bool)
updateEventDataStore_multiRegionEnabled = Lens.lens (\UpdateEventDataStore' {multiRegionEnabled} -> multiRegionEnabled) (\s@UpdateEventDataStore' {} a -> s {multiRegionEnabled = a} :: UpdateEventDataStore)

-- | The event data store name.
updateEventDataStore_name :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Text)
updateEventDataStore_name = Lens.lens (\UpdateEventDataStore' {name} -> name) (\s@UpdateEventDataStore' {} a -> s {name = a} :: UpdateEventDataStore)

-- | Specifies whether an event data store collects events logged for an
-- organization in Organizations.
updateEventDataStore_organizationEnabled :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Bool)
updateEventDataStore_organizationEnabled = Lens.lens (\UpdateEventDataStore' {organizationEnabled} -> organizationEnabled) (\s@UpdateEventDataStore' {} a -> s {organizationEnabled = a} :: UpdateEventDataStore)

-- | The retention period, in days.
updateEventDataStore_retentionPeriod :: Lens.Lens' UpdateEventDataStore (Prelude.Maybe Prelude.Natural)
updateEventDataStore_retentionPeriod = Lens.lens (\UpdateEventDataStore' {retentionPeriod} -> retentionPeriod) (\s@UpdateEventDataStore' {} a -> s {retentionPeriod = a} :: UpdateEventDataStore)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEventDataStoreResponse'
            Prelude.<$> ( x Data..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "EventDataStoreArn")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "MultiRegionEnabled")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OrganizationEnabled")
            Prelude.<*> (x Data..?> "RetentionPeriod")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TerminationProtectionEnabled")
            Prelude.<*> (x Data..?> "UpdatedTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventDataStore where
  hashWithSalt _salt UpdateEventDataStore' {..} =
    _salt `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` multiRegionEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` organizationEnabled
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` terminationProtectionEnabled
      `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData UpdateEventDataStore where
  rnf UpdateEventDataStore' {..} =
    Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf eventDataStore

instance Data.ToHeaders UpdateEventDataStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.UpdateEventDataStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEventDataStore where
  toJSON UpdateEventDataStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdvancedEventSelectors" Data..=)
              Prelude.<$> advancedEventSelectors,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("MultiRegionEnabled" Data..=)
              Prelude.<$> multiRegionEnabled,
            ("Name" Data..=) Prelude.<$> name,
            ("OrganizationEnabled" Data..=)
              Prelude.<$> organizationEnabled,
            ("RetentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            ("TerminationProtectionEnabled" Data..=)
              Prelude.<$> terminationProtectionEnabled,
            Prelude.Just
              ("EventDataStore" Data..= eventDataStore)
          ]
      )

instance Data.ToPath UpdateEventDataStore where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEventDataStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventDataStoreResponse' smart constructor.
data UpdateEventDataStoreResponse = UpdateEventDataStoreResponse'
  { -- | The advanced event selectors that are applied to the event data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The timestamp that shows when an event data store was first created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the event data store.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the KMS key ID that encrypts the events delivered by
    -- CloudTrail. The value is a fully specified ARN to a KMS key in the
    -- following format.
    --
    -- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the event data store includes events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether an event data store is collecting logged events for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The retention period, in days.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
    -- | The status of an event data store. Values can be @ENABLED@ and
    -- @PENDING_DELETION@.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | Indicates whether termination protection is enabled for the event data
    -- store.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp that shows when the event data store was last updated.
    -- @UpdatedTimestamp@ is always either the same or newer than the time
    -- shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Data.POSIX,
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
-- 'advancedEventSelectors', 'updateEventDataStoreResponse_advancedEventSelectors' - The advanced event selectors that are applied to the event data store.
--
-- 'createdTimestamp', 'updateEventDataStoreResponse_createdTimestamp' - The timestamp that shows when an event data store was first created.
--
-- 'eventDataStoreArn', 'updateEventDataStoreResponse_eventDataStoreArn' - The ARN of the event data store.
--
-- 'kmsKeyId', 'updateEventDataStoreResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 'multiRegionEnabled', 'updateEventDataStoreResponse_multiRegionEnabled' - Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
--
-- 'name', 'updateEventDataStoreResponse_name' - The name of the event data store.
--
-- 'organizationEnabled', 'updateEventDataStoreResponse_organizationEnabled' - Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
--
-- 'retentionPeriod', 'updateEventDataStoreResponse_retentionPeriod' - The retention period, in days.
--
-- 'status', 'updateEventDataStoreResponse_status' - The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
--
-- 'terminationProtectionEnabled', 'updateEventDataStoreResponse_terminationProtectionEnabled' - Indicates whether termination protection is enabled for the event data
-- store.
--
-- 'updatedTimestamp', 'updateEventDataStoreResponse_updatedTimestamp' - The timestamp that shows when the event data store was last updated.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
--
-- 'httpStatus', 'updateEventDataStoreResponse_httpStatus' - The response's http status code.
newUpdateEventDataStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventDataStoreResponse
newUpdateEventDataStoreResponse pHttpStatus_ =
  UpdateEventDataStoreResponse'
    { advancedEventSelectors =
        Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      name = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      terminationProtectionEnabled =
        Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The advanced event selectors that are applied to the event data store.
updateEventDataStoreResponse_advancedEventSelectors :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe [AdvancedEventSelector])
updateEventDataStoreResponse_advancedEventSelectors = Lens.lens (\UpdateEventDataStoreResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@UpdateEventDataStoreResponse' {} a -> s {advancedEventSelectors = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that shows when an event data store was first created.
updateEventDataStoreResponse_createdTimestamp :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
updateEventDataStoreResponse_createdTimestamp = Lens.lens (\UpdateEventDataStoreResponse' {createdTimestamp} -> createdTimestamp) (\s@UpdateEventDataStoreResponse' {} a -> s {createdTimestamp = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the event data store.
updateEventDataStoreResponse_eventDataStoreArn :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
updateEventDataStoreResponse_eventDataStoreArn = Lens.lens (\UpdateEventDataStoreResponse' {eventDataStoreArn} -> eventDataStoreArn) (\s@UpdateEventDataStoreResponse' {} a -> s {eventDataStoreArn = a} :: UpdateEventDataStoreResponse)

-- | Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
updateEventDataStoreResponse_kmsKeyId :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
updateEventDataStoreResponse_kmsKeyId = Lens.lens (\UpdateEventDataStoreResponse' {kmsKeyId} -> kmsKeyId) (\s@UpdateEventDataStoreResponse' {} a -> s {kmsKeyId = a} :: UpdateEventDataStoreResponse)

-- | Indicates whether the event data store includes events from all regions,
-- or only from the region in which it was created.
updateEventDataStoreResponse_multiRegionEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_multiRegionEnabled = Lens.lens (\UpdateEventDataStoreResponse' {multiRegionEnabled} -> multiRegionEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {multiRegionEnabled = a} :: UpdateEventDataStoreResponse)

-- | The name of the event data store.
updateEventDataStoreResponse_name :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
updateEventDataStoreResponse_name = Lens.lens (\UpdateEventDataStoreResponse' {name} -> name) (\s@UpdateEventDataStoreResponse' {} a -> s {name = a} :: UpdateEventDataStoreResponse)

-- | Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
updateEventDataStoreResponse_organizationEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_organizationEnabled = Lens.lens (\UpdateEventDataStoreResponse' {organizationEnabled} -> organizationEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {organizationEnabled = a} :: UpdateEventDataStoreResponse)

-- | The retention period, in days.
updateEventDataStoreResponse_retentionPeriod :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Natural)
updateEventDataStoreResponse_retentionPeriod = Lens.lens (\UpdateEventDataStoreResponse' {retentionPeriod} -> retentionPeriod) (\s@UpdateEventDataStoreResponse' {} a -> s {retentionPeriod = a} :: UpdateEventDataStoreResponse)

-- | The status of an event data store. Values can be @ENABLED@ and
-- @PENDING_DELETION@.
updateEventDataStoreResponse_status :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe EventDataStoreStatus)
updateEventDataStoreResponse_status = Lens.lens (\UpdateEventDataStoreResponse' {status} -> status) (\s@UpdateEventDataStoreResponse' {} a -> s {status = a} :: UpdateEventDataStoreResponse)

-- | Indicates whether termination protection is enabled for the event data
-- store.
updateEventDataStoreResponse_terminationProtectionEnabled :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
updateEventDataStoreResponse_terminationProtectionEnabled = Lens.lens (\UpdateEventDataStoreResponse' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@UpdateEventDataStoreResponse' {} a -> s {terminationProtectionEnabled = a} :: UpdateEventDataStoreResponse)

-- | The timestamp that shows when the event data store was last updated.
-- @UpdatedTimestamp@ is always either the same or newer than the time
-- shown in @CreatedTimestamp@.
updateEventDataStoreResponse_updatedTimestamp :: Lens.Lens' UpdateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
updateEventDataStoreResponse_updatedTimestamp = Lens.lens (\UpdateEventDataStoreResponse' {updatedTimestamp} -> updatedTimestamp) (\s@UpdateEventDataStoreResponse' {} a -> s {updatedTimestamp = a} :: UpdateEventDataStoreResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateEventDataStoreResponse_httpStatus :: Lens.Lens' UpdateEventDataStoreResponse Prelude.Int
updateEventDataStoreResponse_httpStatus = Lens.lens (\UpdateEventDataStoreResponse' {httpStatus} -> httpStatus) (\s@UpdateEventDataStoreResponse' {} a -> s {httpStatus = a} :: UpdateEventDataStoreResponse)

instance Prelude.NFData UpdateEventDataStoreResponse where
  rnf UpdateEventDataStoreResponse' {..} =
    Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf eventDataStoreArn
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf httpStatus

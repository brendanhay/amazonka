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
-- Module      : Amazonka.CloudTrail.CreateEventDataStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event data store.
module Amazonka.CloudTrail.CreateEventDataStore
  ( -- * Creating a Request
    CreateEventDataStore (..),
    newCreateEventDataStore,

    -- * Request Lenses
    createEventDataStore_advancedEventSelectors,
    createEventDataStore_multiRegionEnabled,
    createEventDataStore_tagsList,
    createEventDataStore_retentionPeriod,
    createEventDataStore_kmsKeyId,
    createEventDataStore_organizationEnabled,
    createEventDataStore_terminationProtectionEnabled,
    createEventDataStore_name,

    -- * Destructuring the Response
    CreateEventDataStoreResponse (..),
    newCreateEventDataStoreResponse,

    -- * Response Lenses
    createEventDataStoreResponse_name,
    createEventDataStoreResponse_eventDataStoreArn,
    createEventDataStoreResponse_advancedEventSelectors,
    createEventDataStoreResponse_createdTimestamp,
    createEventDataStoreResponse_multiRegionEnabled,
    createEventDataStoreResponse_tagsList,
    createEventDataStoreResponse_updatedTimestamp,
    createEventDataStoreResponse_status,
    createEventDataStoreResponse_retentionPeriod,
    createEventDataStoreResponse_kmsKeyId,
    createEventDataStoreResponse_organizationEnabled,
    createEventDataStoreResponse_terminationProtectionEnabled,
    createEventDataStoreResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEventDataStore' smart constructor.
data CreateEventDataStore = CreateEventDataStore'
  { -- | The advanced event selectors to use to select the events for the data
    -- store. For more information about how to use advanced event selectors,
    -- see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html#creating-data-event-selectors-advanced Log events by using advanced event selectors>
    -- in the CloudTrail User Guide.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | Specifies whether the event data store includes events from all regions,
    -- or only from the region in which the event data store is created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    tagsList :: Prelude.Maybe [Tag],
    -- | The retention period of the event data store, in days. You can set a
    -- retention period of up to 2557 days, the equivalent of seven years.
    retentionPeriod :: Prelude.Maybe Prelude.Natural,
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
    -- | Specifies whether an event data store collects events logged for an
    -- organization in Organizations.
    organizationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether termination protection is enabled for the event data
    -- store. If termination protection is enabled, you cannot delete the event
    -- data store until termination protection is disabled.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the event data store.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventDataStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedEventSelectors', 'createEventDataStore_advancedEventSelectors' - The advanced event selectors to use to select the events for the data
-- store. For more information about how to use advanced event selectors,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html#creating-data-event-selectors-advanced Log events by using advanced event selectors>
-- in the CloudTrail User Guide.
--
-- 'multiRegionEnabled', 'createEventDataStore_multiRegionEnabled' - Specifies whether the event data store includes events from all regions,
-- or only from the region in which the event data store is created.
--
-- 'tagsList', 'createEventDataStore_tagsList' - Undocumented member.
--
-- 'retentionPeriod', 'createEventDataStore_retentionPeriod' - The retention period of the event data store, in days. You can set a
-- retention period of up to 2557 days, the equivalent of seven years.
--
-- 'kmsKeyId', 'createEventDataStore_kmsKeyId' - Specifies the KMS key ID to use to encrypt the events delivered by
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
-- 'organizationEnabled', 'createEventDataStore_organizationEnabled' - Specifies whether an event data store collects events logged for an
-- organization in Organizations.
--
-- 'terminationProtectionEnabled', 'createEventDataStore_terminationProtectionEnabled' - Specifies whether termination protection is enabled for the event data
-- store. If termination protection is enabled, you cannot delete the event
-- data store until termination protection is disabled.
--
-- 'name', 'createEventDataStore_name' - The name of the event data store.
newCreateEventDataStore ::
  -- | 'name'
  Prelude.Text ->
  CreateEventDataStore
newCreateEventDataStore pName_ =
  CreateEventDataStore'
    { advancedEventSelectors =
        Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      tagsList = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled = Prelude.Nothing,
      name = pName_
    }

-- | The advanced event selectors to use to select the events for the data
-- store. For more information about how to use advanced event selectors,
-- see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html#creating-data-event-selectors-advanced Log events by using advanced event selectors>
-- in the CloudTrail User Guide.
createEventDataStore_advancedEventSelectors :: Lens.Lens' CreateEventDataStore (Prelude.Maybe [AdvancedEventSelector])
createEventDataStore_advancedEventSelectors = Lens.lens (\CreateEventDataStore' {advancedEventSelectors} -> advancedEventSelectors) (\s@CreateEventDataStore' {} a -> s {advancedEventSelectors = a} :: CreateEventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the event data store includes events from all regions,
-- or only from the region in which the event data store is created.
createEventDataStore_multiRegionEnabled :: Lens.Lens' CreateEventDataStore (Prelude.Maybe Prelude.Bool)
createEventDataStore_multiRegionEnabled = Lens.lens (\CreateEventDataStore' {multiRegionEnabled} -> multiRegionEnabled) (\s@CreateEventDataStore' {} a -> s {multiRegionEnabled = a} :: CreateEventDataStore)

-- | Undocumented member.
createEventDataStore_tagsList :: Lens.Lens' CreateEventDataStore (Prelude.Maybe [Tag])
createEventDataStore_tagsList = Lens.lens (\CreateEventDataStore' {tagsList} -> tagsList) (\s@CreateEventDataStore' {} a -> s {tagsList = a} :: CreateEventDataStore) Prelude.. Lens.mapping Lens.coerced

-- | The retention period of the event data store, in days. You can set a
-- retention period of up to 2557 days, the equivalent of seven years.
createEventDataStore_retentionPeriod :: Lens.Lens' CreateEventDataStore (Prelude.Maybe Prelude.Natural)
createEventDataStore_retentionPeriod = Lens.lens (\CreateEventDataStore' {retentionPeriod} -> retentionPeriod) (\s@CreateEventDataStore' {} a -> s {retentionPeriod = a} :: CreateEventDataStore)

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
createEventDataStore_kmsKeyId :: Lens.Lens' CreateEventDataStore (Prelude.Maybe Prelude.Text)
createEventDataStore_kmsKeyId = Lens.lens (\CreateEventDataStore' {kmsKeyId} -> kmsKeyId) (\s@CreateEventDataStore' {} a -> s {kmsKeyId = a} :: CreateEventDataStore)

-- | Specifies whether an event data store collects events logged for an
-- organization in Organizations.
createEventDataStore_organizationEnabled :: Lens.Lens' CreateEventDataStore (Prelude.Maybe Prelude.Bool)
createEventDataStore_organizationEnabled = Lens.lens (\CreateEventDataStore' {organizationEnabled} -> organizationEnabled) (\s@CreateEventDataStore' {} a -> s {organizationEnabled = a} :: CreateEventDataStore)

-- | Specifies whether termination protection is enabled for the event data
-- store. If termination protection is enabled, you cannot delete the event
-- data store until termination protection is disabled.
createEventDataStore_terminationProtectionEnabled :: Lens.Lens' CreateEventDataStore (Prelude.Maybe Prelude.Bool)
createEventDataStore_terminationProtectionEnabled = Lens.lens (\CreateEventDataStore' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@CreateEventDataStore' {} a -> s {terminationProtectionEnabled = a} :: CreateEventDataStore)

-- | The name of the event data store.
createEventDataStore_name :: Lens.Lens' CreateEventDataStore Prelude.Text
createEventDataStore_name = Lens.lens (\CreateEventDataStore' {name} -> name) (\s@CreateEventDataStore' {} a -> s {name = a} :: CreateEventDataStore)

instance Core.AWSRequest CreateEventDataStore where
  type
    AWSResponse CreateEventDataStore =
      CreateEventDataStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventDataStoreResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "EventDataStoreArn")
            Prelude.<*> ( x Core..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "MultiRegionEnabled")
            Prelude.<*> (x Core..?> "TagsList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "UpdatedTimestamp")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "RetentionPeriod")
            Prelude.<*> (x Core..?> "KmsKeyId")
            Prelude.<*> (x Core..?> "OrganizationEnabled")
            Prelude.<*> (x Core..?> "TerminationProtectionEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventDataStore where
  hashWithSalt _salt CreateEventDataStore' {..} =
    _salt `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` multiRegionEnabled
      `Prelude.hashWithSalt` tagsList
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` organizationEnabled
      `Prelude.hashWithSalt` terminationProtectionEnabled
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateEventDataStore where
  rnf CreateEventDataStore' {..} =
    Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf tagsList
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateEventDataStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.CreateEventDataStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEventDataStore where
  toJSON CreateEventDataStore' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AdvancedEventSelectors" Core..=)
              Prelude.<$> advancedEventSelectors,
            ("MultiRegionEnabled" Core..=)
              Prelude.<$> multiRegionEnabled,
            ("TagsList" Core..=) Prelude.<$> tagsList,
            ("RetentionPeriod" Core..=)
              Prelude.<$> retentionPeriod,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("OrganizationEnabled" Core..=)
              Prelude.<$> organizationEnabled,
            ("TerminationProtectionEnabled" Core..=)
              Prelude.<$> terminationProtectionEnabled,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateEventDataStore where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEventDataStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventDataStoreResponse' smart constructor.
data CreateEventDataStoreResponse = CreateEventDataStoreResponse'
  { -- | The name of the event data store.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event data store.
    eventDataStoreArn :: Prelude.Maybe Prelude.Text,
    -- | The advanced event selectors that were used to select the events for the
    -- data store.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The timestamp that shows when the event data store was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the event data store collects events from all regions,
    -- or only from the region in which it was created.
    multiRegionEnabled :: Prelude.Maybe Prelude.Bool,
    tagsList :: Prelude.Maybe [Tag],
    -- | The timestamp that shows when an event data store was updated, if
    -- applicable. @UpdatedTimestamp@ is always either the same or newer than
    -- the time shown in @CreatedTimestamp@.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of event data store creation.
    status :: Prelude.Maybe EventDataStoreStatus,
    -- | The retention period of an event data store, in days.
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
    -- | Indicates whether termination protection is enabled for the event data
    -- store.
    terminationProtectionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventDataStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createEventDataStoreResponse_name' - The name of the event data store.
--
-- 'eventDataStoreArn', 'createEventDataStoreResponse_eventDataStoreArn' - The ARN of the event data store.
--
-- 'advancedEventSelectors', 'createEventDataStoreResponse_advancedEventSelectors' - The advanced event selectors that were used to select the events for the
-- data store.
--
-- 'createdTimestamp', 'createEventDataStoreResponse_createdTimestamp' - The timestamp that shows when the event data store was created.
--
-- 'multiRegionEnabled', 'createEventDataStoreResponse_multiRegionEnabled' - Indicates whether the event data store collects events from all regions,
-- or only from the region in which it was created.
--
-- 'tagsList', 'createEventDataStoreResponse_tagsList' - Undocumented member.
--
-- 'updatedTimestamp', 'createEventDataStoreResponse_updatedTimestamp' - The timestamp that shows when an event data store was updated, if
-- applicable. @UpdatedTimestamp@ is always either the same or newer than
-- the time shown in @CreatedTimestamp@.
--
-- 'status', 'createEventDataStoreResponse_status' - The status of event data store creation.
--
-- 'retentionPeriod', 'createEventDataStoreResponse_retentionPeriod' - The retention period of an event data store, in days.
--
-- 'kmsKeyId', 'createEventDataStoreResponse_kmsKeyId' - Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- 'organizationEnabled', 'createEventDataStoreResponse_organizationEnabled' - Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
--
-- 'terminationProtectionEnabled', 'createEventDataStoreResponse_terminationProtectionEnabled' - Indicates whether termination protection is enabled for the event data
-- store.
--
-- 'httpStatus', 'createEventDataStoreResponse_httpStatus' - The response's http status code.
newCreateEventDataStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventDataStoreResponse
newCreateEventDataStoreResponse pHttpStatus_ =
  CreateEventDataStoreResponse'
    { name =
        Prelude.Nothing,
      eventDataStoreArn = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      multiRegionEnabled = Prelude.Nothing,
      tagsList = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      organizationEnabled = Prelude.Nothing,
      terminationProtectionEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the event data store.
createEventDataStoreResponse_name :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
createEventDataStoreResponse_name = Lens.lens (\CreateEventDataStoreResponse' {name} -> name) (\s@CreateEventDataStoreResponse' {} a -> s {name = a} :: CreateEventDataStoreResponse)

-- | The ARN of the event data store.
createEventDataStoreResponse_eventDataStoreArn :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
createEventDataStoreResponse_eventDataStoreArn = Lens.lens (\CreateEventDataStoreResponse' {eventDataStoreArn} -> eventDataStoreArn) (\s@CreateEventDataStoreResponse' {} a -> s {eventDataStoreArn = a} :: CreateEventDataStoreResponse)

-- | The advanced event selectors that were used to select the events for the
-- data store.
createEventDataStoreResponse_advancedEventSelectors :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe [AdvancedEventSelector])
createEventDataStoreResponse_advancedEventSelectors = Lens.lens (\CreateEventDataStoreResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@CreateEventDataStoreResponse' {} a -> s {advancedEventSelectors = a} :: CreateEventDataStoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that shows when the event data store was created.
createEventDataStoreResponse_createdTimestamp :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
createEventDataStoreResponse_createdTimestamp = Lens.lens (\CreateEventDataStoreResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateEventDataStoreResponse' {} a -> s {createdTimestamp = a} :: CreateEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the event data store collects events from all regions,
-- or only from the region in which it was created.
createEventDataStoreResponse_multiRegionEnabled :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
createEventDataStoreResponse_multiRegionEnabled = Lens.lens (\CreateEventDataStoreResponse' {multiRegionEnabled} -> multiRegionEnabled) (\s@CreateEventDataStoreResponse' {} a -> s {multiRegionEnabled = a} :: CreateEventDataStoreResponse)

-- | Undocumented member.
createEventDataStoreResponse_tagsList :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe [Tag])
createEventDataStoreResponse_tagsList = Lens.lens (\CreateEventDataStoreResponse' {tagsList} -> tagsList) (\s@CreateEventDataStoreResponse' {} a -> s {tagsList = a} :: CreateEventDataStoreResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that shows when an event data store was updated, if
-- applicable. @UpdatedTimestamp@ is always either the same or newer than
-- the time shown in @CreatedTimestamp@.
createEventDataStoreResponse_updatedTimestamp :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.UTCTime)
createEventDataStoreResponse_updatedTimestamp = Lens.lens (\CreateEventDataStoreResponse' {updatedTimestamp} -> updatedTimestamp) (\s@CreateEventDataStoreResponse' {} a -> s {updatedTimestamp = a} :: CreateEventDataStoreResponse) Prelude.. Lens.mapping Core._Time

-- | The status of event data store creation.
createEventDataStoreResponse_status :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe EventDataStoreStatus)
createEventDataStoreResponse_status = Lens.lens (\CreateEventDataStoreResponse' {status} -> status) (\s@CreateEventDataStoreResponse' {} a -> s {status = a} :: CreateEventDataStoreResponse)

-- | The retention period of an event data store, in days.
createEventDataStoreResponse_retentionPeriod :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Natural)
createEventDataStoreResponse_retentionPeriod = Lens.lens (\CreateEventDataStoreResponse' {retentionPeriod} -> retentionPeriod) (\s@CreateEventDataStoreResponse' {} a -> s {retentionPeriod = a} :: CreateEventDataStoreResponse)

-- | Specifies the KMS key ID that encrypts the events delivered by
-- CloudTrail. The value is a fully specified ARN to a KMS key in the
-- following format.
--
-- @arn:aws:kms:us-east-2:123456789012:key\/12345678-1234-1234-1234-123456789012@
createEventDataStoreResponse_kmsKeyId :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Text)
createEventDataStoreResponse_kmsKeyId = Lens.lens (\CreateEventDataStoreResponse' {kmsKeyId} -> kmsKeyId) (\s@CreateEventDataStoreResponse' {} a -> s {kmsKeyId = a} :: CreateEventDataStoreResponse)

-- | Indicates whether an event data store is collecting logged events for an
-- organization in Organizations.
createEventDataStoreResponse_organizationEnabled :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
createEventDataStoreResponse_organizationEnabled = Lens.lens (\CreateEventDataStoreResponse' {organizationEnabled} -> organizationEnabled) (\s@CreateEventDataStoreResponse' {} a -> s {organizationEnabled = a} :: CreateEventDataStoreResponse)

-- | Indicates whether termination protection is enabled for the event data
-- store.
createEventDataStoreResponse_terminationProtectionEnabled :: Lens.Lens' CreateEventDataStoreResponse (Prelude.Maybe Prelude.Bool)
createEventDataStoreResponse_terminationProtectionEnabled = Lens.lens (\CreateEventDataStoreResponse' {terminationProtectionEnabled} -> terminationProtectionEnabled) (\s@CreateEventDataStoreResponse' {} a -> s {terminationProtectionEnabled = a} :: CreateEventDataStoreResponse)

-- | The response's http status code.
createEventDataStoreResponse_httpStatus :: Lens.Lens' CreateEventDataStoreResponse Prelude.Int
createEventDataStoreResponse_httpStatus = Lens.lens (\CreateEventDataStoreResponse' {httpStatus} -> httpStatus) (\s@CreateEventDataStoreResponse' {} a -> s {httpStatus = a} :: CreateEventDataStoreResponse)

instance Prelude.NFData CreateEventDataStoreResponse where
  rnf CreateEventDataStoreResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventDataStoreArn
      `Prelude.seq` Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf multiRegionEnabled
      `Prelude.seq` Prelude.rnf tagsList
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf organizationEnabled
      `Prelude.seq` Prelude.rnf terminationProtectionEnabled
      `Prelude.seq` Prelude.rnf httpStatus

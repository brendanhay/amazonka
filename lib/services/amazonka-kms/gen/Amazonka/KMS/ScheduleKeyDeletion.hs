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
-- Module      : Amazonka.KMS.ScheduleKeyDeletion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules the deletion of a KMS key. By default, KMS applies a waiting
-- period of 30 days, but you can specify a waiting period of 7-30 days.
-- When this operation is successful, the key state of the KMS key changes
-- to @PendingDeletion@ and the key can\'t be used in any cryptographic
-- operations. It remains in this state for the duration of the waiting
-- period. Before the waiting period ends, you can use CancelKeyDeletion to
-- cancel the deletion of the KMS key. After the waiting period ends, KMS
-- deletes the KMS key, its key material, and all KMS data associated with
-- it, including all aliases that refer to it.
--
-- Deleting a KMS key is a destructive and potentially dangerous operation.
-- When a KMS key is deleted, all data that was encrypted under the KMS key
-- is unrecoverable. (The only exception is a multi-Region replica key.) To
-- prevent the use of a KMS key without deleting it, use DisableKey.
--
-- You can schedule the deletion of a multi-Region primary key and its
-- replica keys at any time. However, KMS will not delete a multi-Region
-- primary key with existing replica keys. If you schedule the deletion of
-- a primary key with replicas, its key state changes to
-- @PendingReplicaDeletion@ and it cannot be replicated or used in
-- cryptographic operations. This status can continue indefinitely. When
-- the last of its replicas keys is deleted (not just scheduled), the key
-- state of the primary key changes to @PendingDeletion@ and its waiting
-- period (@PendingWindowInDays@) begins. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/multi-region-keys-delete.html Deleting multi-Region keys>
-- in the /Key Management Service Developer Guide/.
--
-- When KMS
-- <https://docs.aws.amazon.com/kms/latest/developerguide/delete-cmk-keystore.html deletes a KMS key from an CloudHSM key store>,
-- it makes a best effort to delete the associated key material from the
-- associated CloudHSM cluster. However, you might need to manually
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material>
-- from the cluster and its backups.
-- <https://docs.aws.amazon.com/kms/latest/developerguide/delete-xks-key.html Deleting a KMS key from an external key store>
-- has no effect on the associated external key. However, for both types of
-- custom key stores, deleting a KMS key is destructive and irreversible.
-- You cannot decrypt ciphertext encrypted under the KMS key by using only
-- its associated external key or CloudHSM key. Also, you cannot recreate a
-- KMS key in an external key store by creating a new KMS key with the same
-- key material.
--
-- For more information about scheduling a KMS key for deletion, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__: kms:ScheduleKeyDeletion (key policy)
--
-- __Related operations__
--
-- -   CancelKeyDeletion
--
-- -   DisableKey
module Amazonka.KMS.ScheduleKeyDeletion
  ( -- * Creating a Request
    ScheduleKeyDeletion (..),
    newScheduleKeyDeletion,

    -- * Request Lenses
    scheduleKeyDeletion_pendingWindowInDays,
    scheduleKeyDeletion_keyId,

    -- * Destructuring the Response
    ScheduleKeyDeletionResponse (..),
    newScheduleKeyDeletionResponse,

    -- * Response Lenses
    scheduleKeyDeletionResponse_deletionDate,
    scheduleKeyDeletionResponse_keyId,
    scheduleKeyDeletionResponse_keyState,
    scheduleKeyDeletionResponse_pendingWindowInDays,
    scheduleKeyDeletionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newScheduleKeyDeletion' smart constructor.
data ScheduleKeyDeletion = ScheduleKeyDeletion'
  { -- | The waiting period, specified in number of days. After the waiting
    -- period ends, KMS deletes the KMS key.
    --
    -- If the KMS key is a multi-Region primary key with replica keys, the
    -- waiting period begins when the last of its replica keys is deleted.
    -- Otherwise, the waiting period begins immediately.
    --
    -- This value is optional. If you include a value, it must be between 7 and
    -- 30, inclusive. If you do not include a value, it defaults to 30.
    pendingWindowInDays :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the KMS key to delete.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleKeyDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingWindowInDays', 'scheduleKeyDeletion_pendingWindowInDays' - The waiting period, specified in number of days. After the waiting
-- period ends, KMS deletes the KMS key.
--
-- If the KMS key is a multi-Region primary key with replica keys, the
-- waiting period begins when the last of its replica keys is deleted.
-- Otherwise, the waiting period begins immediately.
--
-- This value is optional. If you include a value, it must be between 7 and
-- 30, inclusive. If you do not include a value, it defaults to 30.
--
-- 'keyId', 'scheduleKeyDeletion_keyId' - The unique identifier of the KMS key to delete.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
newScheduleKeyDeletion ::
  -- | 'keyId'
  Prelude.Text ->
  ScheduleKeyDeletion
newScheduleKeyDeletion pKeyId_ =
  ScheduleKeyDeletion'
    { pendingWindowInDays =
        Prelude.Nothing,
      keyId = pKeyId_
    }

-- | The waiting period, specified in number of days. After the waiting
-- period ends, KMS deletes the KMS key.
--
-- If the KMS key is a multi-Region primary key with replica keys, the
-- waiting period begins when the last of its replica keys is deleted.
-- Otherwise, the waiting period begins immediately.
--
-- This value is optional. If you include a value, it must be between 7 and
-- 30, inclusive. If you do not include a value, it defaults to 30.
scheduleKeyDeletion_pendingWindowInDays :: Lens.Lens' ScheduleKeyDeletion (Prelude.Maybe Prelude.Natural)
scheduleKeyDeletion_pendingWindowInDays = Lens.lens (\ScheduleKeyDeletion' {pendingWindowInDays} -> pendingWindowInDays) (\s@ScheduleKeyDeletion' {} a -> s {pendingWindowInDays = a} :: ScheduleKeyDeletion)

-- | The unique identifier of the KMS key to delete.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
scheduleKeyDeletion_keyId :: Lens.Lens' ScheduleKeyDeletion Prelude.Text
scheduleKeyDeletion_keyId = Lens.lens (\ScheduleKeyDeletion' {keyId} -> keyId) (\s@ScheduleKeyDeletion' {} a -> s {keyId = a} :: ScheduleKeyDeletion)

instance Core.AWSRequest ScheduleKeyDeletion where
  type
    AWSResponse ScheduleKeyDeletion =
      ScheduleKeyDeletionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ScheduleKeyDeletionResponse'
            Prelude.<$> (x Data..?> "DeletionDate")
            Prelude.<*> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "KeyState")
            Prelude.<*> (x Data..?> "PendingWindowInDays")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ScheduleKeyDeletion where
  hashWithSalt _salt ScheduleKeyDeletion' {..} =
    _salt
      `Prelude.hashWithSalt` pendingWindowInDays
      `Prelude.hashWithSalt` keyId

instance Prelude.NFData ScheduleKeyDeletion where
  rnf ScheduleKeyDeletion' {..} =
    Prelude.rnf pendingWindowInDays
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToHeaders ScheduleKeyDeletion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.ScheduleKeyDeletion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ScheduleKeyDeletion where
  toJSON ScheduleKeyDeletion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PendingWindowInDays" Data..=)
              Prelude.<$> pendingWindowInDays,
            Prelude.Just ("KeyId" Data..= keyId)
          ]
      )

instance Data.ToPath ScheduleKeyDeletion where
  toPath = Prelude.const "/"

instance Data.ToQuery ScheduleKeyDeletion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newScheduleKeyDeletionResponse' smart constructor.
data ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse'
  { -- | The date and time after which KMS deletes the KMS key.
    --
    -- If the KMS key is a multi-Region primary key with replica keys, this
    -- field does not appear. The deletion date for the primary key isn\'t
    -- known until its last replica key is deleted.
    deletionDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the KMS key whose deletion is scheduled.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the KMS key.
    --
    -- For more information about how key state affects the use of a KMS key,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
    -- in the /Key Management Service Developer Guide/.
    keyState :: Prelude.Maybe KeyState,
    -- | The waiting period before the KMS key is deleted.
    --
    -- If the KMS key is a multi-Region primary key with replicas, the waiting
    -- period begins when the last of its replica keys is deleted. Otherwise,
    -- the waiting period begins immediately.
    pendingWindowInDays :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleKeyDeletionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionDate', 'scheduleKeyDeletionResponse_deletionDate' - The date and time after which KMS deletes the KMS key.
--
-- If the KMS key is a multi-Region primary key with replica keys, this
-- field does not appear. The deletion date for the primary key isn\'t
-- known until its last replica key is deleted.
--
-- 'keyId', 'scheduleKeyDeletionResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key whose deletion is scheduled.
--
-- 'keyState', 'scheduleKeyDeletionResponse_keyState' - The current status of the KMS key.
--
-- For more information about how key state affects the use of a KMS key,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- 'pendingWindowInDays', 'scheduleKeyDeletionResponse_pendingWindowInDays' - The waiting period before the KMS key is deleted.
--
-- If the KMS key is a multi-Region primary key with replicas, the waiting
-- period begins when the last of its replica keys is deleted. Otherwise,
-- the waiting period begins immediately.
--
-- 'httpStatus', 'scheduleKeyDeletionResponse_httpStatus' - The response's http status code.
newScheduleKeyDeletionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ScheduleKeyDeletionResponse
newScheduleKeyDeletionResponse pHttpStatus_ =
  ScheduleKeyDeletionResponse'
    { deletionDate =
        Prelude.Nothing,
      keyId = Prelude.Nothing,
      keyState = Prelude.Nothing,
      pendingWindowInDays = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time after which KMS deletes the KMS key.
--
-- If the KMS key is a multi-Region primary key with replica keys, this
-- field does not appear. The deletion date for the primary key isn\'t
-- known until its last replica key is deleted.
scheduleKeyDeletionResponse_deletionDate :: Lens.Lens' ScheduleKeyDeletionResponse (Prelude.Maybe Prelude.UTCTime)
scheduleKeyDeletionResponse_deletionDate = Lens.lens (\ScheduleKeyDeletionResponse' {deletionDate} -> deletionDate) (\s@ScheduleKeyDeletionResponse' {} a -> s {deletionDate = a} :: ScheduleKeyDeletionResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the KMS key whose deletion is scheduled.
scheduleKeyDeletionResponse_keyId :: Lens.Lens' ScheduleKeyDeletionResponse (Prelude.Maybe Prelude.Text)
scheduleKeyDeletionResponse_keyId = Lens.lens (\ScheduleKeyDeletionResponse' {keyId} -> keyId) (\s@ScheduleKeyDeletionResponse' {} a -> s {keyId = a} :: ScheduleKeyDeletionResponse)

-- | The current status of the KMS key.
--
-- For more information about how key state affects the use of a KMS key,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
scheduleKeyDeletionResponse_keyState :: Lens.Lens' ScheduleKeyDeletionResponse (Prelude.Maybe KeyState)
scheduleKeyDeletionResponse_keyState = Lens.lens (\ScheduleKeyDeletionResponse' {keyState} -> keyState) (\s@ScheduleKeyDeletionResponse' {} a -> s {keyState = a} :: ScheduleKeyDeletionResponse)

-- | The waiting period before the KMS key is deleted.
--
-- If the KMS key is a multi-Region primary key with replicas, the waiting
-- period begins when the last of its replica keys is deleted. Otherwise,
-- the waiting period begins immediately.
scheduleKeyDeletionResponse_pendingWindowInDays :: Lens.Lens' ScheduleKeyDeletionResponse (Prelude.Maybe Prelude.Natural)
scheduleKeyDeletionResponse_pendingWindowInDays = Lens.lens (\ScheduleKeyDeletionResponse' {pendingWindowInDays} -> pendingWindowInDays) (\s@ScheduleKeyDeletionResponse' {} a -> s {pendingWindowInDays = a} :: ScheduleKeyDeletionResponse)

-- | The response's http status code.
scheduleKeyDeletionResponse_httpStatus :: Lens.Lens' ScheduleKeyDeletionResponse Prelude.Int
scheduleKeyDeletionResponse_httpStatus = Lens.lens (\ScheduleKeyDeletionResponse' {httpStatus} -> httpStatus) (\s@ScheduleKeyDeletionResponse' {} a -> s {httpStatus = a} :: ScheduleKeyDeletionResponse)

instance Prelude.NFData ScheduleKeyDeletionResponse where
  rnf ScheduleKeyDeletionResponse' {..} =
    Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf keyState
      `Prelude.seq` Prelude.rnf pendingWindowInDays
      `Prelude.seq` Prelude.rnf httpStatus

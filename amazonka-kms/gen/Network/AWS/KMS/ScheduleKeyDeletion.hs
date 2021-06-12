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
-- Module      : Network.AWS.KMS.ScheduleKeyDeletion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules the deletion of a customer master key (CMK). You may provide a
-- waiting period, specified in days, before deletion occurs. If you do not
-- provide a waiting period, the default period of 30 days is used. When
-- this operation is successful, the key state of the CMK changes to
-- @PendingDeletion@. Before the waiting period ends, you can use
-- CancelKeyDeletion to cancel the deletion of the CMK. After the waiting
-- period ends, AWS KMS deletes the CMK and all AWS KMS data associated
-- with it, including all aliases that refer to it.
--
-- Deleting a CMK is a destructive and potentially dangerous operation.
-- When a CMK is deleted, all data that was encrypted under the CMK is
-- unrecoverable. To prevent the use of a CMK without deleting it, use
-- DisableKey.
--
-- If you schedule deletion of a CMK from a
-- <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store>,
-- when the waiting period expires, @ScheduleKeyDeletion@ deletes the CMK
-- from AWS KMS. Then AWS KMS makes a best effort to delete the key
-- material from the associated AWS CloudHSM cluster. However, you might
-- need to manually
-- <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material>
-- from the cluster and its backups.
--
-- For more information about scheduling a CMK for deletion, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys>
-- in the /AWS Key Management Service Developer Guide/.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:ScheduleKeyDeletion>
-- (key policy)
--
-- __Related operations__
--
-- -   CancelKeyDeletion
--
-- -   DisableKey
module Network.AWS.KMS.ScheduleKeyDeletion
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
    scheduleKeyDeletionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newScheduleKeyDeletion' smart constructor.
data ScheduleKeyDeletion = ScheduleKeyDeletion'
  { -- | The waiting period, specified in number of days. After the waiting
    -- period ends, AWS KMS deletes the customer master key (CMK).
    --
    -- This value is optional. If you include a value, it must be between 7 and
    -- 30, inclusive. If you do not include a value, it defaults to 30.
    pendingWindowInDays :: Core.Maybe Core.Natural,
    -- | The unique identifier of the customer master key (CMK) to delete.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleKeyDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingWindowInDays', 'scheduleKeyDeletion_pendingWindowInDays' - The waiting period, specified in number of days. After the waiting
-- period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and
-- 30, inclusive. If you do not include a value, it defaults to 30.
--
-- 'keyId', 'scheduleKeyDeletion_keyId' - The unique identifier of the customer master key (CMK) to delete.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
newScheduleKeyDeletion ::
  -- | 'keyId'
  Core.Text ->
  ScheduleKeyDeletion
newScheduleKeyDeletion pKeyId_ =
  ScheduleKeyDeletion'
    { pendingWindowInDays =
        Core.Nothing,
      keyId = pKeyId_
    }

-- | The waiting period, specified in number of days. After the waiting
-- period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and
-- 30, inclusive. If you do not include a value, it defaults to 30.
scheduleKeyDeletion_pendingWindowInDays :: Lens.Lens' ScheduleKeyDeletion (Core.Maybe Core.Natural)
scheduleKeyDeletion_pendingWindowInDays = Lens.lens (\ScheduleKeyDeletion' {pendingWindowInDays} -> pendingWindowInDays) (\s@ScheduleKeyDeletion' {} a -> s {pendingWindowInDays = a} :: ScheduleKeyDeletion)

-- | The unique identifier of the customer master key (CMK) to delete.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
scheduleKeyDeletion_keyId :: Lens.Lens' ScheduleKeyDeletion Core.Text
scheduleKeyDeletion_keyId = Lens.lens (\ScheduleKeyDeletion' {keyId} -> keyId) (\s@ScheduleKeyDeletion' {} a -> s {keyId = a} :: ScheduleKeyDeletion)

instance Core.AWSRequest ScheduleKeyDeletion where
  type
    AWSResponse ScheduleKeyDeletion =
      ScheduleKeyDeletionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ScheduleKeyDeletionResponse'
            Core.<$> (x Core..?> "DeletionDate")
            Core.<*> (x Core..?> "KeyId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ScheduleKeyDeletion

instance Core.NFData ScheduleKeyDeletion

instance Core.ToHeaders ScheduleKeyDeletion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TrentService.ScheduleKeyDeletion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ScheduleKeyDeletion where
  toJSON ScheduleKeyDeletion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PendingWindowInDays" Core..=)
              Core.<$> pendingWindowInDays,
            Core.Just ("KeyId" Core..= keyId)
          ]
      )

instance Core.ToPath ScheduleKeyDeletion where
  toPath = Core.const "/"

instance Core.ToQuery ScheduleKeyDeletion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newScheduleKeyDeletionResponse' smart constructor.
data ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse'
  { -- | The date and time after which AWS KMS deletes the customer master key
    -- (CMK).
    deletionDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
    -- of the CMK whose deletion is scheduled.
    keyId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleKeyDeletionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionDate', 'scheduleKeyDeletionResponse_deletionDate' - The date and time after which AWS KMS deletes the customer master key
-- (CMK).
--
-- 'keyId', 'scheduleKeyDeletionResponse_keyId' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK whose deletion is scheduled.
--
-- 'httpStatus', 'scheduleKeyDeletionResponse_httpStatus' - The response's http status code.
newScheduleKeyDeletionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ScheduleKeyDeletionResponse
newScheduleKeyDeletionResponse pHttpStatus_ =
  ScheduleKeyDeletionResponse'
    { deletionDate =
        Core.Nothing,
      keyId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time after which AWS KMS deletes the customer master key
-- (CMK).
scheduleKeyDeletionResponse_deletionDate :: Lens.Lens' ScheduleKeyDeletionResponse (Core.Maybe Core.UTCTime)
scheduleKeyDeletionResponse_deletionDate = Lens.lens (\ScheduleKeyDeletionResponse' {deletionDate} -> deletionDate) (\s@ScheduleKeyDeletionResponse' {} a -> s {deletionDate = a} :: ScheduleKeyDeletionResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN>)
-- of the CMK whose deletion is scheduled.
scheduleKeyDeletionResponse_keyId :: Lens.Lens' ScheduleKeyDeletionResponse (Core.Maybe Core.Text)
scheduleKeyDeletionResponse_keyId = Lens.lens (\ScheduleKeyDeletionResponse' {keyId} -> keyId) (\s@ScheduleKeyDeletionResponse' {} a -> s {keyId = a} :: ScheduleKeyDeletionResponse)

-- | The response's http status code.
scheduleKeyDeletionResponse_httpStatus :: Lens.Lens' ScheduleKeyDeletionResponse Core.Int
scheduleKeyDeletionResponse_httpStatus = Lens.lens (\ScheduleKeyDeletionResponse' {httpStatus} -> httpStatus) (\s@ScheduleKeyDeletionResponse' {} a -> s {httpStatus = a} :: ScheduleKeyDeletionResponse)

instance Core.NFData ScheduleKeyDeletionResponse

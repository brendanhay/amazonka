{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.ScheduleKeyDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules the deletion of a customer master key (CMK). You may provide a waiting period, specified in days, before deletion occurs. If you do not provide a waiting period, the default period of 30 days is used. When this operation is successful, the key state of the CMK changes to @PendingDeletion@ . Before the waiting period ends, you can use 'CancelKeyDeletion' to cancel the deletion of the CMK. After the waiting period ends, AWS KMS deletes the CMK and all AWS KMS data associated with it, including all aliases that refer to it.
--
-- /Important:/ Deleting a CMK is a destructive and potentially dangerous operation. When a CMK is deleted, all data that was encrypted under the CMK is unrecoverable. To prevent the use of a CMK without deleting it, use 'DisableKey' .
-- If you schedule deletion of a CMK from a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> , when the waiting period expires, @ScheduleKeyDeletion@ deletes the CMK from AWS KMS. Then AWS KMS makes a best effort to delete the key material from the associated AWS CloudHSM cluster. However, you might need to manually <https://docs.aws.amazon.com/kms/latest/developerguide/fix-keystore.html#fix-keystore-orphaned-key delete the orphaned key material> from the cluster and its backups.
-- You cannot perform this operation on a CMK in a different AWS account.
-- For more information about scheduling a CMK for deletion, see <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.ScheduleKeyDeletion
  ( -- * Creating a request
    ScheduleKeyDeletion (..),
    mkScheduleKeyDeletion,

    -- ** Request lenses
    skdKeyId,
    skdPendingWindowInDays,

    -- * Destructuring the response
    ScheduleKeyDeletionResponse (..),
    mkScheduleKeyDeletionResponse,

    -- ** Response lenses
    skdrsKeyId,
    skdrsDeletionDate,
    skdrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkScheduleKeyDeletion' smart constructor.
data ScheduleKeyDeletion = ScheduleKeyDeletion'
  { -- | The unique identifier of the customer master key (CMK) to delete.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Lude.Text,
    -- | The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
    --
    -- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
    pendingWindowInDays :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleKeyDeletion' with the minimum fields required to make a request.
--
-- * 'keyId' - The unique identifier of the customer master key (CMK) to delete.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
-- * 'pendingWindowInDays' - The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
mkScheduleKeyDeletion ::
  -- | 'keyId'
  Lude.Text ->
  ScheduleKeyDeletion
mkScheduleKeyDeletion pKeyId_ =
  ScheduleKeyDeletion'
    { keyId = pKeyId_,
      pendingWindowInDays = Lude.Nothing
    }

-- | The unique identifier of the customer master key (CMK) to delete.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdKeyId :: Lens.Lens' ScheduleKeyDeletion Lude.Text
skdKeyId = Lens.lens (keyId :: ScheduleKeyDeletion -> Lude.Text) (\s a -> s {keyId = a} :: ScheduleKeyDeletion)
{-# DEPRECATED skdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The waiting period, specified in number of days. After the waiting period ends, AWS KMS deletes the customer master key (CMK).
--
-- This value is optional. If you include a value, it must be between 7 and 30, inclusive. If you do not include a value, it defaults to 30.
--
-- /Note:/ Consider using 'pendingWindowInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdPendingWindowInDays :: Lens.Lens' ScheduleKeyDeletion (Lude.Maybe Lude.Natural)
skdPendingWindowInDays = Lens.lens (pendingWindowInDays :: ScheduleKeyDeletion -> Lude.Maybe Lude.Natural) (\s a -> s {pendingWindowInDays = a} :: ScheduleKeyDeletion)
{-# DEPRECATED skdPendingWindowInDays "Use generic-lens or generic-optics with 'pendingWindowInDays' instead." #-}

instance Lude.AWSRequest ScheduleKeyDeletion where
  type Rs ScheduleKeyDeletion = ScheduleKeyDeletionResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ScheduleKeyDeletionResponse'
            Lude.<$> (x Lude..?> "KeyId")
            Lude.<*> (x Lude..?> "DeletionDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ScheduleKeyDeletion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.ScheduleKeyDeletion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ScheduleKeyDeletion where
  toJSON ScheduleKeyDeletion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            ("PendingWindowInDays" Lude..=) Lude.<$> pendingWindowInDays
          ]
      )

instance Lude.ToPath ScheduleKeyDeletion where
  toPath = Lude.const "/"

instance Lude.ToQuery ScheduleKeyDeletion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkScheduleKeyDeletionResponse' smart constructor.
data ScheduleKeyDeletionResponse = ScheduleKeyDeletionResponse'
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is scheduled.
    keyId :: Lude.Maybe Lude.Text,
    -- | The date and time after which AWS KMS deletes the customer master key (CMK).
    deletionDate :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleKeyDeletionResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is scheduled.
-- * 'deletionDate' - The date and time after which AWS KMS deletes the customer master key (CMK).
-- * 'responseStatus' - The response status code.
mkScheduleKeyDeletionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ScheduleKeyDeletionResponse
mkScheduleKeyDeletionResponse pResponseStatus_ =
  ScheduleKeyDeletionResponse'
    { keyId = Lude.Nothing,
      deletionDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is scheduled.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrsKeyId :: Lens.Lens' ScheduleKeyDeletionResponse (Lude.Maybe Lude.Text)
skdrsKeyId = Lens.lens (keyId :: ScheduleKeyDeletionResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: ScheduleKeyDeletionResponse)
{-# DEPRECATED skdrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The date and time after which AWS KMS deletes the customer master key (CMK).
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrsDeletionDate :: Lens.Lens' ScheduleKeyDeletionResponse (Lude.Maybe Lude.Timestamp)
skdrsDeletionDate = Lens.lens (deletionDate :: ScheduleKeyDeletionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {deletionDate = a} :: ScheduleKeyDeletionResponse)
{-# DEPRECATED skdrsDeletionDate "Use generic-lens or generic-optics with 'deletionDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skdrsResponseStatus :: Lens.Lens' ScheduleKeyDeletionResponse Lude.Int
skdrsResponseStatus = Lens.lens (responseStatus :: ScheduleKeyDeletionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ScheduleKeyDeletionResponse)
{-# DEPRECATED skdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.CancelKeyDeletion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the deletion of a customer master key (CMK). When this operation succeeds, the key state of the CMK is @Disabled@ . To enable the CMK, use 'EnableKey' . You cannot perform this operation on a CMK in a different AWS account.
--
-- For more information about scheduling and canceling deletion of a CMK, see <https://docs.aws.amazon.com/kms/latest/developerguide/deleting-keys.html Deleting Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.CancelKeyDeletion
  ( -- * Creating a request
    CancelKeyDeletion (..),
    mkCancelKeyDeletion,

    -- ** Request lenses
    ckdKeyId,

    -- * Destructuring the response
    CancelKeyDeletionResponse (..),
    mkCancelKeyDeletionResponse,

    -- ** Response lenses
    ckdrsKeyId,
    ckdrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelKeyDeletion' smart constructor.
newtype CancelKeyDeletion = CancelKeyDeletion' {keyId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelKeyDeletion' with the minimum fields required to make a request.
--
-- * 'keyId' - The unique identifier for the customer master key (CMK) for which to cancel deletion.
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
mkCancelKeyDeletion ::
  -- | 'keyId'
  Lude.Text ->
  CancelKeyDeletion
mkCancelKeyDeletion pKeyId_ = CancelKeyDeletion' {keyId = pKeyId_}

-- | The unique identifier for the customer master key (CMK) for which to cancel deletion.
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
ckdKeyId :: Lens.Lens' CancelKeyDeletion Lude.Text
ckdKeyId = Lens.lens (keyId :: CancelKeyDeletion -> Lude.Text) (\s a -> s {keyId = a} :: CancelKeyDeletion)
{-# DEPRECATED ckdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest CancelKeyDeletion where
  type Rs CancelKeyDeletion = CancelKeyDeletionResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelKeyDeletionResponse'
            Lude.<$> (x Lude..?> "KeyId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelKeyDeletion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.CancelKeyDeletion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelKeyDeletion where
  toJSON CancelKeyDeletion' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath CancelKeyDeletion where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelKeyDeletion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelKeyDeletionResponse' smart constructor.
data CancelKeyDeletionResponse = CancelKeyDeletionResponse'
  { keyId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelKeyDeletionResponse' with the minimum fields required to make a request.
--
-- * 'keyId' - The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is canceled.
-- * 'responseStatus' - The response status code.
mkCancelKeyDeletionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelKeyDeletionResponse
mkCancelKeyDeletionResponse pResponseStatus_ =
  CancelKeyDeletionResponse'
    { keyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-ARN key ARN> ) of the CMK whose deletion is canceled.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckdrsKeyId :: Lens.Lens' CancelKeyDeletionResponse (Lude.Maybe Lude.Text)
ckdrsKeyId = Lens.lens (keyId :: CancelKeyDeletionResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: CancelKeyDeletionResponse)
{-# DEPRECATED ckdrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckdrsResponseStatus :: Lens.Lens' CancelKeyDeletionResponse Lude.Int
ckdrsResponseStatus = Lens.lens (responseStatus :: CancelKeyDeletionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelKeyDeletionResponse)
{-# DEPRECATED ckdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

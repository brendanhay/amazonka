{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> for the specified symmetric customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . You cannot perform this operation on a CMK in a different AWS account.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DisableKeyRotation
  ( -- * Creating a request
    DisableKeyRotation (..),
    mkDisableKeyRotation,

    -- ** Request lenses
    dkrKeyId,

    -- * Destructuring the response
    DisableKeyRotationResponse (..),
    mkDisableKeyRotationResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableKeyRotation' smart constructor.
newtype DisableKeyRotation = DisableKeyRotation'
  { keyId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableKeyRotation' with the minimum fields required to make a request.
--
-- * 'keyId' - Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
mkDisableKeyRotation ::
  -- | 'keyId'
  Lude.Text ->
  DisableKeyRotation
mkDisableKeyRotation pKeyId_ = DisableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
dkrKeyId :: Lens.Lens' DisableKeyRotation Lude.Text
dkrKeyId = Lens.lens (keyId :: DisableKeyRotation -> Lude.Text) (\s a -> s {keyId = a} :: DisableKeyRotation)
{-# DEPRECATED dkrKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest DisableKeyRotation where
  type Rs DisableKeyRotation = DisableKeyRotationResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull DisableKeyRotationResponse'

instance Lude.ToHeaders DisableKeyRotation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DisableKeyRotation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableKeyRotation where
  toJSON DisableKeyRotation' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath DisableKeyRotation where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableKeyRotation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse = DisableKeyRotationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableKeyRotationResponse' with the minimum fields required to make a request.
mkDisableKeyRotationResponse ::
  DisableKeyRotationResponse
mkDisableKeyRotationResponse = DisableKeyRotationResponse'

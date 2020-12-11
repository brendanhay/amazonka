{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.EnableKeyRotation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> for the specified symmetric customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.EnableKeyRotation
  ( -- * Creating a request
    EnableKeyRotation (..),
    mkEnableKeyRotation,

    -- ** Request lenses
    ekrKeyId,

    -- * Destructuring the response
    EnableKeyRotationResponse (..),
    mkEnableKeyRotationResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableKeyRotation' smart constructor.
newtype EnableKeyRotation = EnableKeyRotation' {keyId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableKeyRotation' with the minimum fields required to make a request.
--
-- * 'keyId' - Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
mkEnableKeyRotation ::
  -- | 'keyId'
  Lude.Text ->
  EnableKeyRotation
mkEnableKeyRotation pKeyId_ = EnableKeyRotation' {keyId = pKeyId_}

-- | Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> .
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
ekrKeyId :: Lens.Lens' EnableKeyRotation Lude.Text
ekrKeyId = Lens.lens (keyId :: EnableKeyRotation -> Lude.Text) (\s a -> s {keyId = a} :: EnableKeyRotation)
{-# DEPRECATED ekrKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest EnableKeyRotation where
  type Rs EnableKeyRotation = EnableKeyRotationResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull EnableKeyRotationResponse'

instance Lude.ToHeaders EnableKeyRotation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.EnableKeyRotation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableKeyRotation where
  toJSON EnableKeyRotation' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath EnableKeyRotation where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableKeyRotation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableKeyRotationResponse' smart constructor.
data EnableKeyRotationResponse = EnableKeyRotationResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableKeyRotationResponse' with the minimum fields required to make a request.
mkEnableKeyRotationResponse ::
  EnableKeyRotationResponse
mkEnableKeyRotationResponse = EnableKeyRotationResponse'

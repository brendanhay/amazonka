{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> is enabled for the specified customer master key (CMK).
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . The key rotation status for these CMKs is always @false@ .
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
--     * Disabled: The key rotation status does not change when you disable a CMK. However, while the CMK is disabled, AWS KMS does not rotate the backing key.
--
--
--     * Pending deletion: While a CMK is pending deletion, its key rotation status is @false@ and AWS KMS does not rotate the backing key. If you cancel the deletion, the original key rotation status is restored.
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
module Network.AWS.KMS.GetKeyRotationStatus
  ( -- * Creating a request
    GetKeyRotationStatus (..),
    mkGetKeyRotationStatus,

    -- ** Request lenses
    gkrsKeyId,

    -- * Destructuring the response
    GetKeyRotationStatusResponse (..),
    mkGetKeyRotationStatusResponse,

    -- ** Response lenses
    gkrsrsKeyRotationEnabled,
    gkrsrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyRotationStatus' smart constructor.
newtype GetKeyRotationStatus = GetKeyRotationStatus'
  { -- | A unique identifier for the customer master key (CMK).
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
    -- For example:
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
    keyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyRotationStatus' with the minimum fields required to make a request.
--
-- * 'keyId' - A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
mkGetKeyRotationStatus ::
  -- | 'keyId'
  Lude.Text ->
  GetKeyRotationStatus
mkGetKeyRotationStatus pKeyId_ =
  GetKeyRotationStatus' {keyId = pKeyId_}

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN.
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
gkrsKeyId :: Lens.Lens' GetKeyRotationStatus Lude.Text
gkrsKeyId = Lens.lens (keyId :: GetKeyRotationStatus -> Lude.Text) (\s a -> s {keyId = a} :: GetKeyRotationStatus)
{-# DEPRECATED gkrsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest GetKeyRotationStatus where
  type Rs GetKeyRotationStatus = GetKeyRotationStatusResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetKeyRotationStatusResponse'
            Lude.<$> (x Lude..?> "KeyRotationEnabled")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyRotationStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GetKeyRotationStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetKeyRotationStatus where
  toJSON GetKeyRotationStatus' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath GetKeyRotationStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetKeyRotationStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { -- | A Boolean value that specifies whether key rotation is enabled.
    keyRotationEnabled :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyRotationStatusResponse' with the minimum fields required to make a request.
--
-- * 'keyRotationEnabled' - A Boolean value that specifies whether key rotation is enabled.
-- * 'responseStatus' - The response status code.
mkGetKeyRotationStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyRotationStatusResponse
mkGetKeyRotationStatusResponse pResponseStatus_ =
  GetKeyRotationStatusResponse'
    { keyRotationEnabled = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A Boolean value that specifies whether key rotation is enabled.
--
-- /Note:/ Consider using 'keyRotationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkrsrsKeyRotationEnabled :: Lens.Lens' GetKeyRotationStatusResponse (Lude.Maybe Lude.Bool)
gkrsrsKeyRotationEnabled = Lens.lens (keyRotationEnabled :: GetKeyRotationStatusResponse -> Lude.Maybe Lude.Bool) (\s a -> s {keyRotationEnabled = a} :: GetKeyRotationStatusResponse)
{-# DEPRECATED gkrsrsKeyRotationEnabled "Use generic-lens or generic-optics with 'keyRotationEnabled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkrsrsResponseStatus :: Lens.Lens' GetKeyRotationStatusResponse Lude.Int
gkrsrsResponseStatus = Lens.lens (responseStatus :: GetKeyRotationStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyRotationStatusResponse)
{-# DEPRECATED gkrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

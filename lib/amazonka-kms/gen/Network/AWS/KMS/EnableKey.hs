{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.EnableKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the key state of a customer master key (CMK) to enabled. This allows you to use the CMK for <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#cryptographic-operations cryptographic operations> . You cannot perform this operation on a CMK in a different AWS account.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.EnableKey
  ( -- * Creating a request
    EnableKey (..),
    mkEnableKey,

    -- ** Request lenses
    ekKeyId,

    -- * Destructuring the response
    EnableKeyResponse (..),
    mkEnableKeyResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableKey' smart constructor.
newtype EnableKey = EnableKey'
  { -- | A unique identifier for the customer master key (CMK).
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
    keyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableKey' with the minimum fields required to make a request.
--
-- * 'keyId' - A unique identifier for the customer master key (CMK).
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
mkEnableKey ::
  -- | 'keyId'
  Lude.Text ->
  EnableKey
mkEnableKey pKeyId_ = EnableKey' {keyId = pKeyId_}

-- | A unique identifier for the customer master key (CMK).
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
ekKeyId :: Lens.Lens' EnableKey Lude.Text
ekKeyId = Lens.lens (keyId :: EnableKey -> Lude.Text) (\s a -> s {keyId = a} :: EnableKey)
{-# DEPRECATED ekKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest EnableKey where
  type Rs EnableKey = EnableKeyResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull EnableKeyResponse'

instance Lude.ToHeaders EnableKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.EnableKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableKey where
  toJSON EnableKey' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath EnableKey where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableKeyResponse' smart constructor.
data EnableKeyResponse = EnableKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableKeyResponse' with the minimum fields required to make a request.
mkEnableKeyResponse ::
  EnableKeyResponse
mkEnableKeyResponse = EnableKeyResponse'

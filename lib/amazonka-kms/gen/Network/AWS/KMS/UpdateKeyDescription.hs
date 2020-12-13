{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateKeyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a customer master key (CMK). To see the description of a CMK, use 'DescribeKey' .
--
-- You cannot perform this operation on a CMK in a different AWS account.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.UpdateKeyDescription
  ( -- * Creating a request
    UpdateKeyDescription (..),
    mkUpdateKeyDescription,

    -- ** Request lenses
    ukdKeyId,
    ukdDescription,

    -- * Destructuring the response
    UpdateKeyDescriptionResponse (..),
    mkUpdateKeyDescriptionResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateKeyDescription' smart constructor.
data UpdateKeyDescription = UpdateKeyDescription'
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
    keyId :: Lude.Text,
    -- | New description for the CMK.
    description :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateKeyDescription' with the minimum fields required to make a request.
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
-- * 'description' - New description for the CMK.
mkUpdateKeyDescription ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  UpdateKeyDescription
mkUpdateKeyDescription pKeyId_ pDescription_ =
  UpdateKeyDescription'
    { keyId = pKeyId_,
      description = pDescription_
    }

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
ukdKeyId :: Lens.Lens' UpdateKeyDescription Lude.Text
ukdKeyId = Lens.lens (keyId :: UpdateKeyDescription -> Lude.Text) (\s a -> s {keyId = a} :: UpdateKeyDescription)
{-# DEPRECATED ukdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | New description for the CMK.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukdDescription :: Lens.Lens' UpdateKeyDescription Lude.Text
ukdDescription = Lens.lens (description :: UpdateKeyDescription -> Lude.Text) (\s a -> s {description = a} :: UpdateKeyDescription)
{-# DEPRECATED ukdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateKeyDescription where
  type Rs UpdateKeyDescription = UpdateKeyDescriptionResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull UpdateKeyDescriptionResponse'

instance Lude.ToHeaders UpdateKeyDescription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.UpdateKeyDescription" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateKeyDescription where
  toJSON UpdateKeyDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("Description" Lude..= description)
          ]
      )

instance Lude.ToPath UpdateKeyDescription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateKeyDescription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateKeyDescriptionResponse' smart constructor.
data UpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateKeyDescriptionResponse' with the minimum fields required to make a request.
mkUpdateKeyDescriptionResponse ::
  UpdateKeyDescriptionResponse
mkUpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse'

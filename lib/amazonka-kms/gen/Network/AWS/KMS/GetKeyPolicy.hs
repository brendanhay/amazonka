{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key policy attached to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
module Network.AWS.KMS.GetKeyPolicy
  ( -- * Creating a request
    GetKeyPolicy (..),
    mkGetKeyPolicy,

    -- ** Request lenses
    gkpKeyId,
    gkpPolicyName,

    -- * Destructuring the response
    GetKeyPolicyResponse (..),
    mkGetKeyPolicyResponse,

    -- ** Response lenses
    gkprsPolicy,
    gkprsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
  { keyId :: Lude.Text,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyPolicy' with the minimum fields required to make a request.
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
-- * 'policyName' - Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
mkGetKeyPolicy ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  GetKeyPolicy
mkGetKeyPolicy pKeyId_ pPolicyName_ =
  GetKeyPolicy' {keyId = pKeyId_, policyName = pPolicyName_}

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
gkpKeyId :: Lens.Lens' GetKeyPolicy Lude.Text
gkpKeyId = Lens.lens (keyId :: GetKeyPolicy -> Lude.Text) (\s a -> s {keyId = a} :: GetKeyPolicy)
{-# DEPRECATED gkpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpPolicyName :: Lens.Lens' GetKeyPolicy Lude.Text
gkpPolicyName = Lens.lens (policyName :: GetKeyPolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetKeyPolicy)
{-# DEPRECATED gkpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest GetKeyPolicy where
  type Rs GetKeyPolicy = GetKeyPolicyResponse
  request = Req.postJSON kmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetKeyPolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.GetKeyPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetKeyPolicy where
  toJSON GetKeyPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("PolicyName" Lude..= policyName)
          ]
      )

instance Lude.ToPath GetKeyPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetKeyPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
  { policy ::
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

-- | Creates a value of 'GetKeyPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A key policy document in JSON format.
-- * 'responseStatus' - The response status code.
mkGetKeyPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyPolicyResponse
mkGetKeyPolicyResponse pResponseStatus_ =
  GetKeyPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A key policy document in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprsPolicy :: Lens.Lens' GetKeyPolicyResponse (Lude.Maybe Lude.Text)
gkprsPolicy = Lens.lens (policy :: GetKeyPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetKeyPolicyResponse)
{-# DEPRECATED gkprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprsResponseStatus :: Lens.Lens' GetKeyPolicyResponse Lude.Int
gkprsResponseStatus = Lens.lens (responseStatus :: GetKeyPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyPolicyResponse)
{-# DEPRECATED gkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

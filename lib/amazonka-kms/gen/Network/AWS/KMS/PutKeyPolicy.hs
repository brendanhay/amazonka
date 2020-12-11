{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.PutKeyPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a key policy to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
--
-- For more information about key policies, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key Policies> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.PutKeyPolicy
  ( -- * Creating a request
    PutKeyPolicy (..),
    mkPutKeyPolicy,

    -- ** Request lenses
    pkpBypassPolicyLockoutSafetyCheck,
    pkpKeyId,
    pkpPolicyName,
    pkpPolicy,

    -- * Destructuring the response
    PutKeyPolicyResponse (..),
    mkPutKeyPolicyResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutKeyPolicy' smart constructor.
data PutKeyPolicy = PutKeyPolicy'
  { bypassPolicyLockoutSafetyCheck ::
      Lude.Maybe Lude.Bool,
    keyId :: Lude.Text,
    policyName :: Lude.Text,
    policy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutKeyPolicy' with the minimum fields required to make a request.
--
-- * 'bypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety check.
--
-- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
-- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ .
-- Use this parameter only when you intend to prevent the principal that is making the request from making a subsequent @PutKeyPolicy@ request on the CMK.
-- The default value is false.
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
-- * 'policy' - The key policy to attach to the CMK.
--
-- The key policy must meet the following criteria:
--
--     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @PutKeyPolicy@ request to make a subsequent @PutKeyPolicy@ request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /AWS Key Management Service Developer Guide/ .
--
--
--     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ .
--
--
-- The key policy cannot exceed 32 kilobytes (32768 bytes). For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource Quotas> in the /AWS Key Management Service Developer Guide/ .
-- * 'policyName' - The name of the key policy. The only valid value is @default@ .
mkPutKeyPolicy ::
  -- | 'keyId'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutKeyPolicy
mkPutKeyPolicy pKeyId_ pPolicyName_ pPolicy_ =
  PutKeyPolicy'
    { bypassPolicyLockoutSafetyCheck = Lude.Nothing,
      keyId = pKeyId_,
      policyName = pPolicyName_,
      policy = pPolicy_
    }

-- | A flag to indicate whether to bypass the key policy lockout safety check.
--
-- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
-- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ .
-- Use this parameter only when you intend to prevent the principal that is making the request from making a subsequent @PutKeyPolicy@ request on the CMK.
-- The default value is false.
--
-- /Note:/ Consider using 'bypassPolicyLockoutSafetyCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkpBypassPolicyLockoutSafetyCheck :: Lens.Lens' PutKeyPolicy (Lude.Maybe Lude.Bool)
pkpBypassPolicyLockoutSafetyCheck = Lens.lens (bypassPolicyLockoutSafetyCheck :: PutKeyPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutKeyPolicy)
{-# DEPRECATED pkpBypassPolicyLockoutSafetyCheck "Use generic-lens or generic-optics with 'bypassPolicyLockoutSafetyCheck' instead." #-}

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
pkpKeyId :: Lens.Lens' PutKeyPolicy Lude.Text
pkpKeyId = Lens.lens (keyId :: PutKeyPolicy -> Lude.Text) (\s a -> s {keyId = a} :: PutKeyPolicy)
{-# DEPRECATED pkpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The name of the key policy. The only valid value is @default@ .
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkpPolicyName :: Lens.Lens' PutKeyPolicy Lude.Text
pkpPolicyName = Lens.lens (policyName :: PutKeyPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutKeyPolicy)
{-# DEPRECATED pkpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The key policy to attach to the CMK.
--
-- The key policy must meet the following criteria:
--
--     * If you don't set @BypassPolicyLockoutSafetyCheck@ to true, the key policy must allow the principal that is making the @PutKeyPolicy@ request to make a subsequent @PutKeyPolicy@ request on the CMK. This reduces the risk that the CMK becomes unmanageable. For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section of the /AWS Key Management Service Developer Guide/ .
--
--
--     * Each statement in the key policy must contain one or more principals. The principals in the key policy must exist and be visible to AWS KMS. When you create a new AWS principal (for example, an IAM user or role), you might need to enforce a delay before including the new principal in a key policy because the new principal might not be immediately visible to AWS KMS. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible> in the /AWS Identity and Access Management User Guide/ .
--
--
-- The key policy cannot exceed 32 kilobytes (32768 bytes). For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource Quotas> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkpPolicy :: Lens.Lens' PutKeyPolicy Lude.Text
pkpPolicy = Lens.lens (policy :: PutKeyPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutKeyPolicy)
{-# DEPRECATED pkpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutKeyPolicy where
  type Rs PutKeyPolicy = PutKeyPolicyResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull PutKeyPolicyResponse'

instance Lude.ToHeaders PutKeyPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.PutKeyPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutKeyPolicy where
  toJSON PutKeyPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Lude..=)
              Lude.<$> bypassPolicyLockoutSafetyCheck,
            Lude.Just ("KeyId" Lude..= keyId),
            Lude.Just ("PolicyName" Lude..= policyName),
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutKeyPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutKeyPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutKeyPolicyResponse' smart constructor.
data PutKeyPolicyResponse = PutKeyPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutKeyPolicyResponse' with the minimum fields required to make a request.
mkPutKeyPolicyResponse ::
  PutKeyPolicyResponse
mkPutKeyPolicyResponse = PutKeyPolicyResponse'

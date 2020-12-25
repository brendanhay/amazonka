{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    pkpKeyId,
    pkpPolicyName,
    pkpPolicy,
    pkpBypassPolicyLockoutSafetyCheck,

    -- * Destructuring the response
    PutKeyPolicyResponse (..),
    mkPutKeyPolicyResponse,
  )
where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutKeyPolicy' smart constructor.
data PutKeyPolicy = PutKeyPolicy'
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
    keyId :: Types.KeyId,
    -- | The name of the key policy. The only valid value is @default@ .
    policyName :: Types.PolicyName,
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
    policy :: Types.PolicyType,
    -- | A flag to indicate whether to bypass the key policy lockout safety check.
    --
    -- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
    -- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ .
    -- Use this parameter only when you intend to prevent the principal that is making the request from making a subsequent @PutKeyPolicy@ request on the CMK.
    -- The default value is false.
    bypassPolicyLockoutSafetyCheck :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutKeyPolicy' value with any optional fields omitted.
mkPutKeyPolicy ::
  -- | 'keyId'
  Types.KeyId ->
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policy'
  Types.PolicyType ->
  PutKeyPolicy
mkPutKeyPolicy keyId policyName policy =
  PutKeyPolicy'
    { keyId,
      policyName,
      policy,
      bypassPolicyLockoutSafetyCheck = Core.Nothing
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
pkpKeyId :: Lens.Lens' PutKeyPolicy Types.KeyId
pkpKeyId = Lens.field @"keyId"
{-# DEPRECATED pkpKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The name of the key policy. The only valid value is @default@ .
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkpPolicyName :: Lens.Lens' PutKeyPolicy Types.PolicyName
pkpPolicyName = Lens.field @"policyName"
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
pkpPolicy :: Lens.Lens' PutKeyPolicy Types.PolicyType
pkpPolicy = Lens.field @"policy"
{-# DEPRECATED pkpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A flag to indicate whether to bypass the key policy lockout safety check.
--
-- /Important:/ Setting this value to true increases the risk that the CMK becomes unmanageable. Do not set this value to true indiscriminately.
-- For more information, refer to the scenario in the <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy> section in the /AWS Key Management Service Developer Guide/ .
-- Use this parameter only when you intend to prevent the principal that is making the request from making a subsequent @PutKeyPolicy@ request on the CMK.
-- The default value is false.
--
-- /Note:/ Consider using 'bypassPolicyLockoutSafetyCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pkpBypassPolicyLockoutSafetyCheck :: Lens.Lens' PutKeyPolicy (Core.Maybe Core.Bool)
pkpBypassPolicyLockoutSafetyCheck = Lens.field @"bypassPolicyLockoutSafetyCheck"
{-# DEPRECATED pkpBypassPolicyLockoutSafetyCheck "Use generic-lens or generic-optics with 'bypassPolicyLockoutSafetyCheck' instead." #-}

instance Core.FromJSON PutKeyPolicy where
  toJSON PutKeyPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("PolicyName" Core..= policyName),
            Core.Just ("Policy" Core..= policy),
            ("BypassPolicyLockoutSafetyCheck" Core..=)
              Core.<$> bypassPolicyLockoutSafetyCheck
          ]
      )

instance Core.AWSRequest PutKeyPolicy where
  type Rs PutKeyPolicy = PutKeyPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "TrentService.PutKeyPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutKeyPolicyResponse'

-- | /See:/ 'mkPutKeyPolicyResponse' smart constructor.
data PutKeyPolicyResponse = PutKeyPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutKeyPolicyResponse' value with any optional fields omitted.
mkPutKeyPolicyResponse ::
  PutKeyPolicyResponse
mkPutKeyPolicyResponse = PutKeyPolicyResponse'

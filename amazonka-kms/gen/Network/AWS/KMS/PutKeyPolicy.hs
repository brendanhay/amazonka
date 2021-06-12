{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.PutKeyPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a key policy to the specified customer master key (CMK).
--
-- For more information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key Policies>
-- in the /AWS Key Management Service Developer Guide/. For help writing
-- and formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //IAM User Guide// . For examples of adding a key policy in
-- multiple programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-key-policies.html#put-policy Setting a key policy>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:PutKeyPolicy>
-- (key policy)
--
-- __Related operations__: GetKeyPolicy
module Network.AWS.KMS.PutKeyPolicy
  ( -- * Creating a Request
    PutKeyPolicy (..),
    newPutKeyPolicy,

    -- * Request Lenses
    putKeyPolicy_bypassPolicyLockoutSafetyCheck,
    putKeyPolicy_keyId,
    putKeyPolicy_policyName,
    putKeyPolicy_policy,

    -- * Destructuring the Response
    PutKeyPolicyResponse (..),
    newPutKeyPolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutKeyPolicy' smart constructor.
data PutKeyPolicy = PutKeyPolicy'
  { -- | A flag to indicate whether to bypass the key policy lockout safety
    -- check.
    --
    -- Setting this value to true increases the risk that the CMK becomes
    -- unmanageable. Do not set this value to true indiscriminately.
    --
    -- For more information, refer to the scenario in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    -- section in the /AWS Key Management Service Developer Guide/.
    --
    -- Use this parameter only when you intend to prevent the principal that is
    -- making the request from making a subsequent @PutKeyPolicy@ request on
    -- the CMK.
    --
    -- The default value is false.
    bypassPolicyLockoutSafetyCheck :: Core.Maybe Core.Bool,
    -- | A unique identifier for the customer master key (CMK).
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Core.Text,
    -- | The name of the key policy. The only valid value is @default@.
    policyName :: Core.Text,
    -- | The key policy to attach to the CMK.
    --
    -- The key policy must meet the following criteria:
    --
    -- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
    --     policy must allow the principal that is making the @PutKeyPolicy@
    --     request to make a subsequent @PutKeyPolicy@ request on the CMK. This
    --     reduces the risk that the CMK becomes unmanageable. For more
    --     information, refer to the scenario in the
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    --     section of the /AWS Key Management Service Developer Guide/.
    --
    -- -   Each statement in the key policy must contain one or more
    --     principals. The principals in the key policy must exist and be
    --     visible to AWS KMS. When you create a new AWS principal (for
    --     example, an IAM user or role), you might need to enforce a delay
    --     before including the new principal in a key policy because the new
    --     principal might not be immediately visible to AWS KMS. For more
    --     information, see
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
    --     in the /AWS Identity and Access Management User Guide/.
    --
    -- The key policy cannot exceed 32 kilobytes (32768 bytes). For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource Quotas>
    -- in the /AWS Key Management Service Developer Guide/.
    policy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutKeyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassPolicyLockoutSafetyCheck', 'putKeyPolicy_bypassPolicyLockoutSafetyCheck' - A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the CMK becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /AWS Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the CMK.
--
-- The default value is false.
--
-- 'keyId', 'putKeyPolicy_keyId' - A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
--
-- 'policyName', 'putKeyPolicy_policyName' - The name of the key policy. The only valid value is @default@.
--
-- 'policy', 'putKeyPolicy_policy' - The key policy to attach to the CMK.
--
-- The key policy must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must allow the principal that is making the @PutKeyPolicy@
--     request to make a subsequent @PutKeyPolicy@ request on the CMK. This
--     reduces the risk that the CMK becomes unmanageable. For more
--     information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the /AWS Key Management Service Developer Guide/.
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to AWS KMS. When you create a new AWS principal (for
--     example, an IAM user or role), you might need to enforce a delay
--     before including the new principal in a key policy because the new
--     principal might not be immediately visible to AWS KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /AWS Identity and Access Management User Guide/.
--
-- The key policy cannot exceed 32 kilobytes (32768 bytes). For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource Quotas>
-- in the /AWS Key Management Service Developer Guide/.
newPutKeyPolicy ::
  -- | 'keyId'
  Core.Text ->
  -- | 'policyName'
  Core.Text ->
  -- | 'policy'
  Core.Text ->
  PutKeyPolicy
newPutKeyPolicy pKeyId_ pPolicyName_ pPolicy_ =
  PutKeyPolicy'
    { bypassPolicyLockoutSafetyCheck =
        Core.Nothing,
      keyId = pKeyId_,
      policyName = pPolicyName_,
      policy = pPolicy_
    }

-- | A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the CMK becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /AWS Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the CMK.
--
-- The default value is false.
putKeyPolicy_bypassPolicyLockoutSafetyCheck :: Lens.Lens' PutKeyPolicy (Core.Maybe Core.Bool)
putKeyPolicy_bypassPolicyLockoutSafetyCheck = Lens.lens (\PutKeyPolicy' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@PutKeyPolicy' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutKeyPolicy)

-- | A unique identifier for the customer master key (CMK).
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
putKeyPolicy_keyId :: Lens.Lens' PutKeyPolicy Core.Text
putKeyPolicy_keyId = Lens.lens (\PutKeyPolicy' {keyId} -> keyId) (\s@PutKeyPolicy' {} a -> s {keyId = a} :: PutKeyPolicy)

-- | The name of the key policy. The only valid value is @default@.
putKeyPolicy_policyName :: Lens.Lens' PutKeyPolicy Core.Text
putKeyPolicy_policyName = Lens.lens (\PutKeyPolicy' {policyName} -> policyName) (\s@PutKeyPolicy' {} a -> s {policyName = a} :: PutKeyPolicy)

-- | The key policy to attach to the CMK.
--
-- The key policy must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must allow the principal that is making the @PutKeyPolicy@
--     request to make a subsequent @PutKeyPolicy@ request on the CMK. This
--     reduces the risk that the CMK becomes unmanageable. For more
--     information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the /AWS Key Management Service Developer Guide/.
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to AWS KMS. When you create a new AWS principal (for
--     example, an IAM user or role), you might need to enforce a delay
--     before including the new principal in a key policy because the new
--     principal might not be immediately visible to AWS KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /AWS Identity and Access Management User Guide/.
--
-- The key policy cannot exceed 32 kilobytes (32768 bytes). For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource Quotas>
-- in the /AWS Key Management Service Developer Guide/.
putKeyPolicy_policy :: Lens.Lens' PutKeyPolicy Core.Text
putKeyPolicy_policy = Lens.lens (\PutKeyPolicy' {policy} -> policy) (\s@PutKeyPolicy' {} a -> s {policy = a} :: PutKeyPolicy)

instance Core.AWSRequest PutKeyPolicy where
  type AWSResponse PutKeyPolicy = PutKeyPolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull PutKeyPolicyResponse'

instance Core.Hashable PutKeyPolicy

instance Core.NFData PutKeyPolicy

instance Core.ToHeaders PutKeyPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.PutKeyPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutKeyPolicy where
  toJSON PutKeyPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Core..=)
              Core.<$> bypassPolicyLockoutSafetyCheck,
            Core.Just ("KeyId" Core..= keyId),
            Core.Just ("PolicyName" Core..= policyName),
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.ToPath PutKeyPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutKeyPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutKeyPolicyResponse' smart constructor.
data PutKeyPolicyResponse = PutKeyPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutKeyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutKeyPolicyResponse ::
  PutKeyPolicyResponse
newPutKeyPolicyResponse = PutKeyPolicyResponse'

instance Core.NFData PutKeyPolicyResponse

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
-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key policy attached to the specified customer master key (CMK).
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetKeyPolicy>
-- (key policy)
--
-- __Related operations__: PutKeyPolicy
module Network.AWS.KMS.GetKeyPolicy
  ( -- * Creating a Request
    GetKeyPolicy (..),
    newGetKeyPolicy,

    -- * Request Lenses
    getKeyPolicy_keyId,
    getKeyPolicy_policyName,

    -- * Destructuring the Response
    GetKeyPolicyResponse (..),
    newGetKeyPolicyResponse,

    -- * Response Lenses
    getKeyPolicyResponse_policy,
    getKeyPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
  { -- | A unique identifier for the customer master key (CMK).
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
    -- | Specifies the name of the key policy. The only valid name is @default@.
    -- To get the names of key policies, use ListKeyPolicies.
    policyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetKeyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'getKeyPolicy_keyId' - A unique identifier for the customer master key (CMK).
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
-- 'policyName', 'getKeyPolicy_policyName' - Specifies the name of the key policy. The only valid name is @default@.
-- To get the names of key policies, use ListKeyPolicies.
newGetKeyPolicy ::
  -- | 'keyId'
  Core.Text ->
  -- | 'policyName'
  Core.Text ->
  GetKeyPolicy
newGetKeyPolicy pKeyId_ pPolicyName_ =
  GetKeyPolicy'
    { keyId = pKeyId_,
      policyName = pPolicyName_
    }

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
getKeyPolicy_keyId :: Lens.Lens' GetKeyPolicy Core.Text
getKeyPolicy_keyId = Lens.lens (\GetKeyPolicy' {keyId} -> keyId) (\s@GetKeyPolicy' {} a -> s {keyId = a} :: GetKeyPolicy)

-- | Specifies the name of the key policy. The only valid name is @default@.
-- To get the names of key policies, use ListKeyPolicies.
getKeyPolicy_policyName :: Lens.Lens' GetKeyPolicy Core.Text
getKeyPolicy_policyName = Lens.lens (\GetKeyPolicy' {policyName} -> policyName) (\s@GetKeyPolicy' {} a -> s {policyName = a} :: GetKeyPolicy)

instance Core.AWSRequest GetKeyPolicy where
  type AWSResponse GetKeyPolicy = GetKeyPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPolicyResponse'
            Core.<$> (x Core..?> "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetKeyPolicy

instance Core.NFData GetKeyPolicy

instance Core.ToHeaders GetKeyPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.GetKeyPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetKeyPolicy where
  toJSON GetKeyPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyId" Core..= keyId),
            Core.Just ("PolicyName" Core..= policyName)
          ]
      )

instance Core.ToPath GetKeyPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetKeyPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
  { -- | A key policy document in JSON format.
    policy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetKeyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getKeyPolicyResponse_policy' - A key policy document in JSON format.
--
-- 'httpStatus', 'getKeyPolicyResponse_httpStatus' - The response's http status code.
newGetKeyPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetKeyPolicyResponse
newGetKeyPolicyResponse pHttpStatus_ =
  GetKeyPolicyResponse'
    { policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A key policy document in JSON format.
getKeyPolicyResponse_policy :: Lens.Lens' GetKeyPolicyResponse (Core.Maybe Core.Text)
getKeyPolicyResponse_policy = Lens.lens (\GetKeyPolicyResponse' {policy} -> policy) (\s@GetKeyPolicyResponse' {} a -> s {policy = a} :: GetKeyPolicyResponse)

-- | The response's http status code.
getKeyPolicyResponse_httpStatus :: Lens.Lens' GetKeyPolicyResponse Core.Int
getKeyPolicyResponse_httpStatus = Lens.lens (\GetKeyPolicyResponse' {httpStatus} -> httpStatus) (\s@GetKeyPolicyResponse' {} a -> s {httpStatus = a} :: GetKeyPolicyResponse)

instance Core.NFData GetKeyPolicyResponse

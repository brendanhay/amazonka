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
-- Module      : Amazonka.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key policy attached to the specified KMS key.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GetKeyPolicy>
-- (key policy)
--
-- __Related operations__: PutKeyPolicy
module Amazonka.KMS.GetKeyPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
  { -- | Gets the key policy for the specified KMS key.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text,
    -- | Specifies the name of the key policy. The only valid name is @default@.
    -- To get the names of key policies, use ListKeyPolicies.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'getKeyPolicy_keyId' - Gets the key policy for the specified KMS key.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
--
-- 'policyName', 'getKeyPolicy_policyName' - Specifies the name of the key policy. The only valid name is @default@.
-- To get the names of key policies, use ListKeyPolicies.
newGetKeyPolicy ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  GetKeyPolicy
newGetKeyPolicy pKeyId_ pPolicyName_ =
  GetKeyPolicy'
    { keyId = pKeyId_,
      policyName = pPolicyName_
    }

-- | Gets the key policy for the specified KMS key.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
getKeyPolicy_keyId :: Lens.Lens' GetKeyPolicy Prelude.Text
getKeyPolicy_keyId = Lens.lens (\GetKeyPolicy' {keyId} -> keyId) (\s@GetKeyPolicy' {} a -> s {keyId = a} :: GetKeyPolicy)

-- | Specifies the name of the key policy. The only valid name is @default@.
-- To get the names of key policies, use ListKeyPolicies.
getKeyPolicy_policyName :: Lens.Lens' GetKeyPolicy Prelude.Text
getKeyPolicy_policyName = Lens.lens (\GetKeyPolicy' {policyName} -> policyName) (\s@GetKeyPolicy' {} a -> s {policyName = a} :: GetKeyPolicy)

instance Core.AWSRequest GetKeyPolicy where
  type AWSResponse GetKeyPolicy = GetKeyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyPolicy where
  hashWithSalt _salt GetKeyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData GetKeyPolicy where
  rnf GetKeyPolicy' {..} =
    Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders GetKeyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.GetKeyPolicy" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKeyPolicy where
  toJSON GetKeyPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("PolicyName" Data..= policyName)
          ]
      )

instance Data.ToPath GetKeyPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKeyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
  { -- | A key policy document in JSON format.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetKeyPolicyResponse
newGetKeyPolicyResponse pHttpStatus_ =
  GetKeyPolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A key policy document in JSON format.
getKeyPolicyResponse_policy :: Lens.Lens' GetKeyPolicyResponse (Prelude.Maybe Prelude.Text)
getKeyPolicyResponse_policy = Lens.lens (\GetKeyPolicyResponse' {policy} -> policy) (\s@GetKeyPolicyResponse' {} a -> s {policy = a} :: GetKeyPolicyResponse)

-- | The response's http status code.
getKeyPolicyResponse_httpStatus :: Lens.Lens' GetKeyPolicyResponse Prelude.Int
getKeyPolicyResponse_httpStatus = Lens.lens (\GetKeyPolicyResponse' {httpStatus} -> httpStatus) (\s@GetKeyPolicyResponse' {} a -> s {httpStatus = a} :: GetKeyPolicyResponse)

instance Prelude.NFData GetKeyPolicyResponse where
  rnf GetKeyPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus

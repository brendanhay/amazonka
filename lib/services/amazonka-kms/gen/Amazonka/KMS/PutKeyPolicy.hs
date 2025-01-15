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
-- Module      : Amazonka.KMS.PutKeyPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a key policy to the specified KMS key.
--
-- For more information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key Policies>
-- in the /Key Management Service Developer Guide/. For help writing and
-- formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //Identity and Access Management User Guide// . For examples of
-- adding a key policy in multiple programming languages, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/programming-key-policies.html#put-policy Setting a key policy>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:PutKeyPolicy>
-- (key policy)
--
-- __Related operations__: GetKeyPolicy
module Amazonka.KMS.PutKeyPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutKeyPolicy' smart constructor.
data PutKeyPolicy = PutKeyPolicy'
  { -- | A flag to indicate whether to bypass the key policy lockout safety
    -- check.
    --
    -- Setting this value to true increases the risk that the KMS key becomes
    -- unmanageable. Do not set this value to true indiscriminately.
    --
    -- For more information, refer to the scenario in the
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    -- section in the /Key Management Service Developer Guide/.
    --
    -- Use this parameter only when you intend to prevent the principal that is
    -- making the request from making a subsequent @PutKeyPolicy@ request on
    -- the KMS key.
    --
    -- The default value is false.
    bypassPolicyLockoutSafetyCheck :: Prelude.Maybe Prelude.Bool,
    -- | Sets the key policy on the specified KMS key.
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
    -- | The name of the key policy. The only valid value is @default@.
    policyName :: Prelude.Text,
    -- | The key policy to attach to the KMS key.
    --
    -- The key policy must meet the following criteria:
    --
    -- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
    --     policy must allow the principal that is making the @PutKeyPolicy@
    --     request to make a subsequent @PutKeyPolicy@ request on the KMS key.
    --     This reduces the risk that the KMS key becomes unmanageable. For
    --     more information, refer to the scenario in the
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
    --     section of the /Key Management Service Developer Guide/.
    --
    -- -   Each statement in the key policy must contain one or more
    --     principals. The principals in the key policy must exist and be
    --     visible to KMS. When you create a new Amazon Web Services principal
    --     (for example, an IAM user or role), you might need to enforce a
    --     delay before including the new principal in a key policy because the
    --     new principal might not be immediately visible to KMS. For more
    --     information, see
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
    --     in the /Amazon Web Services Identity and Access Management User
    --     Guide/.
    --
    -- A key policy document can include only the following characters:
    --
    -- -   Printable ASCII characters from the space character (@\\u0020@)
    --     through the end of the ASCII character range.
    --
    -- -   Printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@).
    --
    -- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
    --     (@\\u000D@) special characters
    --
    -- For information about key policies, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
    -- in the /Key Management Service Developer Guide/.For help writing and
    -- formatting a JSON policy document, see the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
    -- in the //Identity and Access Management User Guide// .
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the KMS key.
--
-- The default value is false.
--
-- 'keyId', 'putKeyPolicy_keyId' - Sets the key policy on the specified KMS key.
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
-- 'policyName', 'putKeyPolicy_policyName' - The name of the key policy. The only valid value is @default@.
--
-- 'policy', 'putKeyPolicy_policy' - The key policy to attach to the KMS key.
--
-- The key policy must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must allow the principal that is making the @PutKeyPolicy@
--     request to make a subsequent @PutKeyPolicy@ request on the KMS key.
--     This reduces the risk that the KMS key becomes unmanageable. For
--     more information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the /Key Management Service Developer Guide/.
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to KMS. When you create a new Amazon Web Services principal
--     (for example, an IAM user or role), you might need to enforce a
--     delay before including the new principal in a key policy because the
--     new principal might not be immediately visible to KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /Amazon Web Services Identity and Access Management User
--     Guide/.
--
-- A key policy document can include only the following characters:
--
-- -   Printable ASCII characters from the space character (@\\u0020@)
--     through the end of the ASCII character range.
--
-- -   Printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@).
--
-- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
--     (@\\u000D@) special characters
--
-- For information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
-- in the /Key Management Service Developer Guide/.For help writing and
-- formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //Identity and Access Management User Guide// .
newPutKeyPolicy ::
  -- | 'keyId'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutKeyPolicy
newPutKeyPolicy pKeyId_ pPolicyName_ pPolicy_ =
  PutKeyPolicy'
    { bypassPolicyLockoutSafetyCheck =
        Prelude.Nothing,
      keyId = pKeyId_,
      policyName = pPolicyName_,
      policy = pPolicy_
    }

-- | A flag to indicate whether to bypass the key policy lockout safety
-- check.
--
-- Setting this value to true increases the risk that the KMS key becomes
-- unmanageable. Do not set this value to true indiscriminately.
--
-- For more information, refer to the scenario in the
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
-- section in the /Key Management Service Developer Guide/.
--
-- Use this parameter only when you intend to prevent the principal that is
-- making the request from making a subsequent @PutKeyPolicy@ request on
-- the KMS key.
--
-- The default value is false.
putKeyPolicy_bypassPolicyLockoutSafetyCheck :: Lens.Lens' PutKeyPolicy (Prelude.Maybe Prelude.Bool)
putKeyPolicy_bypassPolicyLockoutSafetyCheck = Lens.lens (\PutKeyPolicy' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@PutKeyPolicy' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutKeyPolicy)

-- | Sets the key policy on the specified KMS key.
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
putKeyPolicy_keyId :: Lens.Lens' PutKeyPolicy Prelude.Text
putKeyPolicy_keyId = Lens.lens (\PutKeyPolicy' {keyId} -> keyId) (\s@PutKeyPolicy' {} a -> s {keyId = a} :: PutKeyPolicy)

-- | The name of the key policy. The only valid value is @default@.
putKeyPolicy_policyName :: Lens.Lens' PutKeyPolicy Prelude.Text
putKeyPolicy_policyName = Lens.lens (\PutKeyPolicy' {policyName} -> policyName) (\s@PutKeyPolicy' {} a -> s {policyName = a} :: PutKeyPolicy)

-- | The key policy to attach to the KMS key.
--
-- The key policy must meet the following criteria:
--
-- -   If you don\'t set @BypassPolicyLockoutSafetyCheck@ to true, the key
--     policy must allow the principal that is making the @PutKeyPolicy@
--     request to make a subsequent @PutKeyPolicy@ request on the KMS key.
--     This reduces the risk that the KMS key becomes unmanageable. For
--     more information, refer to the scenario in the
--     <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default-allow-root-enable-iam Default Key Policy>
--     section of the /Key Management Service Developer Guide/.
--
-- -   Each statement in the key policy must contain one or more
--     principals. The principals in the key policy must exist and be
--     visible to KMS. When you create a new Amazon Web Services principal
--     (for example, an IAM user or role), you might need to enforce a
--     delay before including the new principal in a key policy because the
--     new principal might not be immediately visible to KMS. For more
--     information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/troubleshoot_general.html#troubleshoot_general_eventual-consistency Changes that I make are not always immediately visible>
--     in the /Amazon Web Services Identity and Access Management User
--     Guide/.
--
-- A key policy document can include only the following characters:
--
-- -   Printable ASCII characters from the space character (@\\u0020@)
--     through the end of the ASCII character range.
--
-- -   Printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@).
--
-- -   The tab (@\\u0009@), line feed (@\\u000A@), and carriage return
--     (@\\u000D@) special characters
--
-- For information about key policies, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Key policies in KMS>
-- in the /Key Management Service Developer Guide/.For help writing and
-- formatting a JSON policy document, see the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the //Identity and Access Management User Guide// .
putKeyPolicy_policy :: Lens.Lens' PutKeyPolicy Prelude.Text
putKeyPolicy_policy = Lens.lens (\PutKeyPolicy' {policy} -> policy) (\s@PutKeyPolicy' {} a -> s {policy = a} :: PutKeyPolicy)

instance Core.AWSRequest PutKeyPolicy where
  type AWSResponse PutKeyPolicy = PutKeyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull PutKeyPolicyResponse'

instance Prelude.Hashable PutKeyPolicy where
  hashWithSalt _salt PutKeyPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` bypassPolicyLockoutSafetyCheck
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutKeyPolicy where
  rnf PutKeyPolicy' {..} =
    Prelude.rnf bypassPolicyLockoutSafetyCheck `Prelude.seq`
      Prelude.rnf keyId `Prelude.seq`
        Prelude.rnf policyName `Prelude.seq`
          Prelude.rnf policy

instance Data.ToHeaders PutKeyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.PutKeyPolicy" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutKeyPolicy where
  toJSON PutKeyPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Data..=)
              Prelude.<$> bypassPolicyLockoutSafetyCheck,
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("PolicyName" Data..= policyName),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutKeyPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutKeyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutKeyPolicyResponse' smart constructor.
data PutKeyPolicyResponse = PutKeyPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutKeyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutKeyPolicyResponse ::
  PutKeyPolicyResponse
newPutKeyPolicyResponse = PutKeyPolicyResponse'

instance Prelude.NFData PutKeyPolicyResponse where
  rnf _ = ()

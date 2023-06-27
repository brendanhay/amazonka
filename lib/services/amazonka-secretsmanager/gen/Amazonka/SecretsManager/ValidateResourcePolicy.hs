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
-- Module      : Amazonka.SecretsManager.ValidateResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates that a resource policy does not grant a wide range of
-- principals access to your secret. A resource-based policy is optional
-- for secrets.
--
-- The API performs three checks when validating the policy:
--
-- -   Sends a call to
--     <https://aws.amazon.com/blogs/security/protect-sensitive-data-in-the-cloud-with-automated-reasoning-zelkova/ Zelkova>,
--     an automated reasoning engine, to ensure your resource policy does
--     not allow broad access to your secret, for example policies that use
--     a wildcard for the principal.
--
-- -   Checks for correct syntax in a policy.
--
-- -   Verifies the policy does not lock out a caller.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:ValidateResourcePolicy@ and
-- @secretsmanager:PutResourcePolicy@. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.ValidateResourcePolicy
  ( -- * Creating a Request
    ValidateResourcePolicy (..),
    newValidateResourcePolicy,

    -- * Request Lenses
    validateResourcePolicy_secretId,
    validateResourcePolicy_resourcePolicy,

    -- * Destructuring the Response
    ValidateResourcePolicyResponse (..),
    newValidateResourcePolicyResponse,

    -- * Response Lenses
    validateResourcePolicyResponse_policyValidationPassed,
    validateResourcePolicyResponse_validationErrors,
    validateResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newValidateResourcePolicy' smart constructor.
data ValidateResourcePolicy = ValidateResourcePolicy'
  { -- | This field is reserved for internal use.
    secretId :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string that contains an Amazon Web Services
    -- resource-based policy. The policy in the string identifies who can
    -- access or manage this secret and its versions. For example policies, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'validateResourcePolicy_secretId' - This field is reserved for internal use.
--
-- 'resourcePolicy', 'validateResourcePolicy_resourcePolicy' - A JSON-formatted string that contains an Amazon Web Services
-- resource-based policy. The policy in the string identifies who can
-- access or manage this secret and its versions. For example policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
newValidateResourcePolicy ::
  -- | 'resourcePolicy'
  Prelude.Text ->
  ValidateResourcePolicy
newValidateResourcePolicy pResourcePolicy_ =
  ValidateResourcePolicy'
    { secretId = Prelude.Nothing,
      resourcePolicy = pResourcePolicy_
    }

-- | This field is reserved for internal use.
validateResourcePolicy_secretId :: Lens.Lens' ValidateResourcePolicy (Prelude.Maybe Prelude.Text)
validateResourcePolicy_secretId = Lens.lens (\ValidateResourcePolicy' {secretId} -> secretId) (\s@ValidateResourcePolicy' {} a -> s {secretId = a} :: ValidateResourcePolicy)

-- | A JSON-formatted string that contains an Amazon Web Services
-- resource-based policy. The policy in the string identifies who can
-- access or manage this secret and its versions. For example policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
validateResourcePolicy_resourcePolicy :: Lens.Lens' ValidateResourcePolicy Prelude.Text
validateResourcePolicy_resourcePolicy = Lens.lens (\ValidateResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@ValidateResourcePolicy' {} a -> s {resourcePolicy = a} :: ValidateResourcePolicy)

instance Core.AWSRequest ValidateResourcePolicy where
  type
    AWSResponse ValidateResourcePolicy =
      ValidateResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateResourcePolicyResponse'
            Prelude.<$> (x Data..?> "PolicyValidationPassed")
            Prelude.<*> ( x
                            Data..?> "ValidationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidateResourcePolicy where
  hashWithSalt _salt ValidateResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` resourcePolicy

instance Prelude.NFData ValidateResourcePolicy where
  rnf ValidateResourcePolicy' {..} =
    Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf resourcePolicy

instance Data.ToHeaders ValidateResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.ValidateResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ValidateResourcePolicy where
  toJSON ValidateResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecretId" Data..=) Prelude.<$> secretId,
            Prelude.Just
              ("ResourcePolicy" Data..= resourcePolicy)
          ]
      )

instance Data.ToPath ValidateResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery ValidateResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateResourcePolicyResponse' smart constructor.
data ValidateResourcePolicyResponse = ValidateResourcePolicyResponse'
  { -- | True if your policy passes validation, otherwise false.
    policyValidationPassed :: Prelude.Maybe Prelude.Bool,
    -- | Validation errors if your policy didn\'t pass validation.
    validationErrors :: Prelude.Maybe [ValidationErrorsEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyValidationPassed', 'validateResourcePolicyResponse_policyValidationPassed' - True if your policy passes validation, otherwise false.
--
-- 'validationErrors', 'validateResourcePolicyResponse_validationErrors' - Validation errors if your policy didn\'t pass validation.
--
-- 'httpStatus', 'validateResourcePolicyResponse_httpStatus' - The response's http status code.
newValidateResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateResourcePolicyResponse
newValidateResourcePolicyResponse pHttpStatus_ =
  ValidateResourcePolicyResponse'
    { policyValidationPassed =
        Prelude.Nothing,
      validationErrors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | True if your policy passes validation, otherwise false.
validateResourcePolicyResponse_policyValidationPassed :: Lens.Lens' ValidateResourcePolicyResponse (Prelude.Maybe Prelude.Bool)
validateResourcePolicyResponse_policyValidationPassed = Lens.lens (\ValidateResourcePolicyResponse' {policyValidationPassed} -> policyValidationPassed) (\s@ValidateResourcePolicyResponse' {} a -> s {policyValidationPassed = a} :: ValidateResourcePolicyResponse)

-- | Validation errors if your policy didn\'t pass validation.
validateResourcePolicyResponse_validationErrors :: Lens.Lens' ValidateResourcePolicyResponse (Prelude.Maybe [ValidationErrorsEntry])
validateResourcePolicyResponse_validationErrors = Lens.lens (\ValidateResourcePolicyResponse' {validationErrors} -> validationErrors) (\s@ValidateResourcePolicyResponse' {} a -> s {validationErrors = a} :: ValidateResourcePolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
validateResourcePolicyResponse_httpStatus :: Lens.Lens' ValidateResourcePolicyResponse Prelude.Int
validateResourcePolicyResponse_httpStatus = Lens.lens (\ValidateResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@ValidateResourcePolicyResponse' {} a -> s {httpStatus = a} :: ValidateResourcePolicyResponse)

instance
  Prelude.NFData
    ValidateResourcePolicyResponse
  where
  rnf ValidateResourcePolicyResponse' {..} =
    Prelude.rnf policyValidationPassed
      `Prelude.seq` Prelude.rnf validationErrors
      `Prelude.seq` Prelude.rnf httpStatus

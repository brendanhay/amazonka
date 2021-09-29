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
-- Module      : Network.AWS.SecretsManager.ValidateResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates that the resource policy does not grant a wide range of IAM
-- principals access to your secret. The JSON request string input and
-- response output displays formatted code with white space and line breaks
-- for better readability. Submit your input as a single line JSON string.
-- A resource-based policy is optional for secrets.
--
-- The API performs three checks when validating the secret:
--
-- -   Sends a call to
--     <https://aws.amazon.com/blogs/security/protect-sensitive-data-in-the-cloud-with-automated-reasoning-zelkova/ Zelkova>,
--     an automated reasoning engine, to ensure your Resource Policy does
--     not allow broad access to your secret.
--
-- -   Checks for correct syntax in a policy.
--
-- -   Verifies the policy does not lock out a caller.
--
-- __Minimum Permissions__
--
-- You must have the permissions required to access the following APIs:
--
-- -   @secretsmanager:PutResourcePolicy@
--
-- -   @secretsmanager:ValidateResourcePolicy@
module Network.AWS.SecretsManager.ValidateResourcePolicy
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newValidateResourcePolicy' smart constructor.
data ValidateResourcePolicy = ValidateResourcePolicy'
  { -- | (Optional) The identifier of the secret with the resource-based policy
    -- you want to validate. You can specify either the Amazon Resource Name
    -- (ARN) or the friendly name of the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string constructed according to the grammar and syntax
    -- for an Amazon Web Services resource-based policy. The policy in the
    -- string identifies who can access or manage this secret and its versions.
    -- For information on how to format a JSON parameter for the various
    -- command line tool environments, see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /CLI User Guide/.publi
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
-- 'secretId', 'validateResourcePolicy_secretId' - (Optional) The identifier of the secret with the resource-based policy
-- you want to validate. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
--
-- 'resourcePolicy', 'validateResourcePolicy_resourcePolicy' - A JSON-formatted string constructed according to the grammar and syntax
-- for an Amazon Web Services resource-based policy. The policy in the
-- string identifies who can access or manage this secret and its versions.
-- For information on how to format a JSON parameter for the various
-- command line tool environments, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/.publi
newValidateResourcePolicy ::
  -- | 'resourcePolicy'
  Prelude.Text ->
  ValidateResourcePolicy
newValidateResourcePolicy pResourcePolicy_ =
  ValidateResourcePolicy'
    { secretId = Prelude.Nothing,
      resourcePolicy = pResourcePolicy_
    }

-- | (Optional) The identifier of the secret with the resource-based policy
-- you want to validate. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
validateResourcePolicy_secretId :: Lens.Lens' ValidateResourcePolicy (Prelude.Maybe Prelude.Text)
validateResourcePolicy_secretId = Lens.lens (\ValidateResourcePolicy' {secretId} -> secretId) (\s@ValidateResourcePolicy' {} a -> s {secretId = a} :: ValidateResourcePolicy)

-- | A JSON-formatted string constructed according to the grammar and syntax
-- for an Amazon Web Services resource-based policy. The policy in the
-- string identifies who can access or manage this secret and its versions.
-- For information on how to format a JSON parameter for the various
-- command line tool environments, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/.publi
validateResourcePolicy_resourcePolicy :: Lens.Lens' ValidateResourcePolicy Prelude.Text
validateResourcePolicy_resourcePolicy = Lens.lens (\ValidateResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@ValidateResourcePolicy' {} a -> s {resourcePolicy = a} :: ValidateResourcePolicy)

instance Core.AWSRequest ValidateResourcePolicy where
  type
    AWSResponse ValidateResourcePolicy =
      ValidateResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ValidateResourcePolicyResponse'
            Prelude.<$> (x Core..?> "PolicyValidationPassed")
            Prelude.<*> ( x Core..?> "ValidationErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidateResourcePolicy

instance Prelude.NFData ValidateResourcePolicy

instance Core.ToHeaders ValidateResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.ValidateResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ValidateResourcePolicy where
  toJSON ValidateResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecretId" Core..=) Prelude.<$> secretId,
            Prelude.Just
              ("ResourcePolicy" Core..= resourcePolicy)
          ]
      )

instance Core.ToPath ValidateResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery ValidateResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newValidateResourcePolicyResponse' smart constructor.
data ValidateResourcePolicyResponse = ValidateResourcePolicyResponse'
  { -- | Returns a message stating that your Reource Policy passed validation.
    policyValidationPassed :: Prelude.Maybe Prelude.Bool,
    -- | Returns an error message if your policy doesn\'t pass validatation.
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
-- 'policyValidationPassed', 'validateResourcePolicyResponse_policyValidationPassed' - Returns a message stating that your Reource Policy passed validation.
--
-- 'validationErrors', 'validateResourcePolicyResponse_validationErrors' - Returns an error message if your policy doesn\'t pass validatation.
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

-- | Returns a message stating that your Reource Policy passed validation.
validateResourcePolicyResponse_policyValidationPassed :: Lens.Lens' ValidateResourcePolicyResponse (Prelude.Maybe Prelude.Bool)
validateResourcePolicyResponse_policyValidationPassed = Lens.lens (\ValidateResourcePolicyResponse' {policyValidationPassed} -> policyValidationPassed) (\s@ValidateResourcePolicyResponse' {} a -> s {policyValidationPassed = a} :: ValidateResourcePolicyResponse)

-- | Returns an error message if your policy doesn\'t pass validatation.
validateResourcePolicyResponse_validationErrors :: Lens.Lens' ValidateResourcePolicyResponse (Prelude.Maybe [ValidationErrorsEntry])
validateResourcePolicyResponse_validationErrors = Lens.lens (\ValidateResourcePolicyResponse' {validationErrors} -> validationErrors) (\s@ValidateResourcePolicyResponse' {} a -> s {validationErrors = a} :: ValidateResourcePolicyResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
validateResourcePolicyResponse_httpStatus :: Lens.Lens' ValidateResourcePolicyResponse Prelude.Int
validateResourcePolicyResponse_httpStatus = Lens.lens (\ValidateResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@ValidateResourcePolicyResponse' {} a -> s {httpStatus = a} :: ValidateResourcePolicyResponse)

instance
  Prelude.NFData
    ValidateResourcePolicyResponse

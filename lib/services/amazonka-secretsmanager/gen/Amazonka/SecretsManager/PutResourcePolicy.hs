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
-- Module      : Amazonka.SecretsManager.PutResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the contents of the specified resource-based permission policy
-- to a secret. A resource-based policy is optional. Alternatively, you can
-- use IAM identity-based policies that specify the secret\'s Amazon
-- Resource Name (ARN) in the policy statement\'s @Resources@ element. You
-- can also use a combination of both identity-based and resource-based
-- policies. The affected users and roles receive the permissions that are
-- permitted by all of the relevant policies. For more information, see
-- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-based-policies.html Using Resource-Based Policies for Amazon Web Services Secrets Manager>.
-- For the complete description of the Amazon Web Services policy syntax
-- and grammar, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies.html IAM JSON Policy Reference>
-- in the /IAM User Guide/.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:PutResourcePolicy
--
-- __Related operations__
--
-- -   To retrieve the resource policy attached to a secret, use
--     GetResourcePolicy.
--
-- -   To delete the resource-based policy attached to a secret, use
--     DeleteResourcePolicy.
--
-- -   To list all of the currently available secrets, use ListSecrets.
module Amazonka.SecretsManager.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_blockPublicPolicy,
    putResourcePolicy_secretId,
    putResourcePolicy_resourcePolicy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | (Optional) If you set the parameter, @BlockPublicPolicy@ to true, then
    -- you block resource-based policies that allow broad access to the secret.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the secret that you want to attach the resource-based policy.
    -- You can specify either the ARN or the friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text,
    -- | A JSON-formatted string constructed according to the grammar and syntax
    -- for an Amazon Web Services resource-based policy. The policy in the
    -- string identifies who can access or manage this secret and its versions.
    -- For information on how to format a JSON parameter for the various
    -- command line tool environments, see
    -- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /CLI User Guide/.
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockPublicPolicy', 'putResourcePolicy_blockPublicPolicy' - (Optional) If you set the parameter, @BlockPublicPolicy@ to true, then
-- you block resource-based policies that allow broad access to the secret.
--
-- 'secretId', 'putResourcePolicy_secretId' - Specifies the secret that you want to attach the resource-based policy.
-- You can specify either the ARN or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
--
-- 'resourcePolicy', 'putResourcePolicy_resourcePolicy' - A JSON-formatted string constructed according to the grammar and syntax
-- for an Amazon Web Services resource-based policy. The policy in the
-- string identifies who can access or manage this secret and its versions.
-- For information on how to format a JSON parameter for the various
-- command line tool environments, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/.
newPutResourcePolicy ::
  -- | 'secretId'
  Prelude.Text ->
  -- | 'resourcePolicy'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pSecretId_ pResourcePolicy_ =
  PutResourcePolicy'
    { blockPublicPolicy =
        Prelude.Nothing,
      secretId = pSecretId_,
      resourcePolicy = pResourcePolicy_
    }

-- | (Optional) If you set the parameter, @BlockPublicPolicy@ to true, then
-- you block resource-based policies that allow broad access to the secret.
putResourcePolicy_blockPublicPolicy :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Bool)
putResourcePolicy_blockPublicPolicy = Lens.lens (\PutResourcePolicy' {blockPublicPolicy} -> blockPublicPolicy) (\s@PutResourcePolicy' {} a -> s {blockPublicPolicy = a} :: PutResourcePolicy)

-- | Specifies the secret that you want to attach the resource-based policy.
-- You can specify either the ARN or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
putResourcePolicy_secretId :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_secretId = Lens.lens (\PutResourcePolicy' {secretId} -> secretId) (\s@PutResourcePolicy' {} a -> s {secretId = a} :: PutResourcePolicy)

-- | A JSON-formatted string constructed according to the grammar and syntax
-- for an Amazon Web Services resource-based policy. The policy in the
-- string identifies who can access or manage this secret and its versions.
-- For information on how to format a JSON parameter for the various
-- command line tool environments, see
-- <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/.
putResourcePolicy_resourcePolicy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourcePolicy = Lens.lens (\PutResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicy' {} a -> s {resourcePolicy = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` blockPublicPolicy
      `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` resourcePolicy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf blockPublicPolicy
      `Prelude.seq` Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf resourcePolicy

instance Core.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BlockPublicPolicy" Core..=)
              Prelude.<$> blockPublicPolicy,
            Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just
              ("ResourcePolicy" Core..= resourcePolicy)
          ]
      )

instance Core.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The ARN of the secret retrieved by the resource-based policy.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret retrieved by the resource-based policy.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'putResourcePolicyResponse_arn' - The ARN of the secret retrieved by the resource-based policy.
--
-- 'name', 'putResourcePolicyResponse_name' - The friendly name of the secret retrieved by the resource-based policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret retrieved by the resource-based policy.
putResourcePolicyResponse_arn :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_arn = Lens.lens (\PutResourcePolicyResponse' {arn} -> arn) (\s@PutResourcePolicyResponse' {} a -> s {arn = a} :: PutResourcePolicyResponse)

-- | The friendly name of the secret retrieved by the resource-based policy.
putResourcePolicyResponse_name :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_name = Lens.lens (\PutResourcePolicyResponse' {name} -> name) (\s@PutResourcePolicyResponse' {} a -> s {name = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus

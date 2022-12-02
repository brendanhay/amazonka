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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based permission policy to a secret. A
-- resource-based policy is optional. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control for Secrets Manager>
--
-- For information about attaching a policy in the console, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-based-policies.html Attach a permissions policy to a secret>.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:PutResourcePolicy@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
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
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Specifies whether to block resource-based policies that allow broad
    -- access to the secret, for example those that use a wildcard for the
    -- principal.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool,
    -- | The ARN or name of the secret to attach the resource-based policy.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text,
    -- | A JSON-formatted string for an Amazon Web Services resource-based
    -- policy. For example policies, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
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
-- 'blockPublicPolicy', 'putResourcePolicy_blockPublicPolicy' - Specifies whether to block resource-based policies that allow broad
-- access to the secret, for example those that use a wildcard for the
-- principal.
--
-- 'secretId', 'putResourcePolicy_secretId' - The ARN or name of the secret to attach the resource-based policy.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
--
-- 'resourcePolicy', 'putResourcePolicy_resourcePolicy' - A JSON-formatted string for an Amazon Web Services resource-based
-- policy. For example policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
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

-- | Specifies whether to block resource-based policies that allow broad
-- access to the secret, for example those that use a wildcard for the
-- principal.
putResourcePolicy_blockPublicPolicy :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Bool)
putResourcePolicy_blockPublicPolicy = Lens.lens (\PutResourcePolicy' {blockPublicPolicy} -> blockPublicPolicy) (\s@PutResourcePolicy' {} a -> s {blockPublicPolicy = a} :: PutResourcePolicy)

-- | The ARN or name of the secret to attach the resource-based policy.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
putResourcePolicy_secretId :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_secretId = Lens.lens (\PutResourcePolicy' {secretId} -> secretId) (\s@PutResourcePolicy' {} a -> s {secretId = a} :: PutResourcePolicy)

-- | A JSON-formatted string for an Amazon Web Services resource-based
-- policy. For example policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html Permissions policy examples>.
putResourcePolicy_resourcePolicy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourcePolicy = Lens.lens (\PutResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicy' {} a -> s {resourcePolicy = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ARN")
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

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BlockPublicPolicy" Data..=)
              Prelude.<$> blockPublicPolicy,
            Prelude.Just ("SecretId" Data..= secretId),
            Prelude.Just
              ("ResourcePolicy" Data..= resourcePolicy)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
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
-- 'name', 'putResourcePolicyResponse_name' - The name of the secret.
--
-- 'arn', 'putResourcePolicyResponse_arn' - The ARN of the secret.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the secret.
putResourcePolicyResponse_name :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_name = Lens.lens (\PutResourcePolicyResponse' {name} -> name) (\s@PutResourcePolicyResponse' {} a -> s {name = a} :: PutResourcePolicyResponse)

-- | The ARN of the secret.
putResourcePolicyResponse_arn :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_arn = Lens.lens (\PutResourcePolicyResponse' {arn} -> arn) (\s@PutResourcePolicyResponse' {} a -> s {arn = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus

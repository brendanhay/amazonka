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
-- Module      : Amazonka.SecretsManager.GetResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document attached
-- to the secret. For more information about permissions policies attached
-- to a secret, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_resource-policies.html Permissions policies attached to a secret>.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:GetResourcePolicy@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.GetResourcePolicy
  ( -- * Creating a Request
    GetResourcePolicy (..),
    newGetResourcePolicy,

    -- * Request Lenses
    getResourcePolicy_secretId,

    -- * Destructuring the Response
    GetResourcePolicyResponse (..),
    newGetResourcePolicyResponse,

    -- * Response Lenses
    getResourcePolicyResponse_arn,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN or name of the secret to retrieve the attached resource-based
    -- policy for.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'getResourcePolicy_secretId' - The ARN or name of the secret to retrieve the attached resource-based
-- policy for.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newGetResourcePolicy ::
  -- | 'secretId'
  Prelude.Text ->
  GetResourcePolicy
newGetResourcePolicy pSecretId_ =
  GetResourcePolicy' {secretId = pSecretId_}

-- | The ARN or name of the secret to retrieve the attached resource-based
-- policy for.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
getResourcePolicy_secretId :: Lens.Lens' GetResourcePolicy Prelude.Text
getResourcePolicy_secretId = Lens.lens (\GetResourcePolicy' {secretId} -> secretId) (\s@GetResourcePolicy' {} a -> s {secretId = a} :: GetResourcePolicy)

instance Core.AWSRequest GetResourcePolicy where
  type
    AWSResponse GetResourcePolicy =
      GetResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ResourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy where
  hashWithSalt _salt GetResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` secretId

instance Prelude.NFData GetResourcePolicy where
  rnf GetResourcePolicy' {..} = Prelude.rnf secretId

instance Data.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.GetResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Data..= secretId)]
      )

instance Data.ToPath GetResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The ARN of the secret that the resource-based policy was retrieved for.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret that the resource-based policy was retrieved for.
    name :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string that contains the permissions policy attached to
    -- the secret. For more information about permissions policies, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control for Secrets Manager>.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getResourcePolicyResponse_arn' - The ARN of the secret that the resource-based policy was retrieved for.
--
-- 'name', 'getResourcePolicyResponse_name' - The name of the secret that the resource-based policy was retrieved for.
--
-- 'resourcePolicy', 'getResourcePolicyResponse_resourcePolicy' - A JSON-formatted string that contains the permissions policy attached to
-- the secret. For more information about permissions policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control for Secrets Manager>.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that the resource-based policy was retrieved for.
getResourcePolicyResponse_arn :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_arn = Lens.lens (\GetResourcePolicyResponse' {arn} -> arn) (\s@GetResourcePolicyResponse' {} a -> s {arn = a} :: GetResourcePolicyResponse)

-- | The name of the secret that the resource-based policy was retrieved for.
getResourcePolicyResponse_name :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_name = Lens.lens (\GetResourcePolicyResponse' {name} -> name) (\s@GetResourcePolicyResponse' {} a -> s {name = a} :: GetResourcePolicyResponse)

-- | A JSON-formatted string that contains the permissions policy attached to
-- the secret. For more information about permissions policies, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control for Secrets Manager>.
getResourcePolicyResponse_resourcePolicy :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_resourcePolicy = Lens.lens (\GetResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@GetResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse where
  rnf GetResourcePolicyResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus

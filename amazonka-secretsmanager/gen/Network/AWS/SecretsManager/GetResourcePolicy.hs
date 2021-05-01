{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SecretsManager.GetResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document attached
-- to the specified secret. The JSON request string input and response
-- output displays formatted code with white space and line breaks for
-- better readability. Submit your input as a single line JSON string.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:GetResourcePolicy
--
-- __Related operations__
--
-- -   To attach a resource policy to a secret, use PutResourcePolicy.
--
-- -   To delete the resource-based policy attached to a secret, use
--     DeleteResourcePolicy.
--
-- -   To list all of the currently available secrets, use ListSecrets.
module Network.AWS.SecretsManager.GetResourcePolicy
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
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | Specifies the secret that you want to retrieve the attached
    -- resource-based policy for. You can specify either the Amazon Resource
    -- Name (ARN) or the friendly name of the secret.
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
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'getResourcePolicy_secretId' - Specifies the secret that you want to retrieve the attached
-- resource-based policy for. You can specify either the Amazon Resource
-- Name (ARN) or the friendly name of the secret.
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
newGetResourcePolicy ::
  -- | 'secretId'
  Prelude.Text ->
  GetResourcePolicy
newGetResourcePolicy pSecretId_ =
  GetResourcePolicy' {secretId = pSecretId_}

-- | Specifies the secret that you want to retrieve the attached
-- resource-based policy for. You can specify either the Amazon Resource
-- Name (ARN) or the friendly name of the secret.
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
getResourcePolicy_secretId :: Lens.Lens' GetResourcePolicy Prelude.Text
getResourcePolicy_secretId = Lens.lens (\GetResourcePolicy' {secretId} -> secretId) (\s@GetResourcePolicy' {} a -> s {secretId = a} :: GetResourcePolicy)

instance Prelude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Prelude..?> "ARN")
            Prelude.<*> (x Prelude..?> "ResourcePolicy")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy

instance Prelude.NFData GetResourcePolicy

instance Prelude.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "secretsmanager.GetResourcePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Prelude..= secretId)]
      )

instance Prelude.ToPath GetResourcePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The ARN of the secret that the resource-based policy was retrieved for.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string that describes the permissions that are
    -- associated with the attached secret. These permissions are combined with
    -- any permissions that are associated with the user or role that attempts
    -- to access this secret. The combined permissions specify who can access
    -- the secret and what actions they can perform. For more information, see
    -- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager>
    -- in the /AWS Secrets Manager User Guide/.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret that the resource-based policy was
    -- retrieved for.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'resourcePolicy', 'getResourcePolicyResponse_resourcePolicy' - A JSON-formatted string that describes the permissions that are
-- associated with the attached secret. These permissions are combined with
-- any permissions that are associated with the user or role that attempts
-- to access this secret. The combined permissions specify who can access
-- the secret and what actions they can perform. For more information, see
-- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager>
-- in the /AWS Secrets Manager User Guide/.
--
-- 'name', 'getResourcePolicyResponse_name' - The friendly name of the secret that the resource-based policy was
-- retrieved for.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { arn = Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that the resource-based policy was retrieved for.
getResourcePolicyResponse_arn :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_arn = Lens.lens (\GetResourcePolicyResponse' {arn} -> arn) (\s@GetResourcePolicyResponse' {} a -> s {arn = a} :: GetResourcePolicyResponse)

-- | A JSON-formatted string that describes the permissions that are
-- associated with the attached secret. These permissions are combined with
-- any permissions that are associated with the user or role that attempts
-- to access this secret. The combined permissions specify who can access
-- the secret and what actions they can perform. For more information, see
-- <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager>
-- in the /AWS Secrets Manager User Guide/.
getResourcePolicyResponse_resourcePolicy :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_resourcePolicy = Lens.lens (\GetResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@GetResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: GetResourcePolicyResponse)

-- | The friendly name of the secret that the resource-based policy was
-- retrieved for.
getResourcePolicyResponse_name :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_name = Lens.lens (\GetResourcePolicyResponse' {name} -> name) (\s@GetResourcePolicyResponse' {} a -> s {name = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse

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
-- Module      : Amazonka.OpenSearchServerless.CreateSecurityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a security policy to be used by one or more OpenSearch
-- Serverless collections. Security policies provide access to a collection
-- and its OpenSearch Dashboards endpoint from public networks or specific
-- VPC endpoints. They also allow you to secure a collection with a KMS
-- encryption key. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-network.html Network access for Amazon OpenSearch Serverless>
-- and
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-encryption.html Encryption at rest for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.CreateSecurityPolicy
  ( -- * Creating a Request
    CreateSecurityPolicy (..),
    newCreateSecurityPolicy,

    -- * Request Lenses
    createSecurityPolicy_clientToken,
    createSecurityPolicy_description,
    createSecurityPolicy_name,
    createSecurityPolicy_policy,
    createSecurityPolicy_type,

    -- * Destructuring the Response
    CreateSecurityPolicyResponse (..),
    newCreateSecurityPolicyResponse,

    -- * Response Lenses
    createSecurityPolicyResponse_securityPolicyDetail,
    createSecurityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSecurityPolicy' smart constructor.
data CreateSecurityPolicy = CreateSecurityPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy. Typically used to store information about
    -- the permissions defined in the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    name :: Prelude.Text,
    -- | The JSON policy document to use as the content for the new policy.
    policy :: Prelude.Text,
    -- | The type of security policy.
    type' :: SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSecurityPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'createSecurityPolicy_description' - A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
--
-- 'name', 'createSecurityPolicy_name' - The name of the policy.
--
-- 'policy', 'createSecurityPolicy_policy' - The JSON policy document to use as the content for the new policy.
--
-- 'type'', 'createSecurityPolicy_type' - The type of security policy.
newCreateSecurityPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  -- | 'type''
  SecurityPolicyType ->
  CreateSecurityPolicy
newCreateSecurityPolicy pName_ pPolicy_ pType_ =
  CreateSecurityPolicy'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      policy = pPolicy_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createSecurityPolicy_clientToken :: Lens.Lens' CreateSecurityPolicy (Prelude.Maybe Prelude.Text)
createSecurityPolicy_clientToken = Lens.lens (\CreateSecurityPolicy' {clientToken} -> clientToken) (\s@CreateSecurityPolicy' {} a -> s {clientToken = a} :: CreateSecurityPolicy)

-- | A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
createSecurityPolicy_description :: Lens.Lens' CreateSecurityPolicy (Prelude.Maybe Prelude.Text)
createSecurityPolicy_description = Lens.lens (\CreateSecurityPolicy' {description} -> description) (\s@CreateSecurityPolicy' {} a -> s {description = a} :: CreateSecurityPolicy)

-- | The name of the policy.
createSecurityPolicy_name :: Lens.Lens' CreateSecurityPolicy Prelude.Text
createSecurityPolicy_name = Lens.lens (\CreateSecurityPolicy' {name} -> name) (\s@CreateSecurityPolicy' {} a -> s {name = a} :: CreateSecurityPolicy)

-- | The JSON policy document to use as the content for the new policy.
createSecurityPolicy_policy :: Lens.Lens' CreateSecurityPolicy Prelude.Text
createSecurityPolicy_policy = Lens.lens (\CreateSecurityPolicy' {policy} -> policy) (\s@CreateSecurityPolicy' {} a -> s {policy = a} :: CreateSecurityPolicy)

-- | The type of security policy.
createSecurityPolicy_type :: Lens.Lens' CreateSecurityPolicy SecurityPolicyType
createSecurityPolicy_type = Lens.lens (\CreateSecurityPolicy' {type'} -> type') (\s@CreateSecurityPolicy' {} a -> s {type' = a} :: CreateSecurityPolicy)

instance Core.AWSRequest CreateSecurityPolicy where
  type
    AWSResponse CreateSecurityPolicy =
      CreateSecurityPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecurityPolicyResponse'
            Prelude.<$> (x Data..?> "securityPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecurityPolicy where
  hashWithSalt _salt CreateSecurityPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateSecurityPolicy where
  rnf CreateSecurityPolicy' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf policy `Prelude.seq`
            Prelude.rnf type'

instance Data.ToHeaders CreateSecurityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.CreateSecurityPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSecurityPolicy where
  toJSON CreateSecurityPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("policy" Data..= policy),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateSecurityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSecurityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecurityPolicyResponse' smart constructor.
data CreateSecurityPolicyResponse = CreateSecurityPolicyResponse'
  { -- | Details about the created security policy.
    securityPolicyDetail :: Prelude.Maybe SecurityPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecurityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityPolicyDetail', 'createSecurityPolicyResponse_securityPolicyDetail' - Details about the created security policy.
--
-- 'httpStatus', 'createSecurityPolicyResponse_httpStatus' - The response's http status code.
newCreateSecurityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSecurityPolicyResponse
newCreateSecurityPolicyResponse pHttpStatus_ =
  CreateSecurityPolicyResponse'
    { securityPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the created security policy.
createSecurityPolicyResponse_securityPolicyDetail :: Lens.Lens' CreateSecurityPolicyResponse (Prelude.Maybe SecurityPolicyDetail)
createSecurityPolicyResponse_securityPolicyDetail = Lens.lens (\CreateSecurityPolicyResponse' {securityPolicyDetail} -> securityPolicyDetail) (\s@CreateSecurityPolicyResponse' {} a -> s {securityPolicyDetail = a} :: CreateSecurityPolicyResponse)

-- | The response's http status code.
createSecurityPolicyResponse_httpStatus :: Lens.Lens' CreateSecurityPolicyResponse Prelude.Int
createSecurityPolicyResponse_httpStatus = Lens.lens (\CreateSecurityPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateSecurityPolicyResponse' {} a -> s {httpStatus = a} :: CreateSecurityPolicyResponse)

instance Prelude.NFData CreateSecurityPolicyResponse where
  rnf CreateSecurityPolicyResponse' {..} =
    Prelude.rnf securityPolicyDetail `Prelude.seq`
      Prelude.rnf httpStatus

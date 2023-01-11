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
-- Module      : Amazonka.OpenSearchServerless.CreateAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data access policy for OpenSearch Serverless. Access policies
-- limit access to collections and the resources within them, and allow a
-- user to access that data irrespective of the access mechanism or network
-- source. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-data-access.html Data access control for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.CreateAccessPolicy
  ( -- * Creating a Request
    CreateAccessPolicy (..),
    newCreateAccessPolicy,

    -- * Request Lenses
    createAccessPolicy_clientToken,
    createAccessPolicy_description,
    createAccessPolicy_name,
    createAccessPolicy_policy,
    createAccessPolicy_type,

    -- * Destructuring the Response
    CreateAccessPolicyResponse (..),
    newCreateAccessPolicyResponse,

    -- * Response Lenses
    createAccessPolicyResponse_accessPolicyDetail,
    createAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessPolicy' smart constructor.
data CreateAccessPolicy = CreateAccessPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy. Typically used to store information about
    -- the permissions defined in the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    name :: Prelude.Text,
    -- | The JSON policy document to use as the content for the policy.
    policy :: Prelude.Text,
    -- | The type of policy.
    type' :: AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createAccessPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'createAccessPolicy_description' - A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
--
-- 'name', 'createAccessPolicy_name' - The name of the policy.
--
-- 'policy', 'createAccessPolicy_policy' - The JSON policy document to use as the content for the policy.
--
-- 'type'', 'createAccessPolicy_type' - The type of policy.
newCreateAccessPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  -- | 'type''
  AccessPolicyType ->
  CreateAccessPolicy
newCreateAccessPolicy pName_ pPolicy_ pType_ =
  CreateAccessPolicy'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      policy = pPolicy_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
createAccessPolicy_clientToken :: Lens.Lens' CreateAccessPolicy (Prelude.Maybe Prelude.Text)
createAccessPolicy_clientToken = Lens.lens (\CreateAccessPolicy' {clientToken} -> clientToken) (\s@CreateAccessPolicy' {} a -> s {clientToken = a} :: CreateAccessPolicy)

-- | A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
createAccessPolicy_description :: Lens.Lens' CreateAccessPolicy (Prelude.Maybe Prelude.Text)
createAccessPolicy_description = Lens.lens (\CreateAccessPolicy' {description} -> description) (\s@CreateAccessPolicy' {} a -> s {description = a} :: CreateAccessPolicy)

-- | The name of the policy.
createAccessPolicy_name :: Lens.Lens' CreateAccessPolicy Prelude.Text
createAccessPolicy_name = Lens.lens (\CreateAccessPolicy' {name} -> name) (\s@CreateAccessPolicy' {} a -> s {name = a} :: CreateAccessPolicy)

-- | The JSON policy document to use as the content for the policy.
createAccessPolicy_policy :: Lens.Lens' CreateAccessPolicy Prelude.Text
createAccessPolicy_policy = Lens.lens (\CreateAccessPolicy' {policy} -> policy) (\s@CreateAccessPolicy' {} a -> s {policy = a} :: CreateAccessPolicy)

-- | The type of policy.
createAccessPolicy_type :: Lens.Lens' CreateAccessPolicy AccessPolicyType
createAccessPolicy_type = Lens.lens (\CreateAccessPolicy' {type'} -> type') (\s@CreateAccessPolicy' {} a -> s {type' = a} :: CreateAccessPolicy)

instance Core.AWSRequest CreateAccessPolicy where
  type
    AWSResponse CreateAccessPolicy =
      CreateAccessPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessPolicyResponse'
            Prelude.<$> (x Data..?> "accessPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccessPolicy where
  hashWithSalt _salt CreateAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateAccessPolicy where
  rnf CreateAccessPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.CreateAccessPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessPolicy where
  toJSON CreateAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("policy" Data..= policy),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateAccessPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessPolicyResponse' smart constructor.
data CreateAccessPolicyResponse = CreateAccessPolicyResponse'
  { -- | Details about the created access policy.
    accessPolicyDetail :: Prelude.Maybe AccessPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicyDetail', 'createAccessPolicyResponse_accessPolicyDetail' - Details about the created access policy.
--
-- 'httpStatus', 'createAccessPolicyResponse_httpStatus' - The response's http status code.
newCreateAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAccessPolicyResponse
newCreateAccessPolicyResponse pHttpStatus_ =
  CreateAccessPolicyResponse'
    { accessPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the created access policy.
createAccessPolicyResponse_accessPolicyDetail :: Lens.Lens' CreateAccessPolicyResponse (Prelude.Maybe AccessPolicyDetail)
createAccessPolicyResponse_accessPolicyDetail = Lens.lens (\CreateAccessPolicyResponse' {accessPolicyDetail} -> accessPolicyDetail) (\s@CreateAccessPolicyResponse' {} a -> s {accessPolicyDetail = a} :: CreateAccessPolicyResponse)

-- | The response's http status code.
createAccessPolicyResponse_httpStatus :: Lens.Lens' CreateAccessPolicyResponse Prelude.Int
createAccessPolicyResponse_httpStatus = Lens.lens (\CreateAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateAccessPolicyResponse' {} a -> s {httpStatus = a} :: CreateAccessPolicyResponse)

instance Prelude.NFData CreateAccessPolicyResponse where
  rnf CreateAccessPolicyResponse' {..} =
    Prelude.rnf accessPolicyDetail
      `Prelude.seq` Prelude.rnf httpStatus

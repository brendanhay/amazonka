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
-- Module      : Amazonka.OpenSearchServerless.UpdateAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an OpenSearch Serverless access policy. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-data-access.html Data access control for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.UpdateAccessPolicy
  ( -- * Creating a Request
    UpdateAccessPolicy (..),
    newUpdateAccessPolicy,

    -- * Request Lenses
    updateAccessPolicy_clientToken,
    updateAccessPolicy_description,
    updateAccessPolicy_policy,
    updateAccessPolicy_name,
    updateAccessPolicy_policyVersion,
    updateAccessPolicy_type,

    -- * Destructuring the Response
    UpdateAccessPolicyResponse (..),
    newUpdateAccessPolicyResponse,

    -- * Response Lenses
    updateAccessPolicyResponse_accessPolicyDetail,
    updateAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAccessPolicy' smart constructor.
data UpdateAccessPolicy = UpdateAccessPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy. Typically used to store information about
    -- the permissions defined in the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy document to use as the content for the policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    name :: Prelude.Text,
    -- | The version of the policy being updated.
    policyVersion :: Prelude.Text,
    -- | The type of policy.
    type' :: AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateAccessPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'updateAccessPolicy_description' - A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
--
-- 'policy', 'updateAccessPolicy_policy' - The JSON policy document to use as the content for the policy.
--
-- 'name', 'updateAccessPolicy_name' - The name of the policy.
--
-- 'policyVersion', 'updateAccessPolicy_policyVersion' - The version of the policy being updated.
--
-- 'type'', 'updateAccessPolicy_type' - The type of policy.
newUpdateAccessPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'policyVersion'
  Prelude.Text ->
  -- | 'type''
  AccessPolicyType ->
  UpdateAccessPolicy
newUpdateAccessPolicy pName_ pPolicyVersion_ pType_ =
  UpdateAccessPolicy'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      policy = Prelude.Nothing,
      name = pName_,
      policyVersion = pPolicyVersion_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
updateAccessPolicy_clientToken :: Lens.Lens' UpdateAccessPolicy (Prelude.Maybe Prelude.Text)
updateAccessPolicy_clientToken = Lens.lens (\UpdateAccessPolicy' {clientToken} -> clientToken) (\s@UpdateAccessPolicy' {} a -> s {clientToken = a} :: UpdateAccessPolicy)

-- | A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
updateAccessPolicy_description :: Lens.Lens' UpdateAccessPolicy (Prelude.Maybe Prelude.Text)
updateAccessPolicy_description = Lens.lens (\UpdateAccessPolicy' {description} -> description) (\s@UpdateAccessPolicy' {} a -> s {description = a} :: UpdateAccessPolicy)

-- | The JSON policy document to use as the content for the policy.
updateAccessPolicy_policy :: Lens.Lens' UpdateAccessPolicy (Prelude.Maybe Prelude.Text)
updateAccessPolicy_policy = Lens.lens (\UpdateAccessPolicy' {policy} -> policy) (\s@UpdateAccessPolicy' {} a -> s {policy = a} :: UpdateAccessPolicy)

-- | The name of the policy.
updateAccessPolicy_name :: Lens.Lens' UpdateAccessPolicy Prelude.Text
updateAccessPolicy_name = Lens.lens (\UpdateAccessPolicy' {name} -> name) (\s@UpdateAccessPolicy' {} a -> s {name = a} :: UpdateAccessPolicy)

-- | The version of the policy being updated.
updateAccessPolicy_policyVersion :: Lens.Lens' UpdateAccessPolicy Prelude.Text
updateAccessPolicy_policyVersion = Lens.lens (\UpdateAccessPolicy' {policyVersion} -> policyVersion) (\s@UpdateAccessPolicy' {} a -> s {policyVersion = a} :: UpdateAccessPolicy)

-- | The type of policy.
updateAccessPolicy_type :: Lens.Lens' UpdateAccessPolicy AccessPolicyType
updateAccessPolicy_type = Lens.lens (\UpdateAccessPolicy' {type'} -> type') (\s@UpdateAccessPolicy' {} a -> s {type' = a} :: UpdateAccessPolicy)

instance Core.AWSRequest UpdateAccessPolicy where
  type
    AWSResponse UpdateAccessPolicy =
      UpdateAccessPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccessPolicyResponse'
            Prelude.<$> (x Data..?> "accessPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAccessPolicy where
  hashWithSalt _salt UpdateAccessPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UpdateAccessPolicy where
  rnf UpdateAccessPolicy' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf policy `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf policyVersion `Prelude.seq`
              Prelude.rnf type'

instance Data.ToHeaders UpdateAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateAccessPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAccessPolicy where
  toJSON UpdateAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("policy" Data..=) Prelude.<$> policy,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("policyVersion" Data..= policyVersion),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath UpdateAccessPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccessPolicyResponse' smart constructor.
data UpdateAccessPolicyResponse = UpdateAccessPolicyResponse'
  { -- | Details about the updated access policy.
    accessPolicyDetail :: Prelude.Maybe AccessPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicyDetail', 'updateAccessPolicyResponse_accessPolicyDetail' - Details about the updated access policy.
--
-- 'httpStatus', 'updateAccessPolicyResponse_httpStatus' - The response's http status code.
newUpdateAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAccessPolicyResponse
newUpdateAccessPolicyResponse pHttpStatus_ =
  UpdateAccessPolicyResponse'
    { accessPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated access policy.
updateAccessPolicyResponse_accessPolicyDetail :: Lens.Lens' UpdateAccessPolicyResponse (Prelude.Maybe AccessPolicyDetail)
updateAccessPolicyResponse_accessPolicyDetail = Lens.lens (\UpdateAccessPolicyResponse' {accessPolicyDetail} -> accessPolicyDetail) (\s@UpdateAccessPolicyResponse' {} a -> s {accessPolicyDetail = a} :: UpdateAccessPolicyResponse)

-- | The response's http status code.
updateAccessPolicyResponse_httpStatus :: Lens.Lens' UpdateAccessPolicyResponse Prelude.Int
updateAccessPolicyResponse_httpStatus = Lens.lens (\UpdateAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateAccessPolicyResponse' {} a -> s {httpStatus = a} :: UpdateAccessPolicyResponse)

instance Prelude.NFData UpdateAccessPolicyResponse where
  rnf UpdateAccessPolicyResponse' {..} =
    Prelude.rnf accessPolicyDetail `Prelude.seq`
      Prelude.rnf httpStatus

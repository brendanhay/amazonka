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
-- Module      : Amazonka.OpenSearchServerless.UpdateSecurityPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an OpenSearch Serverless security policy. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-network.html Network access for Amazon OpenSearch Serverless>
-- and
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-encryption.html Encryption at rest for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.UpdateSecurityPolicy
  ( -- * Creating a Request
    UpdateSecurityPolicy (..),
    newUpdateSecurityPolicy,

    -- * Request Lenses
    updateSecurityPolicy_clientToken,
    updateSecurityPolicy_description,
    updateSecurityPolicy_policy,
    updateSecurityPolicy_name,
    updateSecurityPolicy_policyVersion,
    updateSecurityPolicy_type,

    -- * Destructuring the Response
    UpdateSecurityPolicyResponse (..),
    newUpdateSecurityPolicyResponse,

    -- * Response Lenses
    updateSecurityPolicyResponse_securityPolicyDetail,
    updateSecurityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurityPolicy' smart constructor.
data UpdateSecurityPolicy = UpdateSecurityPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy. Typically used to store information about
    -- the permissions defined in the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy document to use as the content for the new policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    name :: Prelude.Text,
    -- | The version of the policy being updated.
    policyVersion :: Prelude.Text,
    -- | The type of access policy.
    type' :: SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateSecurityPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'description', 'updateSecurityPolicy_description' - A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
--
-- 'policy', 'updateSecurityPolicy_policy' - The JSON policy document to use as the content for the new policy.
--
-- 'name', 'updateSecurityPolicy_name' - The name of the policy.
--
-- 'policyVersion', 'updateSecurityPolicy_policyVersion' - The version of the policy being updated.
--
-- 'type'', 'updateSecurityPolicy_type' - The type of access policy.
newUpdateSecurityPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'policyVersion'
  Prelude.Text ->
  -- | 'type''
  SecurityPolicyType ->
  UpdateSecurityPolicy
newUpdateSecurityPolicy pName_ pPolicyVersion_ pType_ =
  UpdateSecurityPolicy'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      policy = Prelude.Nothing,
      name = pName_,
      policyVersion = pPolicyVersion_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
updateSecurityPolicy_clientToken :: Lens.Lens' UpdateSecurityPolicy (Prelude.Maybe Prelude.Text)
updateSecurityPolicy_clientToken = Lens.lens (\UpdateSecurityPolicy' {clientToken} -> clientToken) (\s@UpdateSecurityPolicy' {} a -> s {clientToken = a} :: UpdateSecurityPolicy)

-- | A description of the policy. Typically used to store information about
-- the permissions defined in the policy.
updateSecurityPolicy_description :: Lens.Lens' UpdateSecurityPolicy (Prelude.Maybe Prelude.Text)
updateSecurityPolicy_description = Lens.lens (\UpdateSecurityPolicy' {description} -> description) (\s@UpdateSecurityPolicy' {} a -> s {description = a} :: UpdateSecurityPolicy)

-- | The JSON policy document to use as the content for the new policy.
updateSecurityPolicy_policy :: Lens.Lens' UpdateSecurityPolicy (Prelude.Maybe Prelude.Text)
updateSecurityPolicy_policy = Lens.lens (\UpdateSecurityPolicy' {policy} -> policy) (\s@UpdateSecurityPolicy' {} a -> s {policy = a} :: UpdateSecurityPolicy)

-- | The name of the policy.
updateSecurityPolicy_name :: Lens.Lens' UpdateSecurityPolicy Prelude.Text
updateSecurityPolicy_name = Lens.lens (\UpdateSecurityPolicy' {name} -> name) (\s@UpdateSecurityPolicy' {} a -> s {name = a} :: UpdateSecurityPolicy)

-- | The version of the policy being updated.
updateSecurityPolicy_policyVersion :: Lens.Lens' UpdateSecurityPolicy Prelude.Text
updateSecurityPolicy_policyVersion = Lens.lens (\UpdateSecurityPolicy' {policyVersion} -> policyVersion) (\s@UpdateSecurityPolicy' {} a -> s {policyVersion = a} :: UpdateSecurityPolicy)

-- | The type of access policy.
updateSecurityPolicy_type :: Lens.Lens' UpdateSecurityPolicy SecurityPolicyType
updateSecurityPolicy_type = Lens.lens (\UpdateSecurityPolicy' {type'} -> type') (\s@UpdateSecurityPolicy' {} a -> s {type' = a} :: UpdateSecurityPolicy)

instance Core.AWSRequest UpdateSecurityPolicy where
  type
    AWSResponse UpdateSecurityPolicy =
      UpdateSecurityPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityPolicyResponse'
            Prelude.<$> (x Data..?> "securityPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecurityPolicy where
  hashWithSalt _salt UpdateSecurityPolicy' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policyVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UpdateSecurityPolicy where
  rnf UpdateSecurityPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders UpdateSecurityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.UpdateSecurityPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSecurityPolicy where
  toJSON UpdateSecurityPolicy' {..} =
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

instance Data.ToPath UpdateSecurityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSecurityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecurityPolicyResponse' smart constructor.
data UpdateSecurityPolicyResponse = UpdateSecurityPolicyResponse'
  { -- | Details about the updated security policy.
    securityPolicyDetail :: Prelude.Maybe SecurityPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityPolicyDetail', 'updateSecurityPolicyResponse_securityPolicyDetail' - Details about the updated security policy.
--
-- 'httpStatus', 'updateSecurityPolicyResponse_httpStatus' - The response's http status code.
newUpdateSecurityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityPolicyResponse
newUpdateSecurityPolicyResponse pHttpStatus_ =
  UpdateSecurityPolicyResponse'
    { securityPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the updated security policy.
updateSecurityPolicyResponse_securityPolicyDetail :: Lens.Lens' UpdateSecurityPolicyResponse (Prelude.Maybe SecurityPolicyDetail)
updateSecurityPolicyResponse_securityPolicyDetail = Lens.lens (\UpdateSecurityPolicyResponse' {securityPolicyDetail} -> securityPolicyDetail) (\s@UpdateSecurityPolicyResponse' {} a -> s {securityPolicyDetail = a} :: UpdateSecurityPolicyResponse)

-- | The response's http status code.
updateSecurityPolicyResponse_httpStatus :: Lens.Lens' UpdateSecurityPolicyResponse Prelude.Int
updateSecurityPolicyResponse_httpStatus = Lens.lens (\UpdateSecurityPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityPolicyResponse' {} a -> s {httpStatus = a} :: UpdateSecurityPolicyResponse)

instance Prelude.NFData UpdateSecurityPolicyResponse where
  rnf UpdateSecurityPolicyResponse' {..} =
    Prelude.rnf securityPolicyDetail
      `Prelude.seq` Prelude.rnf httpStatus

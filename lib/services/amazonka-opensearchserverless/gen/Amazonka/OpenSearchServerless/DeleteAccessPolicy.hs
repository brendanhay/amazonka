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
-- Module      : Amazonka.OpenSearchServerless.DeleteAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenSearch Serverless access policy. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-data-access.html Data access control for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.DeleteAccessPolicy
  ( -- * Creating a Request
    DeleteAccessPolicy (..),
    newDeleteAccessPolicy,

    -- * Request Lenses
    deleteAccessPolicy_clientToken,
    deleteAccessPolicy_name,
    deleteAccessPolicy_type,

    -- * Destructuring the Response
    DeleteAccessPolicyResponse (..),
    newDeleteAccessPolicyResponse,

    -- * Response Lenses
    deleteAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessPolicy' smart constructor.
data DeleteAccessPolicy = DeleteAccessPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy to delete.
    name :: Prelude.Text,
    -- | The type of policy.
    type' :: AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAccessPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'name', 'deleteAccessPolicy_name' - The name of the policy to delete.
--
-- 'type'', 'deleteAccessPolicy_type' - The type of policy.
newDeleteAccessPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AccessPolicyType ->
  DeleteAccessPolicy
newDeleteAccessPolicy pName_ pType_ =
  DeleteAccessPolicy'
    { clientToken = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
deleteAccessPolicy_clientToken :: Lens.Lens' DeleteAccessPolicy (Prelude.Maybe Prelude.Text)
deleteAccessPolicy_clientToken = Lens.lens (\DeleteAccessPolicy' {clientToken} -> clientToken) (\s@DeleteAccessPolicy' {} a -> s {clientToken = a} :: DeleteAccessPolicy)

-- | The name of the policy to delete.
deleteAccessPolicy_name :: Lens.Lens' DeleteAccessPolicy Prelude.Text
deleteAccessPolicy_name = Lens.lens (\DeleteAccessPolicy' {name} -> name) (\s@DeleteAccessPolicy' {} a -> s {name = a} :: DeleteAccessPolicy)

-- | The type of policy.
deleteAccessPolicy_type :: Lens.Lens' DeleteAccessPolicy AccessPolicyType
deleteAccessPolicy_type = Lens.lens (\DeleteAccessPolicy' {type'} -> type') (\s@DeleteAccessPolicy' {} a -> s {type' = a} :: DeleteAccessPolicy)

instance Core.AWSRequest DeleteAccessPolicy where
  type
    AWSResponse DeleteAccessPolicy =
      DeleteAccessPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccessPolicy where
  hashWithSalt _salt DeleteAccessPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DeleteAccessPolicy where
  rnf DeleteAccessPolicy' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf type'

instance Data.ToHeaders DeleteAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.DeleteAccessPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccessPolicy where
  toJSON DeleteAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath DeleteAccessPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessPolicyResponse' smart constructor.
data DeleteAccessPolicyResponse = DeleteAccessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessPolicyResponse_httpStatus' - The response's http status code.
newDeleteAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessPolicyResponse
newDeleteAccessPolicyResponse pHttpStatus_ =
  DeleteAccessPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccessPolicyResponse_httpStatus :: Lens.Lens' DeleteAccessPolicyResponse Prelude.Int
deleteAccessPolicyResponse_httpStatus = Lens.lens (\DeleteAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessPolicyResponse' {} a -> s {httpStatus = a} :: DeleteAccessPolicyResponse)

instance Prelude.NFData DeleteAccessPolicyResponse where
  rnf DeleteAccessPolicyResponse' {..} =
    Prelude.rnf httpStatus

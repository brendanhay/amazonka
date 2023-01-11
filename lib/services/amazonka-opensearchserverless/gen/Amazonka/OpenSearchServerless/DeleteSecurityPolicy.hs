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
-- Module      : Amazonka.OpenSearchServerless.DeleteSecurityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an OpenSearch Serverless security policy.
module Amazonka.OpenSearchServerless.DeleteSecurityPolicy
  ( -- * Creating a Request
    DeleteSecurityPolicy (..),
    newDeleteSecurityPolicy,

    -- * Request Lenses
    deleteSecurityPolicy_clientToken,
    deleteSecurityPolicy_name,
    deleteSecurityPolicy_type,

    -- * Destructuring the Response
    DeleteSecurityPolicyResponse (..),
    newDeleteSecurityPolicyResponse,

    -- * Response Lenses
    deleteSecurityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityPolicy' smart constructor.
data DeleteSecurityPolicy = DeleteSecurityPolicy'
  { -- | Unique, case-sensitive identifier to ensure idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy to delete.
    name :: Prelude.Text,
    -- | The type of policy.
    type' :: SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteSecurityPolicy_clientToken' - Unique, case-sensitive identifier to ensure idempotency of the request.
--
-- 'name', 'deleteSecurityPolicy_name' - The name of the policy to delete.
--
-- 'type'', 'deleteSecurityPolicy_type' - The type of policy.
newDeleteSecurityPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  SecurityPolicyType ->
  DeleteSecurityPolicy
newDeleteSecurityPolicy pName_ pType_ =
  DeleteSecurityPolicy'
    { clientToken =
        Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Unique, case-sensitive identifier to ensure idempotency of the request.
deleteSecurityPolicy_clientToken :: Lens.Lens' DeleteSecurityPolicy (Prelude.Maybe Prelude.Text)
deleteSecurityPolicy_clientToken = Lens.lens (\DeleteSecurityPolicy' {clientToken} -> clientToken) (\s@DeleteSecurityPolicy' {} a -> s {clientToken = a} :: DeleteSecurityPolicy)

-- | The name of the policy to delete.
deleteSecurityPolicy_name :: Lens.Lens' DeleteSecurityPolicy Prelude.Text
deleteSecurityPolicy_name = Lens.lens (\DeleteSecurityPolicy' {name} -> name) (\s@DeleteSecurityPolicy' {} a -> s {name = a} :: DeleteSecurityPolicy)

-- | The type of policy.
deleteSecurityPolicy_type :: Lens.Lens' DeleteSecurityPolicy SecurityPolicyType
deleteSecurityPolicy_type = Lens.lens (\DeleteSecurityPolicy' {type'} -> type') (\s@DeleteSecurityPolicy' {} a -> s {type' = a} :: DeleteSecurityPolicy)

instance Core.AWSRequest DeleteSecurityPolicy where
  type
    AWSResponse DeleteSecurityPolicy =
      DeleteSecurityPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSecurityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecurityPolicy where
  hashWithSalt _salt DeleteSecurityPolicy' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DeleteSecurityPolicy where
  rnf DeleteSecurityPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders DeleteSecurityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.DeleteSecurityPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSecurityPolicy where
  toJSON DeleteSecurityPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath DeleteSecurityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSecurityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecurityPolicyResponse' smart constructor.
data DeleteSecurityPolicyResponse = DeleteSecurityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSecurityPolicyResponse_httpStatus' - The response's http status code.
newDeleteSecurityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecurityPolicyResponse
newDeleteSecurityPolicyResponse pHttpStatus_ =
  DeleteSecurityPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSecurityPolicyResponse_httpStatus :: Lens.Lens' DeleteSecurityPolicyResponse Prelude.Int
deleteSecurityPolicyResponse_httpStatus = Lens.lens (\DeleteSecurityPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteSecurityPolicyResponse' {} a -> s {httpStatus = a} :: DeleteSecurityPolicyResponse)

instance Prelude.NFData DeleteSecurityPolicyResponse where
  rnf DeleteSecurityPolicyResponse' {..} =
    Prelude.rnf httpStatus

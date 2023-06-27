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
-- Module      : Amazonka.VerifiedPermissions.DeletePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the policy store.
--
-- This operation is idempotent; if you specify a policy that doesn\'t
-- exist, the request response returns a successful @HTTP 200@ status code.
module Amazonka.VerifiedPermissions.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_policyStoreId,
    deletePolicy_policyId,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,

    -- * Response Lenses
    deletePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | Specifies the ID of the policy store that contains the policy that you
    -- want to delete.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy that you want to delete.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'deletePolicy_policyStoreId' - Specifies the ID of the policy store that contains the policy that you
-- want to delete.
--
-- 'policyId', 'deletePolicy_policyId' - Specifies the ID of the policy that you want to delete.
newDeletePolicy ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pPolicyStoreId_ pPolicyId_ =
  DeletePolicy'
    { policyStoreId = pPolicyStoreId_,
      policyId = pPolicyId_
    }

-- | Specifies the ID of the policy store that contains the policy that you
-- want to delete.
deletePolicy_policyStoreId :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyStoreId = Lens.lens (\DeletePolicy' {policyStoreId} -> policyStoreId) (\s@DeletePolicy' {} a -> s {policyStoreId = a} :: DeletePolicy)

-- | Specifies the ID of the policy that you want to delete.
deletePolicy_policyId :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyId = Lens.lens (\DeletePolicy' {policyId} -> policyId) (\s@DeletePolicy' {} a -> s {policyId = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePolicy where
  hashWithSalt _salt DeletePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData DeletePolicy where
  rnf DeletePolicy' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders DeletePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.DeletePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("policyId" Data..= policyId)
          ]
      )

instance Data.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePolicyResponse_httpStatus' - The response's http status code.
newDeletePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePolicyResponse
newDeletePolicyResponse pHttpStatus_ =
  DeletePolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePolicyResponse_httpStatus :: Lens.Lens' DeletePolicyResponse Prelude.Int
deletePolicyResponse_httpStatus = Lens.lens (\DeletePolicyResponse' {httpStatus} -> httpStatus) (\s@DeletePolicyResponse' {} a -> s {httpStatus = a} :: DeletePolicyResponse)

instance Prelude.NFData DeletePolicyResponse where
  rnf DeletePolicyResponse' {..} =
    Prelude.rnf httpStatus

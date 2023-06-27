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
-- Module      : Amazonka.VerifiedPermissions.DeletePolicyStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy store.
--
-- This operation is idempotent. If you specify a policy store that does
-- not exist, the request response will still return a successful HTTP 200
-- status code.
module Amazonka.VerifiedPermissions.DeletePolicyStore
  ( -- * Creating a Request
    DeletePolicyStore (..),
    newDeletePolicyStore,

    -- * Request Lenses
    deletePolicyStore_policyStoreId,

    -- * Destructuring the Response
    DeletePolicyStoreResponse (..),
    newDeletePolicyStoreResponse,

    -- * Response Lenses
    deletePolicyStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newDeletePolicyStore' smart constructor.
data DeletePolicyStore = DeletePolicyStore'
  { -- | Specifies the ID of the policy store that you want to delete.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'deletePolicyStore_policyStoreId' - Specifies the ID of the policy store that you want to delete.
newDeletePolicyStore ::
  -- | 'policyStoreId'
  Prelude.Text ->
  DeletePolicyStore
newDeletePolicyStore pPolicyStoreId_ =
  DeletePolicyStore' {policyStoreId = pPolicyStoreId_}

-- | Specifies the ID of the policy store that you want to delete.
deletePolicyStore_policyStoreId :: Lens.Lens' DeletePolicyStore Prelude.Text
deletePolicyStore_policyStoreId = Lens.lens (\DeletePolicyStore' {policyStoreId} -> policyStoreId) (\s@DeletePolicyStore' {} a -> s {policyStoreId = a} :: DeletePolicyStore)

instance Core.AWSRequest DeletePolicyStore where
  type
    AWSResponse DeletePolicyStore =
      DeletePolicyStoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePolicyStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePolicyStore where
  hashWithSalt _salt DeletePolicyStore' {..} =
    _salt `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData DeletePolicyStore where
  rnf DeletePolicyStore' {..} =
    Prelude.rnf policyStoreId

instance Data.ToHeaders DeletePolicyStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.DeletePolicyStore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePolicyStore where
  toJSON DeletePolicyStore' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath DeletePolicyStore where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePolicyStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyStoreResponse' smart constructor.
data DeletePolicyStoreResponse = DeletePolicyStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePolicyStoreResponse_httpStatus' - The response's http status code.
newDeletePolicyStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePolicyStoreResponse
newDeletePolicyStoreResponse pHttpStatus_ =
  DeletePolicyStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePolicyStoreResponse_httpStatus :: Lens.Lens' DeletePolicyStoreResponse Prelude.Int
deletePolicyStoreResponse_httpStatus = Lens.lens (\DeletePolicyStoreResponse' {httpStatus} -> httpStatus) (\s@DeletePolicyStoreResponse' {} a -> s {httpStatus = a} :: DeletePolicyStoreResponse)

instance Prelude.NFData DeletePolicyStoreResponse where
  rnf DeletePolicyStoreResponse' {..} =
    Prelude.rnf httpStatus

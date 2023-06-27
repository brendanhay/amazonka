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
-- Module      : Amazonka.VerifiedPermissions.DeletePolicyTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy template from the policy store.
--
-- This operation also deletes any policies that were created from the
-- specified policy template. Those policies are immediately removed from
-- all future API responses, and are asynchronously deleted from the policy
-- store.
module Amazonka.VerifiedPermissions.DeletePolicyTemplate
  ( -- * Creating a Request
    DeletePolicyTemplate (..),
    newDeletePolicyTemplate,

    -- * Request Lenses
    deletePolicyTemplate_policyStoreId,
    deletePolicyTemplate_policyTemplateId,

    -- * Destructuring the Response
    DeletePolicyTemplateResponse (..),
    newDeletePolicyTemplateResponse,

    -- * Response Lenses
    deletePolicyTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newDeletePolicyTemplate' smart constructor.
data DeletePolicyTemplate = DeletePolicyTemplate'
  { -- | Specifies the ID of the policy store that contains the policy template
    -- that you want to delete.
    policyStoreId :: Prelude.Text,
    -- | Specifies the ID of the policy template that you want to delete.
    policyTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'deletePolicyTemplate_policyStoreId' - Specifies the ID of the policy store that contains the policy template
-- that you want to delete.
--
-- 'policyTemplateId', 'deletePolicyTemplate_policyTemplateId' - Specifies the ID of the policy template that you want to delete.
newDeletePolicyTemplate ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  DeletePolicyTemplate
newDeletePolicyTemplate
  pPolicyStoreId_
  pPolicyTemplateId_ =
    DeletePolicyTemplate'
      { policyStoreId =
          pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_
      }

-- | Specifies the ID of the policy store that contains the policy template
-- that you want to delete.
deletePolicyTemplate_policyStoreId :: Lens.Lens' DeletePolicyTemplate Prelude.Text
deletePolicyTemplate_policyStoreId = Lens.lens (\DeletePolicyTemplate' {policyStoreId} -> policyStoreId) (\s@DeletePolicyTemplate' {} a -> s {policyStoreId = a} :: DeletePolicyTemplate)

-- | Specifies the ID of the policy template that you want to delete.
deletePolicyTemplate_policyTemplateId :: Lens.Lens' DeletePolicyTemplate Prelude.Text
deletePolicyTemplate_policyTemplateId = Lens.lens (\DeletePolicyTemplate' {policyTemplateId} -> policyTemplateId) (\s@DeletePolicyTemplate' {} a -> s {policyTemplateId = a} :: DeletePolicyTemplate)

instance Core.AWSRequest DeletePolicyTemplate where
  type
    AWSResponse DeletePolicyTemplate =
      DeletePolicyTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePolicyTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePolicyTemplate where
  hashWithSalt _salt DeletePolicyTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyTemplateId

instance Prelude.NFData DeletePolicyTemplate where
  rnf DeletePolicyTemplate' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId

instance Data.ToHeaders DeletePolicyTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.DeletePolicyTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePolicyTemplate where
  toJSON DeletePolicyTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just
              ("policyTemplateId" Data..= policyTemplateId)
          ]
      )

instance Data.ToPath DeletePolicyTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePolicyTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyTemplateResponse' smart constructor.
data DeletePolicyTemplateResponse = DeletePolicyTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePolicyTemplateResponse_httpStatus' - The response's http status code.
newDeletePolicyTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePolicyTemplateResponse
newDeletePolicyTemplateResponse pHttpStatus_ =
  DeletePolicyTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePolicyTemplateResponse_httpStatus :: Lens.Lens' DeletePolicyTemplateResponse Prelude.Int
deletePolicyTemplateResponse_httpStatus = Lens.lens (\DeletePolicyTemplateResponse' {httpStatus} -> httpStatus) (\s@DeletePolicyTemplateResponse' {} a -> s {httpStatus = a} :: DeletePolicyTemplateResponse)

instance Prelude.NFData DeletePolicyTemplateResponse where
  rnf DeletePolicyTemplateResponse' {..} =
    Prelude.rnf httpStatus

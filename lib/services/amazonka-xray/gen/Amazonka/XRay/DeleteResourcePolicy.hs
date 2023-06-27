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
-- Module      : Amazonka.XRay.DeleteResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy from the target Amazon Web Services account.
module Amazonka.XRay.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_policyRevisionId,
    deleteResourcePolicy_policyName,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | Specifies a specific policy revision to delete. Provide a
    -- @PolicyRevisionId@ to ensure an atomic delete operation. If the provided
    -- revision id does not match the latest policy revision id, an
    -- @InvalidPolicyRevisionIdException@ exception is returned.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource policy to delete.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'deleteResourcePolicy_policyRevisionId' - Specifies a specific policy revision to delete. Provide a
-- @PolicyRevisionId@ to ensure an atomic delete operation. If the provided
-- revision id does not match the latest policy revision id, an
-- @InvalidPolicyRevisionIdException@ exception is returned.
--
-- 'policyName', 'deleteResourcePolicy_policyName' - The name of the resource policy to delete.
newDeleteResourcePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pPolicyName_ =
  DeleteResourcePolicy'
    { policyRevisionId =
        Prelude.Nothing,
      policyName = pPolicyName_
    }

-- | Specifies a specific policy revision to delete. Provide a
-- @PolicyRevisionId@ to ensure an atomic delete operation. If the provided
-- revision id does not match the latest policy revision id, an
-- @InvalidPolicyRevisionIdException@ exception is returned.
deleteResourcePolicy_policyRevisionId :: Lens.Lens' DeleteResourcePolicy (Prelude.Maybe Prelude.Text)
deleteResourcePolicy_policyRevisionId = Lens.lens (\DeleteResourcePolicy' {policyRevisionId} -> policyRevisionId) (\s@DeleteResourcePolicy' {} a -> s {policyRevisionId = a} :: DeleteResourcePolicy)

-- | The name of the resource policy to delete.
deleteResourcePolicy_policyName :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_policyName = Lens.lens (\DeleteResourcePolicy' {policyName} -> policyName) (\s@DeleteResourcePolicy' {} a -> s {policyName = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePolicy where
  hashWithSalt _salt DeleteResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteResourcePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyRevisionId" Data..=)
              Prelude.<$> policyRevisionId,
            Prelude.Just ("PolicyName" Data..= policyName)
          ]
      )

instance Data.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/DeleteResourcePolicy"

instance Data.ToQuery DeleteResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Prelude.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Prelude.NFData DeleteResourcePolicyResponse where
  rnf DeleteResourcePolicyResponse' {..} =
    Prelude.rnf httpStatus

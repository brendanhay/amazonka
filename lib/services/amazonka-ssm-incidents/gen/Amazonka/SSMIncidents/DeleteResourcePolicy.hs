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
-- Module      : Amazonka.SSMIncidents.DeleteResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource policy that Resource Access Manager uses to share
-- your Incident Manager resource.
module Amazonka.SSMIncidents.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_policyId,
    deleteResourcePolicy_resourceArn,

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
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The ID of the resource policy you\'re deleting.
    policyId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource you\'re deleting the
    -- policy from.
    resourceArn :: Prelude.Text
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
-- 'policyId', 'deleteResourcePolicy_policyId' - The ID of the resource policy you\'re deleting.
--
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the resource you\'re deleting the
-- policy from.
newDeleteResourcePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pPolicyId_ pResourceArn_ =
  DeleteResourcePolicy'
    { policyId = pPolicyId_,
      resourceArn = pResourceArn_
    }

-- | The ID of the resource policy you\'re deleting.
deleteResourcePolicy_policyId :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_policyId = Lens.lens (\DeleteResourcePolicy' {policyId} -> policyId) (\s@DeleteResourcePolicy' {} a -> s {policyId = a} :: DeleteResourcePolicy)

-- | The Amazon Resource Name (ARN) of the resource you\'re deleting the
-- policy from.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

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
    _salt `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("policyId" Data..= policyId),
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/deleteResourcePolicy"

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

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
-- Module      : Amazonka.SSM.DeleteResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Systems Manager resource policy. A resource policy helps you
-- to define the IAM entity (for example, an Amazon Web Services account)
-- that can manage your Systems Manager resources. Currently,
-- @OpsItemGroup@ is the only resource that supports Systems Manager
-- resource policies. The resource policy for @OpsItemGroup@ enables Amazon
-- Web Services accounts to view and interact with OpsCenter operational
-- work items (OpsItems).
module Amazonka.SSM.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicy_policyId,
    deleteResourcePolicy_policyHash,

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
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | Amazon Resource Name (ARN) of the resource to which the policies are
    -- attached.
    resourceArn :: Prelude.Text,
    -- | The policy ID.
    policyId :: Prelude.Text,
    -- | ID of the current policy version. The hash helps to prevent multiple
    -- calls from attempting to overwrite a policy.
    policyHash :: Prelude.Text
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
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - Amazon Resource Name (ARN) of the resource to which the policies are
-- attached.
--
-- 'policyId', 'deleteResourcePolicy_policyId' - The policy ID.
--
-- 'policyHash', 'deleteResourcePolicy_policyHash' - ID of the current policy version. The hash helps to prevent multiple
-- calls from attempting to overwrite a policy.
newDeleteResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'policyHash'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy
  pResourceArn_
  pPolicyId_
  pPolicyHash_ =
    DeleteResourcePolicy'
      { resourceArn = pResourceArn_,
        policyId = pPolicyId_,
        policyHash = pPolicyHash_
      }

-- | Amazon Resource Name (ARN) of the resource to which the policies are
-- attached.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

-- | The policy ID.
deleteResourcePolicy_policyId :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_policyId = Lens.lens (\DeleteResourcePolicy' {policyId} -> policyId) (\s@DeleteResourcePolicy' {} a -> s {policyId = a} :: DeleteResourcePolicy)

-- | ID of the current policy version. The hash helps to prevent multiple
-- calls from attempting to overwrite a policy.
deleteResourcePolicy_policyHash :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_policyHash = Lens.lens (\DeleteResourcePolicy' {policyHash} -> policyHash) (\s@DeleteResourcePolicy' {} a -> s {policyHash = a} :: DeleteResourcePolicy)

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
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyHash

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyHash

instance Data.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeleteResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("PolicyId" Data..= policyId),
            Prelude.Just ("PolicyHash" Data..= policyHash)
          ]
      )

instance Data.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/"

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

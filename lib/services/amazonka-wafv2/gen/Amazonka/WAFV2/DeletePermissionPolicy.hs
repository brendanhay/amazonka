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
-- Module      : Amazonka.WAFV2.DeletePermissionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an IAM policy from the specified rule group.
--
-- You must be the owner of the rule group to perform this operation.
module Amazonka.WAFV2.DeletePermissionPolicy
  ( -- * Creating a Request
    DeletePermissionPolicy (..),
    newDeletePermissionPolicy,

    -- * Request Lenses
    deletePermissionPolicy_resourceArn,

    -- * Destructuring the Response
    DeletePermissionPolicyResponse (..),
    newDeletePermissionPolicyResponse,

    -- * Response Lenses
    deletePermissionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeletePermissionPolicy' smart constructor.
data DeletePermissionPolicy = DeletePermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the rule group from which you want to
    -- delete the policy.
    --
    -- You must be the owner of the rule group to perform this operation.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deletePermissionPolicy_resourceArn' - The Amazon Resource Name (ARN) of the rule group from which you want to
-- delete the policy.
--
-- You must be the owner of the rule group to perform this operation.
newDeletePermissionPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeletePermissionPolicy
newDeletePermissionPolicy pResourceArn_ =
  DeletePermissionPolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the rule group from which you want to
-- delete the policy.
--
-- You must be the owner of the rule group to perform this operation.
deletePermissionPolicy_resourceArn :: Lens.Lens' DeletePermissionPolicy Prelude.Text
deletePermissionPolicy_resourceArn = Lens.lens (\DeletePermissionPolicy' {resourceArn} -> resourceArn) (\s@DeletePermissionPolicy' {} a -> s {resourceArn = a} :: DeletePermissionPolicy)

instance Core.AWSRequest DeletePermissionPolicy where
  type
    AWSResponse DeletePermissionPolicy =
      DeletePermissionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePermissionPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermissionPolicy where
  hashWithSalt _salt DeletePermissionPolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeletePermissionPolicy where
  rnf DeletePermissionPolicy' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders DeletePermissionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeletePermissionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePermissionPolicy where
  toJSON DeletePermissionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DeletePermissionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePermissionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePermissionPolicyResponse' smart constructor.
data DeletePermissionPolicyResponse = DeletePermissionPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePermissionPolicyResponse_httpStatus' - The response's http status code.
newDeletePermissionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionPolicyResponse
newDeletePermissionPolicyResponse pHttpStatus_ =
  DeletePermissionPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePermissionPolicyResponse_httpStatus :: Lens.Lens' DeletePermissionPolicyResponse Prelude.Int
deletePermissionPolicyResponse_httpStatus = Lens.lens (\DeletePermissionPolicyResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionPolicyResponse' {} a -> s {httpStatus = a} :: DeletePermissionPolicyResponse)

instance
  Prelude.NFData
    DeletePermissionPolicyResponse
  where
  rnf DeletePermissionPolicyResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.VPCLattice.DeleteAuthPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified auth policy. If an auth is set to @AWS_IAM@ and
-- the auth policy is deleted, all requests will be denied by default. If
-- you are trying to remove the auth policy completely, you must set the
-- auth_type to @NONE@. If auth is enabled on the resource, but no auth
-- policy is set, all requests will be denied.
module Amazonka.VPCLattice.DeleteAuthPolicy
  ( -- * Creating a Request
    DeleteAuthPolicy (..),
    newDeleteAuthPolicy,

    -- * Request Lenses
    deleteAuthPolicy_resourceIdentifier,

    -- * Destructuring the Response
    DeleteAuthPolicyResponse (..),
    newDeleteAuthPolicyResponse,

    -- * Response Lenses
    deleteAuthPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteAuthPolicy' smart constructor.
data DeleteAuthPolicy = DeleteAuthPolicy'
  { -- | The ID or Amazon Resource Name (ARN) of the resource.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'deleteAuthPolicy_resourceIdentifier' - The ID or Amazon Resource Name (ARN) of the resource.
newDeleteAuthPolicy ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  DeleteAuthPolicy
newDeleteAuthPolicy pResourceIdentifier_ =
  DeleteAuthPolicy'
    { resourceIdentifier =
        pResourceIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the resource.
deleteAuthPolicy_resourceIdentifier :: Lens.Lens' DeleteAuthPolicy Prelude.Text
deleteAuthPolicy_resourceIdentifier = Lens.lens (\DeleteAuthPolicy' {resourceIdentifier} -> resourceIdentifier) (\s@DeleteAuthPolicy' {} a -> s {resourceIdentifier = a} :: DeleteAuthPolicy)

instance Core.AWSRequest DeleteAuthPolicy where
  type
    AWSResponse DeleteAuthPolicy =
      DeleteAuthPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAuthPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAuthPolicy where
  hashWithSalt _salt DeleteAuthPolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData DeleteAuthPolicy where
  rnf DeleteAuthPolicy' {..} =
    Prelude.rnf resourceIdentifier

instance Data.ToHeaders DeleteAuthPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAuthPolicy where
  toPath DeleteAuthPolicy' {..} =
    Prelude.mconcat
      ["/authpolicy/", Data.toBS resourceIdentifier]

instance Data.ToQuery DeleteAuthPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAuthPolicyResponse' smart constructor.
data DeleteAuthPolicyResponse = DeleteAuthPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAuthPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAuthPolicyResponse_httpStatus' - The response's http status code.
newDeleteAuthPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAuthPolicyResponse
newDeleteAuthPolicyResponse pHttpStatus_ =
  DeleteAuthPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAuthPolicyResponse_httpStatus :: Lens.Lens' DeleteAuthPolicyResponse Prelude.Int
deleteAuthPolicyResponse_httpStatus = Lens.lens (\DeleteAuthPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteAuthPolicyResponse' {} a -> s {httpStatus = a} :: DeleteAuthPolicyResponse)

instance Prelude.NFData DeleteAuthPolicyResponse where
  rnf DeleteAuthPolicyResponse' {..} =
    Prelude.rnf httpStatus

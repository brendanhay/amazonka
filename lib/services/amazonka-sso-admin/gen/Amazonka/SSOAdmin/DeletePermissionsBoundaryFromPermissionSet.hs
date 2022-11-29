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
-- Module      : Amazonka.SSOAdmin.DeletePermissionsBoundaryFromPermissionSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary from a specified PermissionSet.
module Amazonka.SSOAdmin.DeletePermissionsBoundaryFromPermissionSet
  ( -- * Creating a Request
    DeletePermissionsBoundaryFromPermissionSet (..),
    newDeletePermissionsBoundaryFromPermissionSet,

    -- * Request Lenses
    deletePermissionsBoundaryFromPermissionSet_instanceArn,
    deletePermissionsBoundaryFromPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    DeletePermissionsBoundaryFromPermissionSetResponse (..),
    newDeletePermissionsBoundaryFromPermissionSetResponse,

    -- * Response Lenses
    deletePermissionsBoundaryFromPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDeletePermissionsBoundaryFromPermissionSet' smart constructor.
data DeletePermissionsBoundaryFromPermissionSet = DeletePermissionsBoundaryFromPermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | The ARN of the @PermissionSet@.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionsBoundaryFromPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'deletePermissionsBoundaryFromPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'deletePermissionsBoundaryFromPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
newDeletePermissionsBoundaryFromPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  DeletePermissionsBoundaryFromPermissionSet
newDeletePermissionsBoundaryFromPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    DeletePermissionsBoundaryFromPermissionSet'
      { instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
deletePermissionsBoundaryFromPermissionSet_instanceArn :: Lens.Lens' DeletePermissionsBoundaryFromPermissionSet Prelude.Text
deletePermissionsBoundaryFromPermissionSet_instanceArn = Lens.lens (\DeletePermissionsBoundaryFromPermissionSet' {instanceArn} -> instanceArn) (\s@DeletePermissionsBoundaryFromPermissionSet' {} a -> s {instanceArn = a} :: DeletePermissionsBoundaryFromPermissionSet)

-- | The ARN of the @PermissionSet@.
deletePermissionsBoundaryFromPermissionSet_permissionSetArn :: Lens.Lens' DeletePermissionsBoundaryFromPermissionSet Prelude.Text
deletePermissionsBoundaryFromPermissionSet_permissionSetArn = Lens.lens (\DeletePermissionsBoundaryFromPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@DeletePermissionsBoundaryFromPermissionSet' {} a -> s {permissionSetArn = a} :: DeletePermissionsBoundaryFromPermissionSet)

instance
  Core.AWSRequest
    DeletePermissionsBoundaryFromPermissionSet
  where
  type
    AWSResponse
      DeletePermissionsBoundaryFromPermissionSet =
      DeletePermissionsBoundaryFromPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePermissionsBoundaryFromPermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeletePermissionsBoundaryFromPermissionSet
  where
  hashWithSalt
    _salt
    DeletePermissionsBoundaryFromPermissionSet' {..} =
      _salt `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    DeletePermissionsBoundaryFromPermissionSet
  where
  rnf DeletePermissionsBoundaryFromPermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Core.ToHeaders
    DeletePermissionsBoundaryFromPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.DeletePermissionsBoundaryFromPermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DeletePermissionsBoundaryFromPermissionSet
  where
  toJSON
    DeletePermissionsBoundaryFromPermissionSet' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just ("InstanceArn" Core..= instanceArn),
              Prelude.Just
                ("PermissionSetArn" Core..= permissionSetArn)
            ]
        )

instance
  Core.ToPath
    DeletePermissionsBoundaryFromPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeletePermissionsBoundaryFromPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePermissionsBoundaryFromPermissionSetResponse' smart constructor.
data DeletePermissionsBoundaryFromPermissionSetResponse = DeletePermissionsBoundaryFromPermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionsBoundaryFromPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePermissionsBoundaryFromPermissionSetResponse_httpStatus' - The response's http status code.
newDeletePermissionsBoundaryFromPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionsBoundaryFromPermissionSetResponse
newDeletePermissionsBoundaryFromPermissionSetResponse
  pHttpStatus_ =
    DeletePermissionsBoundaryFromPermissionSetResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deletePermissionsBoundaryFromPermissionSetResponse_httpStatus :: Lens.Lens' DeletePermissionsBoundaryFromPermissionSetResponse Prelude.Int
deletePermissionsBoundaryFromPermissionSetResponse_httpStatus = Lens.lens (\DeletePermissionsBoundaryFromPermissionSetResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionsBoundaryFromPermissionSetResponse' {} a -> s {httpStatus = a} :: DeletePermissionsBoundaryFromPermissionSetResponse)

instance
  Prelude.NFData
    DeletePermissionsBoundaryFromPermissionSetResponse
  where
  rnf
    DeletePermissionsBoundaryFromPermissionSetResponse' {..} =
      Prelude.rnf httpStatus

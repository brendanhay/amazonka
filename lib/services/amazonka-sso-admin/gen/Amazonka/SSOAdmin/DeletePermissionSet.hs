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
-- Module      : Amazonka.SSOAdmin.DeletePermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified permission set.
module Amazonka.SSOAdmin.DeletePermissionSet
  ( -- * Creating a Request
    DeletePermissionSet (..),
    newDeletePermissionSet,

    -- * Request Lenses
    deletePermissionSet_instanceArn,
    deletePermissionSet_permissionSetArn,

    -- * Destructuring the Response
    DeletePermissionSetResponse (..),
    newDeletePermissionSetResponse,

    -- * Response Lenses
    deletePermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDeletePermissionSet' smart constructor.
data DeletePermissionSet = DeletePermissionSet'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the permission set that should be deleted.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'deletePermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'deletePermissionSet_permissionSetArn' - The ARN of the permission set that should be deleted.
newDeletePermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  DeletePermissionSet
newDeletePermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    DeletePermissionSet'
      { instanceArn = pInstanceArn_,
        permissionSetArn = pPermissionSetArn_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
deletePermissionSet_instanceArn :: Lens.Lens' DeletePermissionSet Prelude.Text
deletePermissionSet_instanceArn = Lens.lens (\DeletePermissionSet' {instanceArn} -> instanceArn) (\s@DeletePermissionSet' {} a -> s {instanceArn = a} :: DeletePermissionSet)

-- | The ARN of the permission set that should be deleted.
deletePermissionSet_permissionSetArn :: Lens.Lens' DeletePermissionSet Prelude.Text
deletePermissionSet_permissionSetArn = Lens.lens (\DeletePermissionSet' {permissionSetArn} -> permissionSetArn) (\s@DeletePermissionSet' {} a -> s {permissionSetArn = a} :: DeletePermissionSet)

instance Core.AWSRequest DeletePermissionSet where
  type
    AWSResponse DeletePermissionSet =
      DeletePermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePermissionSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermissionSet where
  hashWithSalt _salt DeletePermissionSet' {..} =
    _salt
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` permissionSetArn

instance Prelude.NFData DeletePermissionSet where
  rnf DeletePermissionSet' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance Data.ToHeaders DeletePermissionSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.DeletePermissionSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePermissionSet where
  toJSON DeletePermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn)
          ]
      )

instance Data.ToPath DeletePermissionSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePermissionSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePermissionSetResponse' smart constructor.
data DeletePermissionSetResponse = DeletePermissionSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePermissionSetResponse_httpStatus' - The response's http status code.
newDeletePermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionSetResponse
newDeletePermissionSetResponse pHttpStatus_ =
  DeletePermissionSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePermissionSetResponse_httpStatus :: Lens.Lens' DeletePermissionSetResponse Prelude.Int
deletePermissionSetResponse_httpStatus = Lens.lens (\DeletePermissionSetResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionSetResponse' {} a -> s {httpStatus = a} :: DeletePermissionSetResponse)

instance Prelude.NFData DeletePermissionSetResponse where
  rnf DeletePermissionSetResponse' {..} =
    Prelude.rnf httpStatus

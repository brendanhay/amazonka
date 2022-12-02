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
-- Module      : Amazonka.FinSpaceData.DeletePermissionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a permission group. This action is irreversible.
module Amazonka.FinSpaceData.DeletePermissionGroup
  ( -- * Creating a Request
    DeletePermissionGroup (..),
    newDeletePermissionGroup,

    -- * Request Lenses
    deletePermissionGroup_clientToken,
    deletePermissionGroup_permissionGroupId,

    -- * Destructuring the Response
    DeletePermissionGroupResponse (..),
    newDeletePermissionGroupResponse,

    -- * Response Lenses
    deletePermissionGroupResponse_permissionGroupId,
    deletePermissionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePermissionGroup' smart constructor.
data DeletePermissionGroup = DeletePermissionGroup'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the permission group that you want to delete.
    permissionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deletePermissionGroup_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'permissionGroupId', 'deletePermissionGroup_permissionGroupId' - The unique identifier for the permission group that you want to delete.
newDeletePermissionGroup ::
  -- | 'permissionGroupId'
  Prelude.Text ->
  DeletePermissionGroup
newDeletePermissionGroup pPermissionGroupId_ =
  DeletePermissionGroup'
    { clientToken =
        Prelude.Nothing,
      permissionGroupId = pPermissionGroupId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
deletePermissionGroup_clientToken :: Lens.Lens' DeletePermissionGroup (Prelude.Maybe Prelude.Text)
deletePermissionGroup_clientToken = Lens.lens (\DeletePermissionGroup' {clientToken} -> clientToken) (\s@DeletePermissionGroup' {} a -> s {clientToken = a} :: DeletePermissionGroup)

-- | The unique identifier for the permission group that you want to delete.
deletePermissionGroup_permissionGroupId :: Lens.Lens' DeletePermissionGroup Prelude.Text
deletePermissionGroup_permissionGroupId = Lens.lens (\DeletePermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@DeletePermissionGroup' {} a -> s {permissionGroupId = a} :: DeletePermissionGroup)

instance Core.AWSRequest DeletePermissionGroup where
  type
    AWSResponse DeletePermissionGroup =
      DeletePermissionGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePermissionGroupResponse'
            Prelude.<$> (x Data..?> "permissionGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePermissionGroup where
  hashWithSalt _salt DeletePermissionGroup' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionGroupId

instance Prelude.NFData DeletePermissionGroup where
  rnf DeletePermissionGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionGroupId

instance Data.ToHeaders DeletePermissionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePermissionGroup where
  toPath DeletePermissionGroup' {..} =
    Prelude.mconcat
      ["/permission-group/", Data.toBS permissionGroupId]

instance Data.ToQuery DeletePermissionGroup where
  toQuery DeletePermissionGroup' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeletePermissionGroupResponse' smart constructor.
data DeletePermissionGroupResponse = DeletePermissionGroupResponse'
  { -- | The unique identifier for the deleted permission group.
    permissionGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionGroupId', 'deletePermissionGroupResponse_permissionGroupId' - The unique identifier for the deleted permission group.
--
-- 'httpStatus', 'deletePermissionGroupResponse_httpStatus' - The response's http status code.
newDeletePermissionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePermissionGroupResponse
newDeletePermissionGroupResponse pHttpStatus_ =
  DeletePermissionGroupResponse'
    { permissionGroupId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the deleted permission group.
deletePermissionGroupResponse_permissionGroupId :: Lens.Lens' DeletePermissionGroupResponse (Prelude.Maybe Prelude.Text)
deletePermissionGroupResponse_permissionGroupId = Lens.lens (\DeletePermissionGroupResponse' {permissionGroupId} -> permissionGroupId) (\s@DeletePermissionGroupResponse' {} a -> s {permissionGroupId = a} :: DeletePermissionGroupResponse)

-- | The response's http status code.
deletePermissionGroupResponse_httpStatus :: Lens.Lens' DeletePermissionGroupResponse Prelude.Int
deletePermissionGroupResponse_httpStatus = Lens.lens (\DeletePermissionGroupResponse' {httpStatus} -> httpStatus) (\s@DeletePermissionGroupResponse' {} a -> s {httpStatus = a} :: DeletePermissionGroupResponse)

instance Prelude.NFData DeletePermissionGroupResponse where
  rnf DeletePermissionGroupResponse' {..} =
    Prelude.rnf permissionGroupId
      `Prelude.seq` Prelude.rnf httpStatus

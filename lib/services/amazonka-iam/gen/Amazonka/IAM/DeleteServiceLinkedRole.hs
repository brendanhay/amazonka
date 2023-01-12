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
-- Module      : Amazonka.IAM.DeleteServiceLinkedRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a service-linked role deletion request and returns a
-- @DeletionTaskId@, which you can use to check the status of the deletion.
-- Before you call this operation, confirm that the role has no active
-- sessions and that any resources used by the role in the linked service
-- are deleted. If you call this operation more than once for the same
-- service-linked role and an earlier deletion task is not complete, then
-- the @DeletionTaskId@ of the earlier request is returned.
--
-- If you submit a deletion request for a service-linked role whose linked
-- service is still accessing a resource, then the deletion task fails. If
-- it fails, the GetServiceLinkedRoleDeletionStatus operation returns the
-- reason for the failure, usually including the resources that must be
-- deleted. To delete the service-linked role, you must first remove those
-- resources from the linked service and then submit the deletion request
-- again. Resources are specific to the service that is linked to the role.
-- For more information about removing resources from a service, see the
-- <http://docs.aws.amazon.com/ Amazon Web Services documentation> for your
-- service.
--
-- For more information about service-linked roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role Roles terms and concepts: Amazon Web Services service-linked role>
-- in the /IAM User Guide/.
module Amazonka.IAM.DeleteServiceLinkedRole
  ( -- * Creating a Request
    DeleteServiceLinkedRole (..),
    newDeleteServiceLinkedRole,

    -- * Request Lenses
    deleteServiceLinkedRole_roleName,

    -- * Destructuring the Response
    DeleteServiceLinkedRoleResponse (..),
    newDeleteServiceLinkedRoleResponse,

    -- * Response Lenses
    deleteServiceLinkedRoleResponse_httpStatus,
    deleteServiceLinkedRoleResponse_deletionTaskId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteServiceLinkedRole' smart constructor.
data DeleteServiceLinkedRole = DeleteServiceLinkedRole'
  { -- | The name of the service-linked role to be deleted.
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceLinkedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'deleteServiceLinkedRole_roleName' - The name of the service-linked role to be deleted.
newDeleteServiceLinkedRole ::
  -- | 'roleName'
  Prelude.Text ->
  DeleteServiceLinkedRole
newDeleteServiceLinkedRole pRoleName_ =
  DeleteServiceLinkedRole' {roleName = pRoleName_}

-- | The name of the service-linked role to be deleted.
deleteServiceLinkedRole_roleName :: Lens.Lens' DeleteServiceLinkedRole Prelude.Text
deleteServiceLinkedRole_roleName = Lens.lens (\DeleteServiceLinkedRole' {roleName} -> roleName) (\s@DeleteServiceLinkedRole' {} a -> s {roleName = a} :: DeleteServiceLinkedRole)

instance Core.AWSRequest DeleteServiceLinkedRole where
  type
    AWSResponse DeleteServiceLinkedRole =
      DeleteServiceLinkedRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteServiceLinkedRoleResult"
      ( \s h x ->
          DeleteServiceLinkedRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "DeletionTaskId")
      )

instance Prelude.Hashable DeleteServiceLinkedRole where
  hashWithSalt _salt DeleteServiceLinkedRole' {..} =
    _salt `Prelude.hashWithSalt` roleName

instance Prelude.NFData DeleteServiceLinkedRole where
  rnf DeleteServiceLinkedRole' {..} =
    Prelude.rnf roleName

instance Data.ToHeaders DeleteServiceLinkedRole where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteServiceLinkedRole where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteServiceLinkedRole where
  toQuery DeleteServiceLinkedRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteServiceLinkedRole" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName
      ]

-- | /See:/ 'newDeleteServiceLinkedRoleResponse' smart constructor.
data DeleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The deletion task identifier that you can use to check the status of the
    -- deletion. This identifier is returned in the format
    -- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
    deletionTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceLinkedRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServiceLinkedRoleResponse_httpStatus' - The response's http status code.
--
-- 'deletionTaskId', 'deleteServiceLinkedRoleResponse_deletionTaskId' - The deletion task identifier that you can use to check the status of the
-- deletion. This identifier is returned in the format
-- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
newDeleteServiceLinkedRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deletionTaskId'
  Prelude.Text ->
  DeleteServiceLinkedRoleResponse
newDeleteServiceLinkedRoleResponse
  pHttpStatus_
  pDeletionTaskId_ =
    DeleteServiceLinkedRoleResponse'
      { httpStatus =
          pHttpStatus_,
        deletionTaskId = pDeletionTaskId_
      }

-- | The response's http status code.
deleteServiceLinkedRoleResponse_httpStatus :: Lens.Lens' DeleteServiceLinkedRoleResponse Prelude.Int
deleteServiceLinkedRoleResponse_httpStatus = Lens.lens (\DeleteServiceLinkedRoleResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceLinkedRoleResponse' {} a -> s {httpStatus = a} :: DeleteServiceLinkedRoleResponse)

-- | The deletion task identifier that you can use to check the status of the
-- deletion. This identifier is returned in the format
-- @task\/aws-service-role\/\<service-principal-name>\/\<role-name>\/\<task-uuid>@.
deleteServiceLinkedRoleResponse_deletionTaskId :: Lens.Lens' DeleteServiceLinkedRoleResponse Prelude.Text
deleteServiceLinkedRoleResponse_deletionTaskId = Lens.lens (\DeleteServiceLinkedRoleResponse' {deletionTaskId} -> deletionTaskId) (\s@DeleteServiceLinkedRoleResponse' {} a -> s {deletionTaskId = a} :: DeleteServiceLinkedRoleResponse)

instance
  Prelude.NFData
    DeleteServiceLinkedRoleResponse
  where
  rnf DeleteServiceLinkedRoleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deletionTaskId

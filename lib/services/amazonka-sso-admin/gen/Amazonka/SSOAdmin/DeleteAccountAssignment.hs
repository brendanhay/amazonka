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
-- Module      : Amazonka.SSOAdmin.DeleteAccountAssignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a principal\'s access from a specified AWS account using a
-- specified permission set.
--
-- After a successful response, call
-- @DescribeAccountAssignmentCreationStatus@ to describe the status of an
-- assignment deletion request.
module Amazonka.SSOAdmin.DeleteAccountAssignment
  ( -- * Creating a Request
    DeleteAccountAssignment (..),
    newDeleteAccountAssignment,

    -- * Request Lenses
    deleteAccountAssignment_instanceArn,
    deleteAccountAssignment_targetId,
    deleteAccountAssignment_targetType,
    deleteAccountAssignment_permissionSetArn,
    deleteAccountAssignment_principalType,
    deleteAccountAssignment_principalId,

    -- * Destructuring the Response
    DeleteAccountAssignmentResponse (..),
    newDeleteAccountAssignmentResponse,

    -- * Response Lenses
    deleteAccountAssignmentResponse_accountAssignmentDeletionStatus,
    deleteAccountAssignmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newDeleteAccountAssignment' smart constructor.
data DeleteAccountAssignment = DeleteAccountAssignment'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | TargetID is an AWS account identifier, typically a 10-12 digit string
    -- (For example, 123456789012).
    targetId :: Prelude.Text,
    -- | The entity type for which the assignment will be deleted.
    targetType :: TargetType,
    -- | The ARN of the permission set that will be used to remove access.
    permissionSetArn :: Prelude.Text,
    -- | The entity type for which the assignment will be deleted.
    principalType :: PrincipalType,
    -- | An identifier for an object in IAM Identity Center, such as a user or
    -- group. PrincipalIds are GUIDs (For example,
    -- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
    -- PrincipalIds in IAM Identity Center, see the
    -- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
    principalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'deleteAccountAssignment_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'targetId', 'deleteAccountAssignment_targetId' - TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
--
-- 'targetType', 'deleteAccountAssignment_targetType' - The entity type for which the assignment will be deleted.
--
-- 'permissionSetArn', 'deleteAccountAssignment_permissionSetArn' - The ARN of the permission set that will be used to remove access.
--
-- 'principalType', 'deleteAccountAssignment_principalType' - The entity type for which the assignment will be deleted.
--
-- 'principalId', 'deleteAccountAssignment_principalId' - An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
newDeleteAccountAssignment ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'targetId'
  Prelude.Text ->
  -- | 'targetType'
  TargetType ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  -- | 'principalType'
  PrincipalType ->
  -- | 'principalId'
  Prelude.Text ->
  DeleteAccountAssignment
newDeleteAccountAssignment
  pInstanceArn_
  pTargetId_
  pTargetType_
  pPermissionSetArn_
  pPrincipalType_
  pPrincipalId_ =
    DeleteAccountAssignment'
      { instanceArn =
          pInstanceArn_,
        targetId = pTargetId_,
        targetType = pTargetType_,
        permissionSetArn = pPermissionSetArn_,
        principalType = pPrincipalType_,
        principalId = pPrincipalId_
      }

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
deleteAccountAssignment_instanceArn :: Lens.Lens' DeleteAccountAssignment Prelude.Text
deleteAccountAssignment_instanceArn = Lens.lens (\DeleteAccountAssignment' {instanceArn} -> instanceArn) (\s@DeleteAccountAssignment' {} a -> s {instanceArn = a} :: DeleteAccountAssignment)

-- | TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
deleteAccountAssignment_targetId :: Lens.Lens' DeleteAccountAssignment Prelude.Text
deleteAccountAssignment_targetId = Lens.lens (\DeleteAccountAssignment' {targetId} -> targetId) (\s@DeleteAccountAssignment' {} a -> s {targetId = a} :: DeleteAccountAssignment)

-- | The entity type for which the assignment will be deleted.
deleteAccountAssignment_targetType :: Lens.Lens' DeleteAccountAssignment TargetType
deleteAccountAssignment_targetType = Lens.lens (\DeleteAccountAssignment' {targetType} -> targetType) (\s@DeleteAccountAssignment' {} a -> s {targetType = a} :: DeleteAccountAssignment)

-- | The ARN of the permission set that will be used to remove access.
deleteAccountAssignment_permissionSetArn :: Lens.Lens' DeleteAccountAssignment Prelude.Text
deleteAccountAssignment_permissionSetArn = Lens.lens (\DeleteAccountAssignment' {permissionSetArn} -> permissionSetArn) (\s@DeleteAccountAssignment' {} a -> s {permissionSetArn = a} :: DeleteAccountAssignment)

-- | The entity type for which the assignment will be deleted.
deleteAccountAssignment_principalType :: Lens.Lens' DeleteAccountAssignment PrincipalType
deleteAccountAssignment_principalType = Lens.lens (\DeleteAccountAssignment' {principalType} -> principalType) (\s@DeleteAccountAssignment' {} a -> s {principalType = a} :: DeleteAccountAssignment)

-- | An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
deleteAccountAssignment_principalId :: Lens.Lens' DeleteAccountAssignment Prelude.Text
deleteAccountAssignment_principalId = Lens.lens (\DeleteAccountAssignment' {principalId} -> principalId) (\s@DeleteAccountAssignment' {} a -> s {principalId = a} :: DeleteAccountAssignment)

instance Core.AWSRequest DeleteAccountAssignment where
  type
    AWSResponse DeleteAccountAssignment =
      DeleteAccountAssignmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAccountAssignmentResponse'
            Prelude.<$> (x Data..?> "AccountAssignmentDeletionStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccountAssignment where
  hashWithSalt _salt DeleteAccountAssignment' {..} =
    _salt `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` permissionSetArn
      `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData DeleteAccountAssignment where
  rnf DeleteAccountAssignment' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf principalId

instance Data.ToHeaders DeleteAccountAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.DeleteAccountAssignment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccountAssignment where
  toJSON DeleteAccountAssignment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just ("TargetId" Data..= targetId),
            Prelude.Just ("TargetType" Data..= targetType),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn),
            Prelude.Just ("PrincipalType" Data..= principalType),
            Prelude.Just ("PrincipalId" Data..= principalId)
          ]
      )

instance Data.ToPath DeleteAccountAssignment where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccountAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountAssignmentResponse' smart constructor.
data DeleteAccountAssignmentResponse = DeleteAccountAssignmentResponse'
  { -- | The status object for the account assignment deletion operation.
    accountAssignmentDeletionStatus :: Prelude.Maybe AccountAssignmentOperationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAssignmentDeletionStatus', 'deleteAccountAssignmentResponse_accountAssignmentDeletionStatus' - The status object for the account assignment deletion operation.
--
-- 'httpStatus', 'deleteAccountAssignmentResponse_httpStatus' - The response's http status code.
newDeleteAccountAssignmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccountAssignmentResponse
newDeleteAccountAssignmentResponse pHttpStatus_ =
  DeleteAccountAssignmentResponse'
    { accountAssignmentDeletionStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status object for the account assignment deletion operation.
deleteAccountAssignmentResponse_accountAssignmentDeletionStatus :: Lens.Lens' DeleteAccountAssignmentResponse (Prelude.Maybe AccountAssignmentOperationStatus)
deleteAccountAssignmentResponse_accountAssignmentDeletionStatus = Lens.lens (\DeleteAccountAssignmentResponse' {accountAssignmentDeletionStatus} -> accountAssignmentDeletionStatus) (\s@DeleteAccountAssignmentResponse' {} a -> s {accountAssignmentDeletionStatus = a} :: DeleteAccountAssignmentResponse)

-- | The response's http status code.
deleteAccountAssignmentResponse_httpStatus :: Lens.Lens' DeleteAccountAssignmentResponse Prelude.Int
deleteAccountAssignmentResponse_httpStatus = Lens.lens (\DeleteAccountAssignmentResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountAssignmentResponse' {} a -> s {httpStatus = a} :: DeleteAccountAssignmentResponse)

instance
  Prelude.NFData
    DeleteAccountAssignmentResponse
  where
  rnf DeleteAccountAssignmentResponse' {..} =
    Prelude.rnf accountAssignmentDeletionStatus
      `Prelude.seq` Prelude.rnf httpStatus

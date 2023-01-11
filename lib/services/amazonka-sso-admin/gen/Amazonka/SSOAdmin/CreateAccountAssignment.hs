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
-- Module      : Amazonka.SSOAdmin.CreateAccountAssignment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns access to a principal for a specified AWS account using a
-- specified permission set.
--
-- The term /principal/ here refers to a user or group that is defined in
-- IAM Identity Center.
--
-- As part of a successful @CreateAccountAssignment@ call, the specified
-- permission set will automatically be provisioned to the account in the
-- form of an IAM policy. That policy is attached to the IAM role created
-- in IAM Identity Center. If the permission set is subsequently updated,
-- the corresponding IAM policies attached to roles in your accounts will
-- not be updated automatically. In this case, you must call
-- @ ProvisionPermissionSet @ to make these updates.
--
-- After a successful response, call
-- @DescribeAccountAssignmentCreationStatus@ to describe the status of an
-- assignment creation request.
module Amazonka.SSOAdmin.CreateAccountAssignment
  ( -- * Creating a Request
    CreateAccountAssignment (..),
    newCreateAccountAssignment,

    -- * Request Lenses
    createAccountAssignment_instanceArn,
    createAccountAssignment_targetId,
    createAccountAssignment_targetType,
    createAccountAssignment_permissionSetArn,
    createAccountAssignment_principalType,
    createAccountAssignment_principalId,

    -- * Destructuring the Response
    CreateAccountAssignmentResponse (..),
    newCreateAccountAssignmentResponse,

    -- * Response Lenses
    createAccountAssignmentResponse_accountAssignmentCreationStatus,
    createAccountAssignmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newCreateAccountAssignment' smart constructor.
data CreateAccountAssignment = CreateAccountAssignment'
  { -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | TargetID is an AWS account identifier, typically a 10-12 digit string
    -- (For example, 123456789012).
    targetId :: Prelude.Text,
    -- | The entity type for which the assignment will be created.
    targetType :: TargetType,
    -- | The ARN of the permission set that the admin wants to grant the
    -- principal access to.
    permissionSetArn :: Prelude.Text,
    -- | The entity type for which the assignment will be created.
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
-- Create a value of 'CreateAccountAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceArn', 'createAccountAssignment_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'targetId', 'createAccountAssignment_targetId' - TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
--
-- 'targetType', 'createAccountAssignment_targetType' - The entity type for which the assignment will be created.
--
-- 'permissionSetArn', 'createAccountAssignment_permissionSetArn' - The ARN of the permission set that the admin wants to grant the
-- principal access to.
--
-- 'principalType', 'createAccountAssignment_principalType' - The entity type for which the assignment will be created.
--
-- 'principalId', 'createAccountAssignment_principalId' - An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
newCreateAccountAssignment ::
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
  CreateAccountAssignment
newCreateAccountAssignment
  pInstanceArn_
  pTargetId_
  pTargetType_
  pPermissionSetArn_
  pPrincipalType_
  pPrincipalId_ =
    CreateAccountAssignment'
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
createAccountAssignment_instanceArn :: Lens.Lens' CreateAccountAssignment Prelude.Text
createAccountAssignment_instanceArn = Lens.lens (\CreateAccountAssignment' {instanceArn} -> instanceArn) (\s@CreateAccountAssignment' {} a -> s {instanceArn = a} :: CreateAccountAssignment)

-- | TargetID is an AWS account identifier, typically a 10-12 digit string
-- (For example, 123456789012).
createAccountAssignment_targetId :: Lens.Lens' CreateAccountAssignment Prelude.Text
createAccountAssignment_targetId = Lens.lens (\CreateAccountAssignment' {targetId} -> targetId) (\s@CreateAccountAssignment' {} a -> s {targetId = a} :: CreateAccountAssignment)

-- | The entity type for which the assignment will be created.
createAccountAssignment_targetType :: Lens.Lens' CreateAccountAssignment TargetType
createAccountAssignment_targetType = Lens.lens (\CreateAccountAssignment' {targetType} -> targetType) (\s@CreateAccountAssignment' {} a -> s {targetType = a} :: CreateAccountAssignment)

-- | The ARN of the permission set that the admin wants to grant the
-- principal access to.
createAccountAssignment_permissionSetArn :: Lens.Lens' CreateAccountAssignment Prelude.Text
createAccountAssignment_permissionSetArn = Lens.lens (\CreateAccountAssignment' {permissionSetArn} -> permissionSetArn) (\s@CreateAccountAssignment' {} a -> s {permissionSetArn = a} :: CreateAccountAssignment)

-- | The entity type for which the assignment will be created.
createAccountAssignment_principalType :: Lens.Lens' CreateAccountAssignment PrincipalType
createAccountAssignment_principalType = Lens.lens (\CreateAccountAssignment' {principalType} -> principalType) (\s@CreateAccountAssignment' {} a -> s {principalType = a} :: CreateAccountAssignment)

-- | An identifier for an object in IAM Identity Center, such as a user or
-- group. PrincipalIds are GUIDs (For example,
-- f81d4fae-7dec-11d0-a765-00a0c91e6bf6). For more information about
-- PrincipalIds in IAM Identity Center, see the
-- </singlesignon/latest/IdentityStoreAPIReference/welcome.html IAM Identity Center Identity Store API Reference>.
createAccountAssignment_principalId :: Lens.Lens' CreateAccountAssignment Prelude.Text
createAccountAssignment_principalId = Lens.lens (\CreateAccountAssignment' {principalId} -> principalId) (\s@CreateAccountAssignment' {} a -> s {principalId = a} :: CreateAccountAssignment)

instance Core.AWSRequest CreateAccountAssignment where
  type
    AWSResponse CreateAccountAssignment =
      CreateAccountAssignmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountAssignmentResponse'
            Prelude.<$> (x Data..?> "AccountAssignmentCreationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccountAssignment where
  hashWithSalt _salt CreateAccountAssignment' {..} =
    _salt `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` permissionSetArn
      `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData CreateAccountAssignment where
  rnf CreateAccountAssignment' {..} =
    Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf principalId

instance Data.ToHeaders CreateAccountAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.CreateAccountAssignment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccountAssignment where
  toJSON CreateAccountAssignment' {..} =
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

instance Data.ToPath CreateAccountAssignment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAccountAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccountAssignmentResponse' smart constructor.
data CreateAccountAssignmentResponse = CreateAccountAssignmentResponse'
  { -- | The status object for the account assignment creation operation.
    accountAssignmentCreationStatus :: Prelude.Maybe AccountAssignmentOperationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAssignmentCreationStatus', 'createAccountAssignmentResponse_accountAssignmentCreationStatus' - The status object for the account assignment creation operation.
--
-- 'httpStatus', 'createAccountAssignmentResponse_httpStatus' - The response's http status code.
newCreateAccountAssignmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAccountAssignmentResponse
newCreateAccountAssignmentResponse pHttpStatus_ =
  CreateAccountAssignmentResponse'
    { accountAssignmentCreationStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status object for the account assignment creation operation.
createAccountAssignmentResponse_accountAssignmentCreationStatus :: Lens.Lens' CreateAccountAssignmentResponse (Prelude.Maybe AccountAssignmentOperationStatus)
createAccountAssignmentResponse_accountAssignmentCreationStatus = Lens.lens (\CreateAccountAssignmentResponse' {accountAssignmentCreationStatus} -> accountAssignmentCreationStatus) (\s@CreateAccountAssignmentResponse' {} a -> s {accountAssignmentCreationStatus = a} :: CreateAccountAssignmentResponse)

-- | The response's http status code.
createAccountAssignmentResponse_httpStatus :: Lens.Lens' CreateAccountAssignmentResponse Prelude.Int
createAccountAssignmentResponse_httpStatus = Lens.lens (\CreateAccountAssignmentResponse' {httpStatus} -> httpStatus) (\s@CreateAccountAssignmentResponse' {} a -> s {httpStatus = a} :: CreateAccountAssignmentResponse)

instance
  Prelude.NFData
    CreateAccountAssignmentResponse
  where
  rnf CreateAccountAssignmentResponse' {..} =
    Prelude.rnf accountAssignmentCreationStatus
      `Prelude.seq` Prelude.rnf httpStatus

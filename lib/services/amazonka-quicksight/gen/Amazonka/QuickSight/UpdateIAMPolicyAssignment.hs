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
-- Module      : Amazonka.QuickSight.UpdateIAMPolicyAssignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing IAM policy assignment. This operation updates only
-- the optional parameter or parameters that are specified in the request.
-- This overwrites all of the users included in @Identities@.
module Amazonka.QuickSight.UpdateIAMPolicyAssignment
  ( -- * Creating a Request
    UpdateIAMPolicyAssignment (..),
    newUpdateIAMPolicyAssignment,

    -- * Request Lenses
    updateIAMPolicyAssignment_identities,
    updateIAMPolicyAssignment_policyArn,
    updateIAMPolicyAssignment_assignmentStatus,
    updateIAMPolicyAssignment_awsAccountId,
    updateIAMPolicyAssignment_assignmentName,
    updateIAMPolicyAssignment_namespace,

    -- * Destructuring the Response
    UpdateIAMPolicyAssignmentResponse (..),
    newUpdateIAMPolicyAssignmentResponse,

    -- * Response Lenses
    updateIAMPolicyAssignmentResponse_requestId,
    updateIAMPolicyAssignmentResponse_identities,
    updateIAMPolicyAssignmentResponse_policyArn,
    updateIAMPolicyAssignmentResponse_assignmentName,
    updateIAMPolicyAssignmentResponse_assignmentId,
    updateIAMPolicyAssignmentResponse_assignmentStatus,
    updateIAMPolicyAssignmentResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIAMPolicyAssignment' smart constructor.
data UpdateIAMPolicyAssignment = UpdateIAMPolicyAssignment'
  { -- | The Amazon QuickSight users, groups, or both that you want to assign the
    -- policy to.
    identities :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The ARN for the IAM policy to apply to the Amazon QuickSight users and
    -- groups specified in this assignment.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the assignment. Possible values are as follows:
    --
    -- -   @ENABLED@ - Anything specified in this assignment is used when
    --     creating the data source.
    --
    -- -   @DISABLED@ - This assignment isn\'t used when creating the data
    --     source.
    --
    -- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
    --     when creating the data source.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The ID of the Amazon Web Services account that contains the IAM policy
    -- assignment.
    awsAccountId :: Prelude.Text,
    -- | The name of the assignment, also called a rule. This name must be unique
    -- within an Amazon Web Services account.
    assignmentName :: Prelude.Text,
    -- | The namespace of the assignment.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIAMPolicyAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identities', 'updateIAMPolicyAssignment_identities' - The Amazon QuickSight users, groups, or both that you want to assign the
-- policy to.
--
-- 'policyArn', 'updateIAMPolicyAssignment_policyArn' - The ARN for the IAM policy to apply to the Amazon QuickSight users and
-- groups specified in this assignment.
--
-- 'assignmentStatus', 'updateIAMPolicyAssignment_assignmentStatus' - The status of the assignment. Possible values are as follows:
--
-- -   @ENABLED@ - Anything specified in this assignment is used when
--     creating the data source.
--
-- -   @DISABLED@ - This assignment isn\'t used when creating the data
--     source.
--
-- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
--     when creating the data source.
--
-- 'awsAccountId', 'updateIAMPolicyAssignment_awsAccountId' - The ID of the Amazon Web Services account that contains the IAM policy
-- assignment.
--
-- 'assignmentName', 'updateIAMPolicyAssignment_assignmentName' - The name of the assignment, also called a rule. This name must be unique
-- within an Amazon Web Services account.
--
-- 'namespace', 'updateIAMPolicyAssignment_namespace' - The namespace of the assignment.
newUpdateIAMPolicyAssignment ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assignmentName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  UpdateIAMPolicyAssignment
newUpdateIAMPolicyAssignment
  pAwsAccountId_
  pAssignmentName_
  pNamespace_ =
    UpdateIAMPolicyAssignment'
      { identities =
          Prelude.Nothing,
        policyArn = Prelude.Nothing,
        assignmentStatus = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        assignmentName = pAssignmentName_,
        namespace = pNamespace_
      }

-- | The Amazon QuickSight users, groups, or both that you want to assign the
-- policy to.
updateIAMPolicyAssignment_identities :: Lens.Lens' UpdateIAMPolicyAssignment (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
updateIAMPolicyAssignment_identities = Lens.lens (\UpdateIAMPolicyAssignment' {identities} -> identities) (\s@UpdateIAMPolicyAssignment' {} a -> s {identities = a} :: UpdateIAMPolicyAssignment) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for the IAM policy to apply to the Amazon QuickSight users and
-- groups specified in this assignment.
updateIAMPolicyAssignment_policyArn :: Lens.Lens' UpdateIAMPolicyAssignment (Prelude.Maybe Prelude.Text)
updateIAMPolicyAssignment_policyArn = Lens.lens (\UpdateIAMPolicyAssignment' {policyArn} -> policyArn) (\s@UpdateIAMPolicyAssignment' {} a -> s {policyArn = a} :: UpdateIAMPolicyAssignment)

-- | The status of the assignment. Possible values are as follows:
--
-- -   @ENABLED@ - Anything specified in this assignment is used when
--     creating the data source.
--
-- -   @DISABLED@ - This assignment isn\'t used when creating the data
--     source.
--
-- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
--     when creating the data source.
updateIAMPolicyAssignment_assignmentStatus :: Lens.Lens' UpdateIAMPolicyAssignment (Prelude.Maybe AssignmentStatus)
updateIAMPolicyAssignment_assignmentStatus = Lens.lens (\UpdateIAMPolicyAssignment' {assignmentStatus} -> assignmentStatus) (\s@UpdateIAMPolicyAssignment' {} a -> s {assignmentStatus = a} :: UpdateIAMPolicyAssignment)

-- | The ID of the Amazon Web Services account that contains the IAM policy
-- assignment.
updateIAMPolicyAssignment_awsAccountId :: Lens.Lens' UpdateIAMPolicyAssignment Prelude.Text
updateIAMPolicyAssignment_awsAccountId = Lens.lens (\UpdateIAMPolicyAssignment' {awsAccountId} -> awsAccountId) (\s@UpdateIAMPolicyAssignment' {} a -> s {awsAccountId = a} :: UpdateIAMPolicyAssignment)

-- | The name of the assignment, also called a rule. This name must be unique
-- within an Amazon Web Services account.
updateIAMPolicyAssignment_assignmentName :: Lens.Lens' UpdateIAMPolicyAssignment Prelude.Text
updateIAMPolicyAssignment_assignmentName = Lens.lens (\UpdateIAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@UpdateIAMPolicyAssignment' {} a -> s {assignmentName = a} :: UpdateIAMPolicyAssignment)

-- | The namespace of the assignment.
updateIAMPolicyAssignment_namespace :: Lens.Lens' UpdateIAMPolicyAssignment Prelude.Text
updateIAMPolicyAssignment_namespace = Lens.lens (\UpdateIAMPolicyAssignment' {namespace} -> namespace) (\s@UpdateIAMPolicyAssignment' {} a -> s {namespace = a} :: UpdateIAMPolicyAssignment)

instance Core.AWSRequest UpdateIAMPolicyAssignment where
  type
    AWSResponse UpdateIAMPolicyAssignment =
      UpdateIAMPolicyAssignmentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIAMPolicyAssignmentResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Identities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "PolicyArn")
            Prelude.<*> (x Data..?> "AssignmentName")
            Prelude.<*> (x Data..?> "AssignmentId")
            Prelude.<*> (x Data..?> "AssignmentStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIAMPolicyAssignment where
  hashWithSalt _salt UpdateIAMPolicyAssignment' {..} =
    _salt `Prelude.hashWithSalt` identities
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` assignmentStatus
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assignmentName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData UpdateIAMPolicyAssignment where
  rnf UpdateIAMPolicyAssignment' {..} =
    Prelude.rnf identities
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf assignmentStatus
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assignmentName
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders UpdateIAMPolicyAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIAMPolicyAssignment where
  toJSON UpdateIAMPolicyAssignment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Identities" Data..=) Prelude.<$> identities,
            ("PolicyArn" Data..=) Prelude.<$> policyArn,
            ("AssignmentStatus" Data..=)
              Prelude.<$> assignmentStatus
          ]
      )

instance Data.ToPath UpdateIAMPolicyAssignment where
  toPath UpdateIAMPolicyAssignment' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/iam-policy-assignments/",
        Data.toBS assignmentName
      ]

instance Data.ToQuery UpdateIAMPolicyAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIAMPolicyAssignmentResponse' smart constructor.
data UpdateIAMPolicyAssignmentResponse = UpdateIAMPolicyAssignmentResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight users, groups, or both that the IAM policy is
    -- assigned to.
    identities :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The ARN for the IAM policy applied to the Amazon QuickSight users and
    -- groups specified in this assignment.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the assignment or rule.
    assignmentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the assignment.
    assignmentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the assignment. Possible values are as follows:
    --
    -- -   @ENABLED@ - Anything specified in this assignment is used when
    --     creating the data source.
    --
    -- -   @DISABLED@ - This assignment isn\'t used when creating the data
    --     source.
    --
    -- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
    --     when creating the data source.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIAMPolicyAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateIAMPolicyAssignmentResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'identities', 'updateIAMPolicyAssignmentResponse_identities' - The Amazon QuickSight users, groups, or both that the IAM policy is
-- assigned to.
--
-- 'policyArn', 'updateIAMPolicyAssignmentResponse_policyArn' - The ARN for the IAM policy applied to the Amazon QuickSight users and
-- groups specified in this assignment.
--
-- 'assignmentName', 'updateIAMPolicyAssignmentResponse_assignmentName' - The name of the assignment or rule.
--
-- 'assignmentId', 'updateIAMPolicyAssignmentResponse_assignmentId' - The ID of the assignment.
--
-- 'assignmentStatus', 'updateIAMPolicyAssignmentResponse_assignmentStatus' - The status of the assignment. Possible values are as follows:
--
-- -   @ENABLED@ - Anything specified in this assignment is used when
--     creating the data source.
--
-- -   @DISABLED@ - This assignment isn\'t used when creating the data
--     source.
--
-- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
--     when creating the data source.
--
-- 'status', 'updateIAMPolicyAssignmentResponse_status' - The HTTP status of the request.
newUpdateIAMPolicyAssignmentResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateIAMPolicyAssignmentResponse
newUpdateIAMPolicyAssignmentResponse pStatus_ =
  UpdateIAMPolicyAssignmentResponse'
    { requestId =
        Prelude.Nothing,
      identities = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      assignmentName = Prelude.Nothing,
      assignmentId = Prelude.Nothing,
      assignmentStatus = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateIAMPolicyAssignmentResponse_requestId :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe Prelude.Text)
updateIAMPolicyAssignmentResponse_requestId = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {requestId} -> requestId) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {requestId = a} :: UpdateIAMPolicyAssignmentResponse)

-- | The Amazon QuickSight users, groups, or both that the IAM policy is
-- assigned to.
updateIAMPolicyAssignmentResponse_identities :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
updateIAMPolicyAssignmentResponse_identities = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {identities} -> identities) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {identities = a} :: UpdateIAMPolicyAssignmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for the IAM policy applied to the Amazon QuickSight users and
-- groups specified in this assignment.
updateIAMPolicyAssignmentResponse_policyArn :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe Prelude.Text)
updateIAMPolicyAssignmentResponse_policyArn = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {policyArn} -> policyArn) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {policyArn = a} :: UpdateIAMPolicyAssignmentResponse)

-- | The name of the assignment or rule.
updateIAMPolicyAssignmentResponse_assignmentName :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe Prelude.Text)
updateIAMPolicyAssignmentResponse_assignmentName = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {assignmentName} -> assignmentName) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {assignmentName = a} :: UpdateIAMPolicyAssignmentResponse)

-- | The ID of the assignment.
updateIAMPolicyAssignmentResponse_assignmentId :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe Prelude.Text)
updateIAMPolicyAssignmentResponse_assignmentId = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {assignmentId} -> assignmentId) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {assignmentId = a} :: UpdateIAMPolicyAssignmentResponse)

-- | The status of the assignment. Possible values are as follows:
--
-- -   @ENABLED@ - Anything specified in this assignment is used when
--     creating the data source.
--
-- -   @DISABLED@ - This assignment isn\'t used when creating the data
--     source.
--
-- -   @DRAFT@ - This assignment is an unfinished draft and isn\'t used
--     when creating the data source.
updateIAMPolicyAssignmentResponse_assignmentStatus :: Lens.Lens' UpdateIAMPolicyAssignmentResponse (Prelude.Maybe AssignmentStatus)
updateIAMPolicyAssignmentResponse_assignmentStatus = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {assignmentStatus} -> assignmentStatus) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {assignmentStatus = a} :: UpdateIAMPolicyAssignmentResponse)

-- | The HTTP status of the request.
updateIAMPolicyAssignmentResponse_status :: Lens.Lens' UpdateIAMPolicyAssignmentResponse Prelude.Int
updateIAMPolicyAssignmentResponse_status = Lens.lens (\UpdateIAMPolicyAssignmentResponse' {status} -> status) (\s@UpdateIAMPolicyAssignmentResponse' {} a -> s {status = a} :: UpdateIAMPolicyAssignmentResponse)

instance
  Prelude.NFData
    UpdateIAMPolicyAssignmentResponse
  where
  rnf UpdateIAMPolicyAssignmentResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf identities
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf assignmentName
      `Prelude.seq` Prelude.rnf assignmentId
      `Prelude.seq` Prelude.rnf assignmentStatus
      `Prelude.seq` Prelude.rnf status

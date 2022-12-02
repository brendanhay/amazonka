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
-- Module      : Amazonka.QuickSight.DescribeIAMPolicyAssignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing IAM policy assignment, as specified by the
-- assignment name.
module Amazonka.QuickSight.DescribeIAMPolicyAssignment
  ( -- * Creating a Request
    DescribeIAMPolicyAssignment (..),
    newDescribeIAMPolicyAssignment,

    -- * Request Lenses
    describeIAMPolicyAssignment_awsAccountId,
    describeIAMPolicyAssignment_assignmentName,
    describeIAMPolicyAssignment_namespace,

    -- * Destructuring the Response
    DescribeIAMPolicyAssignmentResponse (..),
    newDescribeIAMPolicyAssignmentResponse,

    -- * Response Lenses
    describeIAMPolicyAssignmentResponse_requestId,
    describeIAMPolicyAssignmentResponse_iAMPolicyAssignment,
    describeIAMPolicyAssignmentResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIAMPolicyAssignment' smart constructor.
data DescribeIAMPolicyAssignment = DescribeIAMPolicyAssignment'
  { -- | The ID of the Amazon Web Services account that contains the assignment
    -- that you want to describe.
    awsAccountId :: Prelude.Text,
    -- | The name of the assignment, also called a rule.
    assignmentName :: Prelude.Text,
    -- | The namespace that contains the assignment.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIAMPolicyAssignment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeIAMPolicyAssignment_awsAccountId' - The ID of the Amazon Web Services account that contains the assignment
-- that you want to describe.
--
-- 'assignmentName', 'describeIAMPolicyAssignment_assignmentName' - The name of the assignment, also called a rule.
--
-- 'namespace', 'describeIAMPolicyAssignment_namespace' - The namespace that contains the assignment.
newDescribeIAMPolicyAssignment ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'assignmentName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeIAMPolicyAssignment
newDescribeIAMPolicyAssignment
  pAwsAccountId_
  pAssignmentName_
  pNamespace_ =
    DescribeIAMPolicyAssignment'
      { awsAccountId =
          pAwsAccountId_,
        assignmentName = pAssignmentName_,
        namespace = pNamespace_
      }

-- | The ID of the Amazon Web Services account that contains the assignment
-- that you want to describe.
describeIAMPolicyAssignment_awsAccountId :: Lens.Lens' DescribeIAMPolicyAssignment Prelude.Text
describeIAMPolicyAssignment_awsAccountId = Lens.lens (\DescribeIAMPolicyAssignment' {awsAccountId} -> awsAccountId) (\s@DescribeIAMPolicyAssignment' {} a -> s {awsAccountId = a} :: DescribeIAMPolicyAssignment)

-- | The name of the assignment, also called a rule.
describeIAMPolicyAssignment_assignmentName :: Lens.Lens' DescribeIAMPolicyAssignment Prelude.Text
describeIAMPolicyAssignment_assignmentName = Lens.lens (\DescribeIAMPolicyAssignment' {assignmentName} -> assignmentName) (\s@DescribeIAMPolicyAssignment' {} a -> s {assignmentName = a} :: DescribeIAMPolicyAssignment)

-- | The namespace that contains the assignment.
describeIAMPolicyAssignment_namespace :: Lens.Lens' DescribeIAMPolicyAssignment Prelude.Text
describeIAMPolicyAssignment_namespace = Lens.lens (\DescribeIAMPolicyAssignment' {namespace} -> namespace) (\s@DescribeIAMPolicyAssignment' {} a -> s {namespace = a} :: DescribeIAMPolicyAssignment)

instance Core.AWSRequest DescribeIAMPolicyAssignment where
  type
    AWSResponse DescribeIAMPolicyAssignment =
      DescribeIAMPolicyAssignmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIAMPolicyAssignmentResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "IAMPolicyAssignment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIAMPolicyAssignment where
  hashWithSalt _salt DescribeIAMPolicyAssignment' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` assignmentName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeIAMPolicyAssignment where
  rnf DescribeIAMPolicyAssignment' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf assignmentName
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DescribeIAMPolicyAssignment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeIAMPolicyAssignment where
  toPath DescribeIAMPolicyAssignment' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/iam-policy-assignments/",
        Data.toBS assignmentName
      ]

instance Data.ToQuery DescribeIAMPolicyAssignment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIAMPolicyAssignmentResponse' smart constructor.
data DescribeIAMPolicyAssignmentResponse = DescribeIAMPolicyAssignmentResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information describing the IAM policy assignment.
    iAMPolicyAssignment :: Prelude.Maybe IAMPolicyAssignment,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIAMPolicyAssignmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeIAMPolicyAssignmentResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'iAMPolicyAssignment', 'describeIAMPolicyAssignmentResponse_iAMPolicyAssignment' - Information describing the IAM policy assignment.
--
-- 'status', 'describeIAMPolicyAssignmentResponse_status' - The HTTP status of the request.
newDescribeIAMPolicyAssignmentResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeIAMPolicyAssignmentResponse
newDescribeIAMPolicyAssignmentResponse pStatus_ =
  DescribeIAMPolicyAssignmentResponse'
    { requestId =
        Prelude.Nothing,
      iAMPolicyAssignment = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeIAMPolicyAssignmentResponse_requestId :: Lens.Lens' DescribeIAMPolicyAssignmentResponse (Prelude.Maybe Prelude.Text)
describeIAMPolicyAssignmentResponse_requestId = Lens.lens (\DescribeIAMPolicyAssignmentResponse' {requestId} -> requestId) (\s@DescribeIAMPolicyAssignmentResponse' {} a -> s {requestId = a} :: DescribeIAMPolicyAssignmentResponse)

-- | Information describing the IAM policy assignment.
describeIAMPolicyAssignmentResponse_iAMPolicyAssignment :: Lens.Lens' DescribeIAMPolicyAssignmentResponse (Prelude.Maybe IAMPolicyAssignment)
describeIAMPolicyAssignmentResponse_iAMPolicyAssignment = Lens.lens (\DescribeIAMPolicyAssignmentResponse' {iAMPolicyAssignment} -> iAMPolicyAssignment) (\s@DescribeIAMPolicyAssignmentResponse' {} a -> s {iAMPolicyAssignment = a} :: DescribeIAMPolicyAssignmentResponse)

-- | The HTTP status of the request.
describeIAMPolicyAssignmentResponse_status :: Lens.Lens' DescribeIAMPolicyAssignmentResponse Prelude.Int
describeIAMPolicyAssignmentResponse_status = Lens.lens (\DescribeIAMPolicyAssignmentResponse' {status} -> status) (\s@DescribeIAMPolicyAssignmentResponse' {} a -> s {status = a} :: DescribeIAMPolicyAssignmentResponse)

instance
  Prelude.NFData
    DescribeIAMPolicyAssignmentResponse
  where
  rnf DescribeIAMPolicyAssignmentResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf iAMPolicyAssignment
      `Prelude.seq` Prelude.rnf status

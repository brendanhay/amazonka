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
-- Module      : Amazonka.SSOAdmin.ListAccountAssignments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assignee of the specified AWS account with the specified
-- permission set.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListAccountAssignments
  ( -- * Creating a Request
    ListAccountAssignments (..),
    newListAccountAssignments,

    -- * Request Lenses
    listAccountAssignments_nextToken,
    listAccountAssignments_maxResults,
    listAccountAssignments_instanceArn,
    listAccountAssignments_accountId,
    listAccountAssignments_permissionSetArn,

    -- * Destructuring the Response
    ListAccountAssignmentsResponse (..),
    newListAccountAssignmentsResponse,

    -- * Response Lenses
    listAccountAssignmentsResponse_nextToken,
    listAccountAssignmentsResponse_accountAssignments,
    listAccountAssignmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListAccountAssignments' smart constructor.
data ListAccountAssignments = ListAccountAssignments'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to display for the assignment.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The identifier of the AWS account from which to list the assignments.
    accountId :: Prelude.Text,
    -- | The ARN of the permission set from which to list assignments.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssignments_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'maxResults', 'listAccountAssignments_maxResults' - The maximum number of results to display for the assignment.
--
-- 'instanceArn', 'listAccountAssignments_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'accountId', 'listAccountAssignments_accountId' - The identifier of the AWS account from which to list the assignments.
--
-- 'permissionSetArn', 'listAccountAssignments_permissionSetArn' - The ARN of the permission set from which to list assignments.
newListAccountAssignments ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  ListAccountAssignments
newListAccountAssignments
  pInstanceArn_
  pAccountId_
  pPermissionSetArn_ =
    ListAccountAssignments'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        instanceArn = pInstanceArn_,
        accountId = pAccountId_,
        permissionSetArn = pPermissionSetArn_
      }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignments_nextToken :: Lens.Lens' ListAccountAssignments (Prelude.Maybe Prelude.Text)
listAccountAssignments_nextToken = Lens.lens (\ListAccountAssignments' {nextToken} -> nextToken) (\s@ListAccountAssignments' {} a -> s {nextToken = a} :: ListAccountAssignments)

-- | The maximum number of results to display for the assignment.
listAccountAssignments_maxResults :: Lens.Lens' ListAccountAssignments (Prelude.Maybe Prelude.Natural)
listAccountAssignments_maxResults = Lens.lens (\ListAccountAssignments' {maxResults} -> maxResults) (\s@ListAccountAssignments' {} a -> s {maxResults = a} :: ListAccountAssignments)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listAccountAssignments_instanceArn :: Lens.Lens' ListAccountAssignments Prelude.Text
listAccountAssignments_instanceArn = Lens.lens (\ListAccountAssignments' {instanceArn} -> instanceArn) (\s@ListAccountAssignments' {} a -> s {instanceArn = a} :: ListAccountAssignments)

-- | The identifier of the AWS account from which to list the assignments.
listAccountAssignments_accountId :: Lens.Lens' ListAccountAssignments Prelude.Text
listAccountAssignments_accountId = Lens.lens (\ListAccountAssignments' {accountId} -> accountId) (\s@ListAccountAssignments' {} a -> s {accountId = a} :: ListAccountAssignments)

-- | The ARN of the permission set from which to list assignments.
listAccountAssignments_permissionSetArn :: Lens.Lens' ListAccountAssignments Prelude.Text
listAccountAssignments_permissionSetArn = Lens.lens (\ListAccountAssignments' {permissionSetArn} -> permissionSetArn) (\s@ListAccountAssignments' {} a -> s {permissionSetArn = a} :: ListAccountAssignments)

instance Core.AWSPager ListAccountAssignments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentsResponse_accountAssignments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountAssignments_nextToken
          Lens..~ rs
          Lens.^? listAccountAssignmentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAccountAssignments where
  type
    AWSResponse ListAccountAssignments =
      ListAccountAssignmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountAssignmentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "AccountAssignments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccountAssignments where
  hashWithSalt _salt ListAccountAssignments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` permissionSetArn

instance Prelude.NFData ListAccountAssignments where
  rnf ListAccountAssignments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf permissionSetArn

instance Data.ToHeaders ListAccountAssignments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListAccountAssignments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccountAssignments where
  toJSON ListAccountAssignments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn)
          ]
      )

instance Data.ToPath ListAccountAssignments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccountAssignments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountAssignmentsResponse' smart constructor.
data ListAccountAssignmentsResponse = ListAccountAssignmentsResponse'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of assignments that match the input AWS account and permission
    -- set.
    accountAssignments :: Prelude.Maybe [AccountAssignment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssignmentsResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'accountAssignments', 'listAccountAssignmentsResponse_accountAssignments' - The list of assignments that match the input AWS account and permission
-- set.
--
-- 'httpStatus', 'listAccountAssignmentsResponse_httpStatus' - The response's http status code.
newListAccountAssignmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountAssignmentsResponse
newListAccountAssignmentsResponse pHttpStatus_ =
  ListAccountAssignmentsResponse'
    { nextToken =
        Prelude.Nothing,
      accountAssignments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignmentsResponse_nextToken :: Lens.Lens' ListAccountAssignmentsResponse (Prelude.Maybe Prelude.Text)
listAccountAssignmentsResponse_nextToken = Lens.lens (\ListAccountAssignmentsResponse' {nextToken} -> nextToken) (\s@ListAccountAssignmentsResponse' {} a -> s {nextToken = a} :: ListAccountAssignmentsResponse)

-- | The list of assignments that match the input AWS account and permission
-- set.
listAccountAssignmentsResponse_accountAssignments :: Lens.Lens' ListAccountAssignmentsResponse (Prelude.Maybe [AccountAssignment])
listAccountAssignmentsResponse_accountAssignments = Lens.lens (\ListAccountAssignmentsResponse' {accountAssignments} -> accountAssignments) (\s@ListAccountAssignmentsResponse' {} a -> s {accountAssignments = a} :: ListAccountAssignmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountAssignmentsResponse_httpStatus :: Lens.Lens' ListAccountAssignmentsResponse Prelude.Int
listAccountAssignmentsResponse_httpStatus = Lens.lens (\ListAccountAssignmentsResponse' {httpStatus} -> httpStatus) (\s@ListAccountAssignmentsResponse' {} a -> s {httpStatus = a} :: ListAccountAssignmentsResponse)

instance
  Prelude.NFData
    ListAccountAssignmentsResponse
  where
  rnf ListAccountAssignmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountAssignments
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.SSOAdmin.ListAccountAssignmentCreationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of the AWS account assignment creation requests for a
-- specified IAM Identity Center instance.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListAccountAssignmentCreationStatus
  ( -- * Creating a Request
    ListAccountAssignmentCreationStatus (..),
    newListAccountAssignmentCreationStatus,

    -- * Request Lenses
    listAccountAssignmentCreationStatus_nextToken,
    listAccountAssignmentCreationStatus_filter,
    listAccountAssignmentCreationStatus_maxResults,
    listAccountAssignmentCreationStatus_instanceArn,

    -- * Destructuring the Response
    ListAccountAssignmentCreationStatusResponse (..),
    newListAccountAssignmentCreationStatusResponse,

    -- * Response Lenses
    listAccountAssignmentCreationStatusResponse_nextToken,
    listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus,
    listAccountAssignmentCreationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListAccountAssignmentCreationStatus' smart constructor.
data ListAccountAssignmentCreationStatus = ListAccountAssignmentCreationStatus'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters results based on the passed attribute value.
    filter' :: Prelude.Maybe OperationStatusFilter,
    -- | The maximum number of results to display for the assignment.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignmentCreationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssignmentCreationStatus_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'filter'', 'listAccountAssignmentCreationStatus_filter' - Filters results based on the passed attribute value.
--
-- 'maxResults', 'listAccountAssignmentCreationStatus_maxResults' - The maximum number of results to display for the assignment.
--
-- 'instanceArn', 'listAccountAssignmentCreationStatus_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newListAccountAssignmentCreationStatus ::
  -- | 'instanceArn'
  Prelude.Text ->
  ListAccountAssignmentCreationStatus
newListAccountAssignmentCreationStatus pInstanceArn_ =
  ListAccountAssignmentCreationStatus'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceArn = pInstanceArn_
    }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignmentCreationStatus_nextToken :: Lens.Lens' ListAccountAssignmentCreationStatus (Prelude.Maybe Prelude.Text)
listAccountAssignmentCreationStatus_nextToken = Lens.lens (\ListAccountAssignmentCreationStatus' {nextToken} -> nextToken) (\s@ListAccountAssignmentCreationStatus' {} a -> s {nextToken = a} :: ListAccountAssignmentCreationStatus)

-- | Filters results based on the passed attribute value.
listAccountAssignmentCreationStatus_filter :: Lens.Lens' ListAccountAssignmentCreationStatus (Prelude.Maybe OperationStatusFilter)
listAccountAssignmentCreationStatus_filter = Lens.lens (\ListAccountAssignmentCreationStatus' {filter'} -> filter') (\s@ListAccountAssignmentCreationStatus' {} a -> s {filter' = a} :: ListAccountAssignmentCreationStatus)

-- | The maximum number of results to display for the assignment.
listAccountAssignmentCreationStatus_maxResults :: Lens.Lens' ListAccountAssignmentCreationStatus (Prelude.Maybe Prelude.Natural)
listAccountAssignmentCreationStatus_maxResults = Lens.lens (\ListAccountAssignmentCreationStatus' {maxResults} -> maxResults) (\s@ListAccountAssignmentCreationStatus' {} a -> s {maxResults = a} :: ListAccountAssignmentCreationStatus)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listAccountAssignmentCreationStatus_instanceArn :: Lens.Lens' ListAccountAssignmentCreationStatus Prelude.Text
listAccountAssignmentCreationStatus_instanceArn = Lens.lens (\ListAccountAssignmentCreationStatus' {instanceArn} -> instanceArn) (\s@ListAccountAssignmentCreationStatus' {} a -> s {instanceArn = a} :: ListAccountAssignmentCreationStatus)

instance
  Core.AWSPager
    ListAccountAssignmentCreationStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentCreationStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountAssignmentCreationStatus_nextToken
          Lens..~ rs
          Lens.^? listAccountAssignmentCreationStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAccountAssignmentCreationStatus
  where
  type
    AWSResponse ListAccountAssignmentCreationStatus =
      ListAccountAssignmentCreationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountAssignmentCreationStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "AccountAssignmentsCreationStatus"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAccountAssignmentCreationStatus
  where
  hashWithSalt
    _salt
    ListAccountAssignmentCreationStatus' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` instanceArn

instance
  Prelude.NFData
    ListAccountAssignmentCreationStatus
  where
  rnf ListAccountAssignmentCreationStatus' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArn

instance
  Data.ToHeaders
    ListAccountAssignmentCreationStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListAccountAssignmentCreationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListAccountAssignmentCreationStatus
  where
  toJSON ListAccountAssignmentCreationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Data..= instanceArn)
          ]
      )

instance
  Data.ToPath
    ListAccountAssignmentCreationStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAccountAssignmentCreationStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountAssignmentCreationStatusResponse' smart constructor.
data ListAccountAssignmentCreationStatusResponse = ListAccountAssignmentCreationStatusResponse'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status object for the account assignment creation operation.
    accountAssignmentsCreationStatus :: Prelude.Maybe [AccountAssignmentOperationStatusMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignmentCreationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountAssignmentCreationStatusResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'accountAssignmentsCreationStatus', 'listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus' - The status object for the account assignment creation operation.
--
-- 'httpStatus', 'listAccountAssignmentCreationStatusResponse_httpStatus' - The response's http status code.
newListAccountAssignmentCreationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountAssignmentCreationStatusResponse
newListAccountAssignmentCreationStatusResponse
  pHttpStatus_ =
    ListAccountAssignmentCreationStatusResponse'
      { nextToken =
          Prelude.Nothing,
        accountAssignmentsCreationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignmentCreationStatusResponse_nextToken :: Lens.Lens' ListAccountAssignmentCreationStatusResponse (Prelude.Maybe Prelude.Text)
listAccountAssignmentCreationStatusResponse_nextToken = Lens.lens (\ListAccountAssignmentCreationStatusResponse' {nextToken} -> nextToken) (\s@ListAccountAssignmentCreationStatusResponse' {} a -> s {nextToken = a} :: ListAccountAssignmentCreationStatusResponse)

-- | The status object for the account assignment creation operation.
listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus :: Lens.Lens' ListAccountAssignmentCreationStatusResponse (Prelude.Maybe [AccountAssignmentOperationStatusMetadata])
listAccountAssignmentCreationStatusResponse_accountAssignmentsCreationStatus = Lens.lens (\ListAccountAssignmentCreationStatusResponse' {accountAssignmentsCreationStatus} -> accountAssignmentsCreationStatus) (\s@ListAccountAssignmentCreationStatusResponse' {} a -> s {accountAssignmentsCreationStatus = a} :: ListAccountAssignmentCreationStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountAssignmentCreationStatusResponse_httpStatus :: Lens.Lens' ListAccountAssignmentCreationStatusResponse Prelude.Int
listAccountAssignmentCreationStatusResponse_httpStatus = Lens.lens (\ListAccountAssignmentCreationStatusResponse' {httpStatus} -> httpStatus) (\s@ListAccountAssignmentCreationStatusResponse' {} a -> s {httpStatus = a} :: ListAccountAssignmentCreationStatusResponse)

instance
  Prelude.NFData
    ListAccountAssignmentCreationStatusResponse
  where
  rnf ListAccountAssignmentCreationStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountAssignmentsCreationStatus
      `Prelude.seq` Prelude.rnf httpStatus

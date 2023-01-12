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
-- Module      : Amazonka.SSOAdmin.ListAccountAssignmentDeletionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of the AWS account assignment deletion requests for a
-- specified IAM Identity Center instance.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListAccountAssignmentDeletionStatus
  ( -- * Creating a Request
    ListAccountAssignmentDeletionStatus (..),
    newListAccountAssignmentDeletionStatus,

    -- * Request Lenses
    listAccountAssignmentDeletionStatus_filter,
    listAccountAssignmentDeletionStatus_maxResults,
    listAccountAssignmentDeletionStatus_nextToken,
    listAccountAssignmentDeletionStatus_instanceArn,

    -- * Destructuring the Response
    ListAccountAssignmentDeletionStatusResponse (..),
    newListAccountAssignmentDeletionStatusResponse,

    -- * Response Lenses
    listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus,
    listAccountAssignmentDeletionStatusResponse_nextToken,
    listAccountAssignmentDeletionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListAccountAssignmentDeletionStatus' smart constructor.
data ListAccountAssignmentDeletionStatus = ListAccountAssignmentDeletionStatus'
  { -- | Filters results based on the passed attribute value.
    filter' :: Prelude.Maybe OperationStatusFilter,
    -- | The maximum number of results to display for the assignment.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignmentDeletionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAccountAssignmentDeletionStatus_filter' - Filters results based on the passed attribute value.
--
-- 'maxResults', 'listAccountAssignmentDeletionStatus_maxResults' - The maximum number of results to display for the assignment.
--
-- 'nextToken', 'listAccountAssignmentDeletionStatus_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'instanceArn', 'listAccountAssignmentDeletionStatus_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newListAccountAssignmentDeletionStatus ::
  -- | 'instanceArn'
  Prelude.Text ->
  ListAccountAssignmentDeletionStatus
newListAccountAssignmentDeletionStatus pInstanceArn_ =
  ListAccountAssignmentDeletionStatus'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceArn = pInstanceArn_
    }

-- | Filters results based on the passed attribute value.
listAccountAssignmentDeletionStatus_filter :: Lens.Lens' ListAccountAssignmentDeletionStatus (Prelude.Maybe OperationStatusFilter)
listAccountAssignmentDeletionStatus_filter = Lens.lens (\ListAccountAssignmentDeletionStatus' {filter'} -> filter') (\s@ListAccountAssignmentDeletionStatus' {} a -> s {filter' = a} :: ListAccountAssignmentDeletionStatus)

-- | The maximum number of results to display for the assignment.
listAccountAssignmentDeletionStatus_maxResults :: Lens.Lens' ListAccountAssignmentDeletionStatus (Prelude.Maybe Prelude.Natural)
listAccountAssignmentDeletionStatus_maxResults = Lens.lens (\ListAccountAssignmentDeletionStatus' {maxResults} -> maxResults) (\s@ListAccountAssignmentDeletionStatus' {} a -> s {maxResults = a} :: ListAccountAssignmentDeletionStatus)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignmentDeletionStatus_nextToken :: Lens.Lens' ListAccountAssignmentDeletionStatus (Prelude.Maybe Prelude.Text)
listAccountAssignmentDeletionStatus_nextToken = Lens.lens (\ListAccountAssignmentDeletionStatus' {nextToken} -> nextToken) (\s@ListAccountAssignmentDeletionStatus' {} a -> s {nextToken = a} :: ListAccountAssignmentDeletionStatus)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listAccountAssignmentDeletionStatus_instanceArn :: Lens.Lens' ListAccountAssignmentDeletionStatus Prelude.Text
listAccountAssignmentDeletionStatus_instanceArn = Lens.lens (\ListAccountAssignmentDeletionStatus' {instanceArn} -> instanceArn) (\s@ListAccountAssignmentDeletionStatus' {} a -> s {instanceArn = a} :: ListAccountAssignmentDeletionStatus)

instance
  Core.AWSPager
    ListAccountAssignmentDeletionStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentDeletionStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountAssignmentDeletionStatus_nextToken
          Lens..~ rs
          Lens.^? listAccountAssignmentDeletionStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAccountAssignmentDeletionStatus
  where
  type
    AWSResponse ListAccountAssignmentDeletionStatus =
      ListAccountAssignmentDeletionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountAssignmentDeletionStatusResponse'
            Prelude.<$> ( x Data..?> "AccountAssignmentsDeletionStatus"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAccountAssignmentDeletionStatus
  where
  hashWithSalt
    _salt
    ListAccountAssignmentDeletionStatus' {..} =
      _salt `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` instanceArn

instance
  Prelude.NFData
    ListAccountAssignmentDeletionStatus
  where
  rnf ListAccountAssignmentDeletionStatus' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceArn

instance
  Data.ToHeaders
    ListAccountAssignmentDeletionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListAccountAssignmentDeletionStatus" ::
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
    ListAccountAssignmentDeletionStatus
  where
  toJSON ListAccountAssignmentDeletionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InstanceArn" Data..= instanceArn)
          ]
      )

instance
  Data.ToPath
    ListAccountAssignmentDeletionStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAccountAssignmentDeletionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountAssignmentDeletionStatusResponse' smart constructor.
data ListAccountAssignmentDeletionStatusResponse = ListAccountAssignmentDeletionStatusResponse'
  { -- | The status object for the account assignment deletion operation.
    accountAssignmentsDeletionStatus :: Prelude.Maybe [AccountAssignmentOperationStatusMetadata],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAssignmentDeletionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAssignmentsDeletionStatus', 'listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus' - The status object for the account assignment deletion operation.
--
-- 'nextToken', 'listAccountAssignmentDeletionStatusResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listAccountAssignmentDeletionStatusResponse_httpStatus' - The response's http status code.
newListAccountAssignmentDeletionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountAssignmentDeletionStatusResponse
newListAccountAssignmentDeletionStatusResponse
  pHttpStatus_ =
    ListAccountAssignmentDeletionStatusResponse'
      { accountAssignmentsDeletionStatus =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status object for the account assignment deletion operation.
listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus :: Lens.Lens' ListAccountAssignmentDeletionStatusResponse (Prelude.Maybe [AccountAssignmentOperationStatusMetadata])
listAccountAssignmentDeletionStatusResponse_accountAssignmentsDeletionStatus = Lens.lens (\ListAccountAssignmentDeletionStatusResponse' {accountAssignmentsDeletionStatus} -> accountAssignmentsDeletionStatus) (\s@ListAccountAssignmentDeletionStatusResponse' {} a -> s {accountAssignmentsDeletionStatus = a} :: ListAccountAssignmentDeletionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountAssignmentDeletionStatusResponse_nextToken :: Lens.Lens' ListAccountAssignmentDeletionStatusResponse (Prelude.Maybe Prelude.Text)
listAccountAssignmentDeletionStatusResponse_nextToken = Lens.lens (\ListAccountAssignmentDeletionStatusResponse' {nextToken} -> nextToken) (\s@ListAccountAssignmentDeletionStatusResponse' {} a -> s {nextToken = a} :: ListAccountAssignmentDeletionStatusResponse)

-- | The response's http status code.
listAccountAssignmentDeletionStatusResponse_httpStatus :: Lens.Lens' ListAccountAssignmentDeletionStatusResponse Prelude.Int
listAccountAssignmentDeletionStatusResponse_httpStatus = Lens.lens (\ListAccountAssignmentDeletionStatusResponse' {httpStatus} -> httpStatus) (\s@ListAccountAssignmentDeletionStatusResponse' {} a -> s {httpStatus = a} :: ListAccountAssignmentDeletionStatusResponse)

instance
  Prelude.NFData
    ListAccountAssignmentDeletionStatusResponse
  where
  rnf ListAccountAssignmentDeletionStatusResponse' {..} =
    Prelude.rnf accountAssignmentsDeletionStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

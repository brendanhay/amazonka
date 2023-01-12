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
-- Module      : Amazonka.SSOAdmin.ListPermissionSetProvisioningStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of the permission set provisioning requests for a
-- specified IAM Identity Center instance.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListPermissionSetProvisioningStatus
  ( -- * Creating a Request
    ListPermissionSetProvisioningStatus (..),
    newListPermissionSetProvisioningStatus,

    -- * Request Lenses
    listPermissionSetProvisioningStatus_filter,
    listPermissionSetProvisioningStatus_maxResults,
    listPermissionSetProvisioningStatus_nextToken,
    listPermissionSetProvisioningStatus_instanceArn,

    -- * Destructuring the Response
    ListPermissionSetProvisioningStatusResponse (..),
    newListPermissionSetProvisioningStatusResponse,

    -- * Response Lenses
    listPermissionSetProvisioningStatusResponse_nextToken,
    listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus,
    listPermissionSetProvisioningStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListPermissionSetProvisioningStatus' smart constructor.
data ListPermissionSetProvisioningStatus = ListPermissionSetProvisioningStatus'
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
-- Create a value of 'ListPermissionSetProvisioningStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listPermissionSetProvisioningStatus_filter' - Filters results based on the passed attribute value.
--
-- 'maxResults', 'listPermissionSetProvisioningStatus_maxResults' - The maximum number of results to display for the assignment.
--
-- 'nextToken', 'listPermissionSetProvisioningStatus_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'instanceArn', 'listPermissionSetProvisioningStatus_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newListPermissionSetProvisioningStatus ::
  -- | 'instanceArn'
  Prelude.Text ->
  ListPermissionSetProvisioningStatus
newListPermissionSetProvisioningStatus pInstanceArn_ =
  ListPermissionSetProvisioningStatus'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceArn = pInstanceArn_
    }

-- | Filters results based on the passed attribute value.
listPermissionSetProvisioningStatus_filter :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe OperationStatusFilter)
listPermissionSetProvisioningStatus_filter = Lens.lens (\ListPermissionSetProvisioningStatus' {filter'} -> filter') (\s@ListPermissionSetProvisioningStatus' {} a -> s {filter' = a} :: ListPermissionSetProvisioningStatus)

-- | The maximum number of results to display for the assignment.
listPermissionSetProvisioningStatus_maxResults :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe Prelude.Natural)
listPermissionSetProvisioningStatus_maxResults = Lens.lens (\ListPermissionSetProvisioningStatus' {maxResults} -> maxResults) (\s@ListPermissionSetProvisioningStatus' {} a -> s {maxResults = a} :: ListPermissionSetProvisioningStatus)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetProvisioningStatus_nextToken :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe Prelude.Text)
listPermissionSetProvisioningStatus_nextToken = Lens.lens (\ListPermissionSetProvisioningStatus' {nextToken} -> nextToken) (\s@ListPermissionSetProvisioningStatus' {} a -> s {nextToken = a} :: ListPermissionSetProvisioningStatus)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listPermissionSetProvisioningStatus_instanceArn :: Lens.Lens' ListPermissionSetProvisioningStatus Prelude.Text
listPermissionSetProvisioningStatus_instanceArn = Lens.lens (\ListPermissionSetProvisioningStatus' {instanceArn} -> instanceArn) (\s@ListPermissionSetProvisioningStatus' {} a -> s {instanceArn = a} :: ListPermissionSetProvisioningStatus)

instance
  Core.AWSPager
    ListPermissionSetProvisioningStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPermissionSetProvisioningStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPermissionSetProvisioningStatus_nextToken
          Lens..~ rs
          Lens.^? listPermissionSetProvisioningStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPermissionSetProvisioningStatus
  where
  type
    AWSResponse ListPermissionSetProvisioningStatus =
      ListPermissionSetProvisioningStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionSetProvisioningStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "PermissionSetsProvisioningStatus"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPermissionSetProvisioningStatus
  where
  hashWithSalt
    _salt
    ListPermissionSetProvisioningStatus' {..} =
      _salt `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` instanceArn

instance
  Prelude.NFData
    ListPermissionSetProvisioningStatus
  where
  rnf ListPermissionSetProvisioningStatus' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceArn

instance
  Data.ToHeaders
    ListPermissionSetProvisioningStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListPermissionSetProvisioningStatus" ::
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
    ListPermissionSetProvisioningStatus
  where
  toJSON ListPermissionSetProvisioningStatus' {..} =
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
    ListPermissionSetProvisioningStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListPermissionSetProvisioningStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionSetProvisioningStatusResponse' smart constructor.
data ListPermissionSetProvisioningStatusResponse = ListPermissionSetProvisioningStatusResponse'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status object for the permission set provisioning operation.
    permissionSetsProvisioningStatus :: Prelude.Maybe [PermissionSetProvisioningStatusMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionSetProvisioningStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionSetProvisioningStatusResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'permissionSetsProvisioningStatus', 'listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus' - The status object for the permission set provisioning operation.
--
-- 'httpStatus', 'listPermissionSetProvisioningStatusResponse_httpStatus' - The response's http status code.
newListPermissionSetProvisioningStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionSetProvisioningStatusResponse
newListPermissionSetProvisioningStatusResponse
  pHttpStatus_ =
    ListPermissionSetProvisioningStatusResponse'
      { nextToken =
          Prelude.Nothing,
        permissionSetsProvisioningStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetProvisioningStatusResponse_nextToken :: Lens.Lens' ListPermissionSetProvisioningStatusResponse (Prelude.Maybe Prelude.Text)
listPermissionSetProvisioningStatusResponse_nextToken = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {nextToken} -> nextToken) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {nextToken = a} :: ListPermissionSetProvisioningStatusResponse)

-- | The status object for the permission set provisioning operation.
listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus :: Lens.Lens' ListPermissionSetProvisioningStatusResponse (Prelude.Maybe [PermissionSetProvisioningStatusMetadata])
listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {permissionSetsProvisioningStatus} -> permissionSetsProvisioningStatus) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {permissionSetsProvisioningStatus = a} :: ListPermissionSetProvisioningStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionSetProvisioningStatusResponse_httpStatus :: Lens.Lens' ListPermissionSetProvisioningStatusResponse Prelude.Int
listPermissionSetProvisioningStatusResponse_httpStatus = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {httpStatus} -> httpStatus) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {httpStatus = a} :: ListPermissionSetProvisioningStatusResponse)

instance
  Prelude.NFData
    ListPermissionSetProvisioningStatusResponse
  where
  rnf ListPermissionSetProvisioningStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionSetsProvisioningStatus
      `Prelude.seq` Prelude.rnf httpStatus

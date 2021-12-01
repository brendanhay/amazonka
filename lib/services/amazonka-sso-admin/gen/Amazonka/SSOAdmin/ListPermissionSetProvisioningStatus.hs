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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the status of the permission set provisioning requests for a
-- specified SSO instance.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListPermissionSetProvisioningStatus
  ( -- * Creating a Request
    ListPermissionSetProvisioningStatus (..),
    newListPermissionSetProvisioningStatus,

    -- * Request Lenses
    listPermissionSetProvisioningStatus_nextToken,
    listPermissionSetProvisioningStatus_filter,
    listPermissionSetProvisioningStatus_maxResults,
    listPermissionSetProvisioningStatus_instanceArn,

    -- * Destructuring the Response
    ListPermissionSetProvisioningStatusResponse (..),
    newListPermissionSetProvisioningStatusResponse,

    -- * Response Lenses
    listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus,
    listPermissionSetProvisioningStatusResponse_nextToken,
    listPermissionSetProvisioningStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListPermissionSetProvisioningStatus' smart constructor.
data ListPermissionSetProvisioningStatus = ListPermissionSetProvisioningStatus'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters results based on the passed attribute value.
    filter' :: Prelude.Maybe OperationStatusFilter,
    -- | The maximum number of results to display for the assignment.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the SSO instance under which the operation will be executed.
    -- For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
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
-- 'nextToken', 'listPermissionSetProvisioningStatus_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'filter'', 'listPermissionSetProvisioningStatus_filter' - Filters results based on the passed attribute value.
--
-- 'maxResults', 'listPermissionSetProvisioningStatus_maxResults' - The maximum number of results to display for the assignment.
--
-- 'instanceArn', 'listPermissionSetProvisioningStatus_instanceArn' - The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
newListPermissionSetProvisioningStatus ::
  -- | 'instanceArn'
  Prelude.Text ->
  ListPermissionSetProvisioningStatus
newListPermissionSetProvisioningStatus pInstanceArn_ =
  ListPermissionSetProvisioningStatus'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceArn = pInstanceArn_
    }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetProvisioningStatus_nextToken :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe Prelude.Text)
listPermissionSetProvisioningStatus_nextToken = Lens.lens (\ListPermissionSetProvisioningStatus' {nextToken} -> nextToken) (\s@ListPermissionSetProvisioningStatus' {} a -> s {nextToken = a} :: ListPermissionSetProvisioningStatus)

-- | Filters results based on the passed attribute value.
listPermissionSetProvisioningStatus_filter :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe OperationStatusFilter)
listPermissionSetProvisioningStatus_filter = Lens.lens (\ListPermissionSetProvisioningStatus' {filter'} -> filter') (\s@ListPermissionSetProvisioningStatus' {} a -> s {filter' = a} :: ListPermissionSetProvisioningStatus)

-- | The maximum number of results to display for the assignment.
listPermissionSetProvisioningStatus_maxResults :: Lens.Lens' ListPermissionSetProvisioningStatus (Prelude.Maybe Prelude.Natural)
listPermissionSetProvisioningStatus_maxResults = Lens.lens (\ListPermissionSetProvisioningStatus' {maxResults} -> maxResults) (\s@ListPermissionSetProvisioningStatus' {} a -> s {maxResults = a} :: ListPermissionSetProvisioningStatus)

-- | The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionSetProvisioningStatusResponse'
            Prelude.<$> ( x Core..?> "PermissionSetsProvisioningStatus"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPermissionSetProvisioningStatus
  where
  hashWithSalt
    salt'
    ListPermissionSetProvisioningStatus' {..} =
      salt' `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListPermissionSetProvisioningStatus
  where
  rnf ListPermissionSetProvisioningStatus' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filter'

instance
  Core.ToHeaders
    ListPermissionSetProvisioningStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.ListPermissionSetProvisioningStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListPermissionSetProvisioningStatus
  where
  toJSON ListPermissionSetProvisioningStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Core..= instanceArn)
          ]
      )

instance
  Core.ToPath
    ListPermissionSetProvisioningStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListPermissionSetProvisioningStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionSetProvisioningStatusResponse' smart constructor.
data ListPermissionSetProvisioningStatusResponse = ListPermissionSetProvisioningStatusResponse'
  { -- | The status object for the permission set provisioning operation.
    permissionSetsProvisioningStatus :: Prelude.Maybe [PermissionSetProvisioningStatusMetadata],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'permissionSetsProvisioningStatus', 'listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus' - The status object for the permission set provisioning operation.
--
-- 'nextToken', 'listPermissionSetProvisioningStatusResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listPermissionSetProvisioningStatusResponse_httpStatus' - The response's http status code.
newListPermissionSetProvisioningStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionSetProvisioningStatusResponse
newListPermissionSetProvisioningStatusResponse
  pHttpStatus_ =
    ListPermissionSetProvisioningStatusResponse'
      { permissionSetsProvisioningStatus =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status object for the permission set provisioning operation.
listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus :: Lens.Lens' ListPermissionSetProvisioningStatusResponse (Prelude.Maybe [PermissionSetProvisioningStatusMetadata])
listPermissionSetProvisioningStatusResponse_permissionSetsProvisioningStatus = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {permissionSetsProvisioningStatus} -> permissionSetsProvisioningStatus) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {permissionSetsProvisioningStatus = a} :: ListPermissionSetProvisioningStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetProvisioningStatusResponse_nextToken :: Lens.Lens' ListPermissionSetProvisioningStatusResponse (Prelude.Maybe Prelude.Text)
listPermissionSetProvisioningStatusResponse_nextToken = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {nextToken} -> nextToken) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {nextToken = a} :: ListPermissionSetProvisioningStatusResponse)

-- | The response's http status code.
listPermissionSetProvisioningStatusResponse_httpStatus :: Lens.Lens' ListPermissionSetProvisioningStatusResponse Prelude.Int
listPermissionSetProvisioningStatusResponse_httpStatus = Lens.lens (\ListPermissionSetProvisioningStatusResponse' {httpStatus} -> httpStatus) (\s@ListPermissionSetProvisioningStatusResponse' {} a -> s {httpStatus = a} :: ListPermissionSetProvisioningStatusResponse)

instance
  Prelude.NFData
    ListPermissionSetProvisioningStatusResponse
  where
  rnf ListPermissionSetProvisioningStatusResponse' {..} =
    Prelude.rnf permissionSetsProvisioningStatus
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken

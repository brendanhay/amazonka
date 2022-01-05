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
-- Module      : Amazonka.SSOAdmin.ListPermissionSetsProvisionedToAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the permission sets that are provisioned to a specified Amazon
-- Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListPermissionSetsProvisionedToAccount
  ( -- * Creating a Request
    ListPermissionSetsProvisionedToAccount (..),
    newListPermissionSetsProvisionedToAccount,

    -- * Request Lenses
    listPermissionSetsProvisionedToAccount_provisioningStatus,
    listPermissionSetsProvisionedToAccount_nextToken,
    listPermissionSetsProvisionedToAccount_maxResults,
    listPermissionSetsProvisionedToAccount_instanceArn,
    listPermissionSetsProvisionedToAccount_accountId,

    -- * Destructuring the Response
    ListPermissionSetsProvisionedToAccountResponse (..),
    newListPermissionSetsProvisionedToAccountResponse,

    -- * Response Lenses
    listPermissionSetsProvisionedToAccountResponse_permissionSets,
    listPermissionSetsProvisionedToAccountResponse_nextToken,
    listPermissionSetsProvisionedToAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListPermissionSetsProvisionedToAccount' smart constructor.
data ListPermissionSetsProvisionedToAccount = ListPermissionSetsProvisionedToAccount'
  { -- | The status object for the permission set provisioning operation.
    provisioningStatus :: Prelude.Maybe ProvisioningStatus,
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to display for the assignment.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the SSO instance under which the operation will be executed.
    -- For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    instanceArn :: Prelude.Text,
    -- | The identifier of the Amazon Web Services account from which to list the
    -- assignments.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionSetsProvisionedToAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningStatus', 'listPermissionSetsProvisionedToAccount_provisioningStatus' - The status object for the permission set provisioning operation.
--
-- 'nextToken', 'listPermissionSetsProvisionedToAccount_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'maxResults', 'listPermissionSetsProvisionedToAccount_maxResults' - The maximum number of results to display for the assignment.
--
-- 'instanceArn', 'listPermissionSetsProvisionedToAccount_instanceArn' - The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
--
-- 'accountId', 'listPermissionSetsProvisionedToAccount_accountId' - The identifier of the Amazon Web Services account from which to list the
-- assignments.
newListPermissionSetsProvisionedToAccount ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  ListPermissionSetsProvisionedToAccount
newListPermissionSetsProvisionedToAccount
  pInstanceArn_
  pAccountId_ =
    ListPermissionSetsProvisionedToAccount'
      { provisioningStatus =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        instanceArn = pInstanceArn_,
        accountId = pAccountId_
      }

-- | The status object for the permission set provisioning operation.
listPermissionSetsProvisionedToAccount_provisioningStatus :: Lens.Lens' ListPermissionSetsProvisionedToAccount (Prelude.Maybe ProvisioningStatus)
listPermissionSetsProvisionedToAccount_provisioningStatus = Lens.lens (\ListPermissionSetsProvisionedToAccount' {provisioningStatus} -> provisioningStatus) (\s@ListPermissionSetsProvisionedToAccount' {} a -> s {provisioningStatus = a} :: ListPermissionSetsProvisionedToAccount)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetsProvisionedToAccount_nextToken :: Lens.Lens' ListPermissionSetsProvisionedToAccount (Prelude.Maybe Prelude.Text)
listPermissionSetsProvisionedToAccount_nextToken = Lens.lens (\ListPermissionSetsProvisionedToAccount' {nextToken} -> nextToken) (\s@ListPermissionSetsProvisionedToAccount' {} a -> s {nextToken = a} :: ListPermissionSetsProvisionedToAccount)

-- | The maximum number of results to display for the assignment.
listPermissionSetsProvisionedToAccount_maxResults :: Lens.Lens' ListPermissionSetsProvisionedToAccount (Prelude.Maybe Prelude.Natural)
listPermissionSetsProvisionedToAccount_maxResults = Lens.lens (\ListPermissionSetsProvisionedToAccount' {maxResults} -> maxResults) (\s@ListPermissionSetsProvisionedToAccount' {} a -> s {maxResults = a} :: ListPermissionSetsProvisionedToAccount)

-- | The ARN of the SSO instance under which the operation will be executed.
-- For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
listPermissionSetsProvisionedToAccount_instanceArn :: Lens.Lens' ListPermissionSetsProvisionedToAccount Prelude.Text
listPermissionSetsProvisionedToAccount_instanceArn = Lens.lens (\ListPermissionSetsProvisionedToAccount' {instanceArn} -> instanceArn) (\s@ListPermissionSetsProvisionedToAccount' {} a -> s {instanceArn = a} :: ListPermissionSetsProvisionedToAccount)

-- | The identifier of the Amazon Web Services account from which to list the
-- assignments.
listPermissionSetsProvisionedToAccount_accountId :: Lens.Lens' ListPermissionSetsProvisionedToAccount Prelude.Text
listPermissionSetsProvisionedToAccount_accountId = Lens.lens (\ListPermissionSetsProvisionedToAccount' {accountId} -> accountId) (\s@ListPermissionSetsProvisionedToAccount' {} a -> s {accountId = a} :: ListPermissionSetsProvisionedToAccount)

instance
  Core.AWSPager
    ListPermissionSetsProvisionedToAccount
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPermissionSetsProvisionedToAccountResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPermissionSetsProvisionedToAccountResponse_permissionSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPermissionSetsProvisionedToAccount_nextToken
          Lens..~ rs
            Lens.^? listPermissionSetsProvisionedToAccountResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPermissionSetsProvisionedToAccount
  where
  type
    AWSResponse
      ListPermissionSetsProvisionedToAccount =
      ListPermissionSetsProvisionedToAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionSetsProvisionedToAccountResponse'
            Prelude.<$> (x Core..?> "PermissionSets" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPermissionSetsProvisionedToAccount
  where
  hashWithSalt
    _salt
    ListPermissionSetsProvisionedToAccount' {..} =
      _salt `Prelude.hashWithSalt` provisioningStatus
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    ListPermissionSetsProvisionedToAccount
  where
  rnf ListPermissionSetsProvisionedToAccount' {..} =
    Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf accountId

instance
  Core.ToHeaders
    ListPermissionSetsProvisionedToAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.ListPermissionSetsProvisionedToAccount" ::
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
    ListPermissionSetsProvisionedToAccount
  where
  toJSON ListPermissionSetsProvisionedToAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProvisioningStatus" Core..=)
              Prelude.<$> provisioningStatus,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Core..= instanceArn),
            Prelude.Just ("AccountId" Core..= accountId)
          ]
      )

instance
  Core.ToPath
    ListPermissionSetsProvisionedToAccount
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListPermissionSetsProvisionedToAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionSetsProvisionedToAccountResponse' smart constructor.
data ListPermissionSetsProvisionedToAccountResponse = ListPermissionSetsProvisionedToAccountResponse'
  { -- | Defines the level of access that an Amazon Web Services account has.
    permissionSets :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionSetsProvisionedToAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionSets', 'listPermissionSetsProvisionedToAccountResponse_permissionSets' - Defines the level of access that an Amazon Web Services account has.
--
-- 'nextToken', 'listPermissionSetsProvisionedToAccountResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listPermissionSetsProvisionedToAccountResponse_httpStatus' - The response's http status code.
newListPermissionSetsProvisionedToAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionSetsProvisionedToAccountResponse
newListPermissionSetsProvisionedToAccountResponse
  pHttpStatus_ =
    ListPermissionSetsProvisionedToAccountResponse'
      { permissionSets =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Defines the level of access that an Amazon Web Services account has.
listPermissionSetsProvisionedToAccountResponse_permissionSets :: Lens.Lens' ListPermissionSetsProvisionedToAccountResponse (Prelude.Maybe [Prelude.Text])
listPermissionSetsProvisionedToAccountResponse_permissionSets = Lens.lens (\ListPermissionSetsProvisionedToAccountResponse' {permissionSets} -> permissionSets) (\s@ListPermissionSetsProvisionedToAccountResponse' {} a -> s {permissionSets = a} :: ListPermissionSetsProvisionedToAccountResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetsProvisionedToAccountResponse_nextToken :: Lens.Lens' ListPermissionSetsProvisionedToAccountResponse (Prelude.Maybe Prelude.Text)
listPermissionSetsProvisionedToAccountResponse_nextToken = Lens.lens (\ListPermissionSetsProvisionedToAccountResponse' {nextToken} -> nextToken) (\s@ListPermissionSetsProvisionedToAccountResponse' {} a -> s {nextToken = a} :: ListPermissionSetsProvisionedToAccountResponse)

-- | The response's http status code.
listPermissionSetsProvisionedToAccountResponse_httpStatus :: Lens.Lens' ListPermissionSetsProvisionedToAccountResponse Prelude.Int
listPermissionSetsProvisionedToAccountResponse_httpStatus = Lens.lens (\ListPermissionSetsProvisionedToAccountResponse' {httpStatus} -> httpStatus) (\s@ListPermissionSetsProvisionedToAccountResponse' {} a -> s {httpStatus = a} :: ListPermissionSetsProvisionedToAccountResponse)

instance
  Prelude.NFData
    ListPermissionSetsProvisionedToAccountResponse
  where
  rnf
    ListPermissionSetsProvisionedToAccountResponse' {..} =
      Prelude.rnf permissionSets
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.SSOAdmin.ListAccountsForProvisionedPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the AWS accounts where the specified permission set is
-- provisioned.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListAccountsForProvisionedPermissionSet
  ( -- * Creating a Request
    ListAccountsForProvisionedPermissionSet (..),
    newListAccountsForProvisionedPermissionSet,

    -- * Request Lenses
    listAccountsForProvisionedPermissionSet_maxResults,
    listAccountsForProvisionedPermissionSet_nextToken,
    listAccountsForProvisionedPermissionSet_provisioningStatus,
    listAccountsForProvisionedPermissionSet_instanceArn,
    listAccountsForProvisionedPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    ListAccountsForProvisionedPermissionSetResponse (..),
    newListAccountsForProvisionedPermissionSetResponse,

    -- * Response Lenses
    listAccountsForProvisionedPermissionSetResponse_accountIds,
    listAccountsForProvisionedPermissionSetResponse_nextToken,
    listAccountsForProvisionedPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListAccountsForProvisionedPermissionSet' smart constructor.
data ListAccountsForProvisionedPermissionSet = ListAccountsForProvisionedPermissionSet'
  { -- | The maximum number of results to display for the PermissionSet.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The permission set provisioning status for an AWS account.
    provisioningStatus :: Prelude.Maybe ProvisioningStatus,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the PermissionSet from which the associated AWS accounts will
    -- be listed.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsForProvisionedPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccountsForProvisionedPermissionSet_maxResults' - The maximum number of results to display for the PermissionSet.
--
-- 'nextToken', 'listAccountsForProvisionedPermissionSet_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'provisioningStatus', 'listAccountsForProvisionedPermissionSet_provisioningStatus' - The permission set provisioning status for an AWS account.
--
-- 'instanceArn', 'listAccountsForProvisionedPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'listAccountsForProvisionedPermissionSet_permissionSetArn' - The ARN of the PermissionSet from which the associated AWS accounts will
-- be listed.
newListAccountsForProvisionedPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  ListAccountsForProvisionedPermissionSet
newListAccountsForProvisionedPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    ListAccountsForProvisionedPermissionSet'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        provisioningStatus =
          Prelude.Nothing,
        instanceArn = pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_
      }

-- | The maximum number of results to display for the PermissionSet.
listAccountsForProvisionedPermissionSet_maxResults :: Lens.Lens' ListAccountsForProvisionedPermissionSet (Prelude.Maybe Prelude.Natural)
listAccountsForProvisionedPermissionSet_maxResults = Lens.lens (\ListAccountsForProvisionedPermissionSet' {maxResults} -> maxResults) (\s@ListAccountsForProvisionedPermissionSet' {} a -> s {maxResults = a} :: ListAccountsForProvisionedPermissionSet)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountsForProvisionedPermissionSet_nextToken :: Lens.Lens' ListAccountsForProvisionedPermissionSet (Prelude.Maybe Prelude.Text)
listAccountsForProvisionedPermissionSet_nextToken = Lens.lens (\ListAccountsForProvisionedPermissionSet' {nextToken} -> nextToken) (\s@ListAccountsForProvisionedPermissionSet' {} a -> s {nextToken = a} :: ListAccountsForProvisionedPermissionSet)

-- | The permission set provisioning status for an AWS account.
listAccountsForProvisionedPermissionSet_provisioningStatus :: Lens.Lens' ListAccountsForProvisionedPermissionSet (Prelude.Maybe ProvisioningStatus)
listAccountsForProvisionedPermissionSet_provisioningStatus = Lens.lens (\ListAccountsForProvisionedPermissionSet' {provisioningStatus} -> provisioningStatus) (\s@ListAccountsForProvisionedPermissionSet' {} a -> s {provisioningStatus = a} :: ListAccountsForProvisionedPermissionSet)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listAccountsForProvisionedPermissionSet_instanceArn :: Lens.Lens' ListAccountsForProvisionedPermissionSet Prelude.Text
listAccountsForProvisionedPermissionSet_instanceArn = Lens.lens (\ListAccountsForProvisionedPermissionSet' {instanceArn} -> instanceArn) (\s@ListAccountsForProvisionedPermissionSet' {} a -> s {instanceArn = a} :: ListAccountsForProvisionedPermissionSet)

-- | The ARN of the PermissionSet from which the associated AWS accounts will
-- be listed.
listAccountsForProvisionedPermissionSet_permissionSetArn :: Lens.Lens' ListAccountsForProvisionedPermissionSet Prelude.Text
listAccountsForProvisionedPermissionSet_permissionSetArn = Lens.lens (\ListAccountsForProvisionedPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@ListAccountsForProvisionedPermissionSet' {} a -> s {permissionSetArn = a} :: ListAccountsForProvisionedPermissionSet)

instance
  Core.AWSPager
    ListAccountsForProvisionedPermissionSet
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountsForProvisionedPermissionSetResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountsForProvisionedPermissionSetResponse_accountIds
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAccountsForProvisionedPermissionSet_nextToken
          Lens..~ rs
          Lens.^? listAccountsForProvisionedPermissionSetResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAccountsForProvisionedPermissionSet
  where
  type
    AWSResponse
      ListAccountsForProvisionedPermissionSet =
      ListAccountsForProvisionedPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountsForProvisionedPermissionSetResponse'
            Prelude.<$> (x Data..?> "AccountIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAccountsForProvisionedPermissionSet
  where
  hashWithSalt
    _salt
    ListAccountsForProvisionedPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` provisioningStatus
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    ListAccountsForProvisionedPermissionSet
  where
  rnf ListAccountsForProvisionedPermissionSet' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf provisioningStatus
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Data.ToHeaders
    ListAccountsForProvisionedPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListAccountsForProvisionedPermissionSet" ::
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
    ListAccountsForProvisionedPermissionSet
  where
  toJSON ListAccountsForProvisionedPermissionSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ProvisioningStatus" Data..=)
              Prelude.<$> provisioningStatus,
            Prelude.Just ("InstanceArn" Data..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Data..= permissionSetArn)
          ]
      )

instance
  Data.ToPath
    ListAccountsForProvisionedPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAccountsForProvisionedPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountsForProvisionedPermissionSetResponse' smart constructor.
data ListAccountsForProvisionedPermissionSetResponse = ListAccountsForProvisionedPermissionSetResponse'
  { -- | The list of AWS @AccountIds@.
    accountIds :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountsForProvisionedPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'listAccountsForProvisionedPermissionSetResponse_accountIds' - The list of AWS @AccountIds@.
--
-- 'nextToken', 'listAccountsForProvisionedPermissionSetResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listAccountsForProvisionedPermissionSetResponse_httpStatus' - The response's http status code.
newListAccountsForProvisionedPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountsForProvisionedPermissionSetResponse
newListAccountsForProvisionedPermissionSetResponse
  pHttpStatus_ =
    ListAccountsForProvisionedPermissionSetResponse'
      { accountIds =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of AWS @AccountIds@.
listAccountsForProvisionedPermissionSetResponse_accountIds :: Lens.Lens' ListAccountsForProvisionedPermissionSetResponse (Prelude.Maybe [Prelude.Text])
listAccountsForProvisionedPermissionSetResponse_accountIds = Lens.lens (\ListAccountsForProvisionedPermissionSetResponse' {accountIds} -> accountIds) (\s@ListAccountsForProvisionedPermissionSetResponse' {} a -> s {accountIds = a} :: ListAccountsForProvisionedPermissionSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listAccountsForProvisionedPermissionSetResponse_nextToken :: Lens.Lens' ListAccountsForProvisionedPermissionSetResponse (Prelude.Maybe Prelude.Text)
listAccountsForProvisionedPermissionSetResponse_nextToken = Lens.lens (\ListAccountsForProvisionedPermissionSetResponse' {nextToken} -> nextToken) (\s@ListAccountsForProvisionedPermissionSetResponse' {} a -> s {nextToken = a} :: ListAccountsForProvisionedPermissionSetResponse)

-- | The response's http status code.
listAccountsForProvisionedPermissionSetResponse_httpStatus :: Lens.Lens' ListAccountsForProvisionedPermissionSetResponse Prelude.Int
listAccountsForProvisionedPermissionSetResponse_httpStatus = Lens.lens (\ListAccountsForProvisionedPermissionSetResponse' {httpStatus} -> httpStatus) (\s@ListAccountsForProvisionedPermissionSetResponse' {} a -> s {httpStatus = a} :: ListAccountsForProvisionedPermissionSetResponse)

instance
  Prelude.NFData
    ListAccountsForProvisionedPermissionSetResponse
  where
  rnf
    ListAccountsForProvisionedPermissionSetResponse' {..} =
      Prelude.rnf accountIds
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus

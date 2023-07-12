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
-- Module      : Amazonka.SSOAdmin.ListCustomerManagedPolicyReferencesInPermissionSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all customer managed policies attached to a specified
-- PermissionSet.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListCustomerManagedPolicyReferencesInPermissionSet
  ( -- * Creating a Request
    ListCustomerManagedPolicyReferencesInPermissionSet (..),
    newListCustomerManagedPolicyReferencesInPermissionSet,

    -- * Request Lenses
    listCustomerManagedPolicyReferencesInPermissionSet_maxResults,
    listCustomerManagedPolicyReferencesInPermissionSet_nextToken,
    listCustomerManagedPolicyReferencesInPermissionSet_instanceArn,
    listCustomerManagedPolicyReferencesInPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    ListCustomerManagedPolicyReferencesInPermissionSetResponse (..),
    newListCustomerManagedPolicyReferencesInPermissionSetResponse,

    -- * Response Lenses
    listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences,
    listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken,
    listCustomerManagedPolicyReferencesInPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListCustomerManagedPolicyReferencesInPermissionSet' smart constructor.
data ListCustomerManagedPolicyReferencesInPermissionSet = ListCustomerManagedPolicyReferencesInPermissionSet'
  { -- | The maximum number of results to display for the list call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed.
    instanceArn :: Prelude.Text,
    -- | The ARN of the @PermissionSet@.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomerManagedPolicyReferencesInPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCustomerManagedPolicyReferencesInPermissionSet_maxResults' - The maximum number of results to display for the list call.
--
-- 'nextToken', 'listCustomerManagedPolicyReferencesInPermissionSet_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'instanceArn', 'listCustomerManagedPolicyReferencesInPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
--
-- 'permissionSetArn', 'listCustomerManagedPolicyReferencesInPermissionSet_permissionSetArn' - The ARN of the @PermissionSet@.
newListCustomerManagedPolicyReferencesInPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  ListCustomerManagedPolicyReferencesInPermissionSet
newListCustomerManagedPolicyReferencesInPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    ListCustomerManagedPolicyReferencesInPermissionSet'
      { maxResults =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        instanceArn =
          pInstanceArn_,
        permissionSetArn =
          pPermissionSetArn_
      }

-- | The maximum number of results to display for the list call.
listCustomerManagedPolicyReferencesInPermissionSet_maxResults :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSet (Prelude.Maybe Prelude.Natural)
listCustomerManagedPolicyReferencesInPermissionSet_maxResults = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSet' {maxResults} -> maxResults) (\s@ListCustomerManagedPolicyReferencesInPermissionSet' {} a -> s {maxResults = a} :: ListCustomerManagedPolicyReferencesInPermissionSet)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listCustomerManagedPolicyReferencesInPermissionSet_nextToken :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSet (Prelude.Maybe Prelude.Text)
listCustomerManagedPolicyReferencesInPermissionSet_nextToken = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSet' {nextToken} -> nextToken) (\s@ListCustomerManagedPolicyReferencesInPermissionSet' {} a -> s {nextToken = a} :: ListCustomerManagedPolicyReferencesInPermissionSet)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed.
listCustomerManagedPolicyReferencesInPermissionSet_instanceArn :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSet Prelude.Text
listCustomerManagedPolicyReferencesInPermissionSet_instanceArn = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSet' {instanceArn} -> instanceArn) (\s@ListCustomerManagedPolicyReferencesInPermissionSet' {} a -> s {instanceArn = a} :: ListCustomerManagedPolicyReferencesInPermissionSet)

-- | The ARN of the @PermissionSet@.
listCustomerManagedPolicyReferencesInPermissionSet_permissionSetArn :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSet Prelude.Text
listCustomerManagedPolicyReferencesInPermissionSet_permissionSetArn = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@ListCustomerManagedPolicyReferencesInPermissionSet' {} a -> s {permissionSetArn = a} :: ListCustomerManagedPolicyReferencesInPermissionSet)

instance
  Core.AWSPager
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCustomerManagedPolicyReferencesInPermissionSet_nextToken
          Lens..~ rs
          Lens.^? listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  type
    AWSResponse
      ListCustomerManagedPolicyReferencesInPermissionSet =
      ListCustomerManagedPolicyReferencesInPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomerManagedPolicyReferencesInPermissionSetResponse'
            Prelude.<$> ( x
                            Data..?> "CustomerManagedPolicyReferences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  hashWithSalt
    _salt
    ListCustomerManagedPolicyReferencesInPermissionSet' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  rnf
    ListCustomerManagedPolicyReferencesInPermissionSet' {..} =
      Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf instanceArn
        `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Data.ToHeaders
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListCustomerManagedPolicyReferencesInPermissionSet" ::
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
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  toJSON
    ListCustomerManagedPolicyReferencesInPermissionSet' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MaxResults" Data..=) Prelude.<$> maxResults,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              Prelude.Just ("InstanceArn" Data..= instanceArn),
              Prelude.Just
                ("PermissionSetArn" Data..= permissionSetArn)
            ]
        )

instance
  Data.ToPath
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListCustomerManagedPolicyReferencesInPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomerManagedPolicyReferencesInPermissionSetResponse' smart constructor.
data ListCustomerManagedPolicyReferencesInPermissionSetResponse = ListCustomerManagedPolicyReferencesInPermissionSetResponse'
  { -- | Specifies the names and paths of the customer managed policies that you
    -- have attached to your permission set.
    customerManagedPolicyReferences :: Prelude.Maybe [CustomerManagedPolicyReference],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomerManagedPolicyReferencesInPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedPolicyReferences', 'listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences' - Specifies the names and paths of the customer managed policies that you
-- have attached to your permission set.
--
-- 'nextToken', 'listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listCustomerManagedPolicyReferencesInPermissionSetResponse_httpStatus' - The response's http status code.
newListCustomerManagedPolicyReferencesInPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomerManagedPolicyReferencesInPermissionSetResponse
newListCustomerManagedPolicyReferencesInPermissionSetResponse
  pHttpStatus_ =
    ListCustomerManagedPolicyReferencesInPermissionSetResponse'
      { customerManagedPolicyReferences =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Specifies the names and paths of the customer managed policies that you
-- have attached to your permission set.
listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSetResponse (Prelude.Maybe [CustomerManagedPolicyReference])
listCustomerManagedPolicyReferencesInPermissionSetResponse_customerManagedPolicyReferences = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSetResponse' {customerManagedPolicyReferences} -> customerManagedPolicyReferences) (\s@ListCustomerManagedPolicyReferencesInPermissionSetResponse' {} a -> s {customerManagedPolicyReferences = a} :: ListCustomerManagedPolicyReferencesInPermissionSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSetResponse (Prelude.Maybe Prelude.Text)
listCustomerManagedPolicyReferencesInPermissionSetResponse_nextToken = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSetResponse' {nextToken} -> nextToken) (\s@ListCustomerManagedPolicyReferencesInPermissionSetResponse' {} a -> s {nextToken = a} :: ListCustomerManagedPolicyReferencesInPermissionSetResponse)

-- | The response's http status code.
listCustomerManagedPolicyReferencesInPermissionSetResponse_httpStatus :: Lens.Lens' ListCustomerManagedPolicyReferencesInPermissionSetResponse Prelude.Int
listCustomerManagedPolicyReferencesInPermissionSetResponse_httpStatus = Lens.lens (\ListCustomerManagedPolicyReferencesInPermissionSetResponse' {httpStatus} -> httpStatus) (\s@ListCustomerManagedPolicyReferencesInPermissionSetResponse' {} a -> s {httpStatus = a} :: ListCustomerManagedPolicyReferencesInPermissionSetResponse)

instance
  Prelude.NFData
    ListCustomerManagedPolicyReferencesInPermissionSetResponse
  where
  rnf
    ListCustomerManagedPolicyReferencesInPermissionSetResponse' {..} =
      Prelude.rnf customerManagedPolicyReferences
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus

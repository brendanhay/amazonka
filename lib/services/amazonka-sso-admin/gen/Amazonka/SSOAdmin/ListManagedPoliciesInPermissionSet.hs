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
-- Module      : Amazonka.SSOAdmin.ListManagedPoliciesInPermissionSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS managed policy that is attached to a specified permission
-- set.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListManagedPoliciesInPermissionSet
  ( -- * Creating a Request
    ListManagedPoliciesInPermissionSet (..),
    newListManagedPoliciesInPermissionSet,

    -- * Request Lenses
    listManagedPoliciesInPermissionSet_nextToken,
    listManagedPoliciesInPermissionSet_maxResults,
    listManagedPoliciesInPermissionSet_instanceArn,
    listManagedPoliciesInPermissionSet_permissionSetArn,

    -- * Destructuring the Response
    ListManagedPoliciesInPermissionSetResponse (..),
    newListManagedPoliciesInPermissionSetResponse,

    -- * Response Lenses
    listManagedPoliciesInPermissionSetResponse_nextToken,
    listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies,
    listManagedPoliciesInPermissionSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListManagedPoliciesInPermissionSet' smart constructor.
data ListManagedPoliciesInPermissionSet = ListManagedPoliciesInPermissionSet'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to display for the PermissionSet.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the IAM Identity Center instance under which the operation
    -- will be executed. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    instanceArn :: Prelude.Text,
    -- | The ARN of the PermissionSet whose managed policies will be listed.
    permissionSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedPoliciesInPermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedPoliciesInPermissionSet_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'maxResults', 'listManagedPoliciesInPermissionSet_maxResults' - The maximum number of results to display for the PermissionSet.
--
-- 'instanceArn', 'listManagedPoliciesInPermissionSet_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'permissionSetArn', 'listManagedPoliciesInPermissionSet_permissionSetArn' - The ARN of the PermissionSet whose managed policies will be listed.
newListManagedPoliciesInPermissionSet ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'permissionSetArn'
  Prelude.Text ->
  ListManagedPoliciesInPermissionSet
newListManagedPoliciesInPermissionSet
  pInstanceArn_
  pPermissionSetArn_ =
    ListManagedPoliciesInPermissionSet'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        instanceArn = pInstanceArn_,
        permissionSetArn = pPermissionSetArn_
      }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listManagedPoliciesInPermissionSet_nextToken :: Lens.Lens' ListManagedPoliciesInPermissionSet (Prelude.Maybe Prelude.Text)
listManagedPoliciesInPermissionSet_nextToken = Lens.lens (\ListManagedPoliciesInPermissionSet' {nextToken} -> nextToken) (\s@ListManagedPoliciesInPermissionSet' {} a -> s {nextToken = a} :: ListManagedPoliciesInPermissionSet)

-- | The maximum number of results to display for the PermissionSet.
listManagedPoliciesInPermissionSet_maxResults :: Lens.Lens' ListManagedPoliciesInPermissionSet (Prelude.Maybe Prelude.Natural)
listManagedPoliciesInPermissionSet_maxResults = Lens.lens (\ListManagedPoliciesInPermissionSet' {maxResults} -> maxResults) (\s@ListManagedPoliciesInPermissionSet' {} a -> s {maxResults = a} :: ListManagedPoliciesInPermissionSet)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listManagedPoliciesInPermissionSet_instanceArn :: Lens.Lens' ListManagedPoliciesInPermissionSet Prelude.Text
listManagedPoliciesInPermissionSet_instanceArn = Lens.lens (\ListManagedPoliciesInPermissionSet' {instanceArn} -> instanceArn) (\s@ListManagedPoliciesInPermissionSet' {} a -> s {instanceArn = a} :: ListManagedPoliciesInPermissionSet)

-- | The ARN of the PermissionSet whose managed policies will be listed.
listManagedPoliciesInPermissionSet_permissionSetArn :: Lens.Lens' ListManagedPoliciesInPermissionSet Prelude.Text
listManagedPoliciesInPermissionSet_permissionSetArn = Lens.lens (\ListManagedPoliciesInPermissionSet' {permissionSetArn} -> permissionSetArn) (\s@ListManagedPoliciesInPermissionSet' {} a -> s {permissionSetArn = a} :: ListManagedPoliciesInPermissionSet)

instance
  Core.AWSPager
    ListManagedPoliciesInPermissionSet
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedPoliciesInPermissionSetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listManagedPoliciesInPermissionSet_nextToken
          Lens..~ rs
          Lens.^? listManagedPoliciesInPermissionSetResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListManagedPoliciesInPermissionSet
  where
  type
    AWSResponse ListManagedPoliciesInPermissionSet =
      ListManagedPoliciesInPermissionSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedPoliciesInPermissionSetResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "AttachedManagedPolicies"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListManagedPoliciesInPermissionSet
  where
  hashWithSalt
    _salt
    ListManagedPoliciesInPermissionSet' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` permissionSetArn

instance
  Prelude.NFData
    ListManagedPoliciesInPermissionSet
  where
  rnf ListManagedPoliciesInPermissionSet' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf permissionSetArn

instance
  Core.ToHeaders
    ListManagedPoliciesInPermissionSet
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SWBExternalService.ListManagedPoliciesInPermissionSet" ::
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
    ListManagedPoliciesInPermissionSet
  where
  toJSON ListManagedPoliciesInPermissionSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Core..= instanceArn),
            Prelude.Just
              ("PermissionSetArn" Core..= permissionSetArn)
          ]
      )

instance
  Core.ToPath
    ListManagedPoliciesInPermissionSet
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListManagedPoliciesInPermissionSet
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListManagedPoliciesInPermissionSetResponse' smart constructor.
data ListManagedPoliciesInPermissionSetResponse = ListManagedPoliciesInPermissionSetResponse'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of the AttachedManagedPolicy data type object.
    attachedManagedPolicies :: Prelude.Maybe [AttachedManagedPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedPoliciesInPermissionSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedPoliciesInPermissionSetResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'attachedManagedPolicies', 'listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies' - An array of the AttachedManagedPolicy data type object.
--
-- 'httpStatus', 'listManagedPoliciesInPermissionSetResponse_httpStatus' - The response's http status code.
newListManagedPoliciesInPermissionSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedPoliciesInPermissionSetResponse
newListManagedPoliciesInPermissionSetResponse
  pHttpStatus_ =
    ListManagedPoliciesInPermissionSetResponse'
      { nextToken =
          Prelude.Nothing,
        attachedManagedPolicies =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listManagedPoliciesInPermissionSetResponse_nextToken :: Lens.Lens' ListManagedPoliciesInPermissionSetResponse (Prelude.Maybe Prelude.Text)
listManagedPoliciesInPermissionSetResponse_nextToken = Lens.lens (\ListManagedPoliciesInPermissionSetResponse' {nextToken} -> nextToken) (\s@ListManagedPoliciesInPermissionSetResponse' {} a -> s {nextToken = a} :: ListManagedPoliciesInPermissionSetResponse)

-- | An array of the AttachedManagedPolicy data type object.
listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies :: Lens.Lens' ListManagedPoliciesInPermissionSetResponse (Prelude.Maybe [AttachedManagedPolicy])
listManagedPoliciesInPermissionSetResponse_attachedManagedPolicies = Lens.lens (\ListManagedPoliciesInPermissionSetResponse' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@ListManagedPoliciesInPermissionSetResponse' {} a -> s {attachedManagedPolicies = a} :: ListManagedPoliciesInPermissionSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listManagedPoliciesInPermissionSetResponse_httpStatus :: Lens.Lens' ListManagedPoliciesInPermissionSetResponse Prelude.Int
listManagedPoliciesInPermissionSetResponse_httpStatus = Lens.lens (\ListManagedPoliciesInPermissionSetResponse' {httpStatus} -> httpStatus) (\s@ListManagedPoliciesInPermissionSetResponse' {} a -> s {httpStatus = a} :: ListManagedPoliciesInPermissionSetResponse)

instance
  Prelude.NFData
    ListManagedPoliciesInPermissionSetResponse
  where
  rnf ListManagedPoliciesInPermissionSetResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.SSOAdmin.ListPermissionSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the PermissionSets in an IAM Identity Center instance.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListPermissionSets
  ( -- * Creating a Request
    ListPermissionSets (..),
    newListPermissionSets,

    -- * Request Lenses
    listPermissionSets_nextToken,
    listPermissionSets_maxResults,
    listPermissionSets_instanceArn,

    -- * Destructuring the Response
    ListPermissionSetsResponse (..),
    newListPermissionSetsResponse,

    -- * Response Lenses
    listPermissionSetsResponse_nextToken,
    listPermissionSetsResponse_permissionSets,
    listPermissionSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListPermissionSets' smart constructor.
data ListPermissionSets = ListPermissionSets'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListPermissionSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionSets_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'maxResults', 'listPermissionSets_maxResults' - The maximum number of results to display for the assignment.
--
-- 'instanceArn', 'listPermissionSets_instanceArn' - The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
newListPermissionSets ::
  -- | 'instanceArn'
  Prelude.Text ->
  ListPermissionSets
newListPermissionSets pInstanceArn_ =
  ListPermissionSets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceArn = pInstanceArn_
    }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSets_nextToken :: Lens.Lens' ListPermissionSets (Prelude.Maybe Prelude.Text)
listPermissionSets_nextToken = Lens.lens (\ListPermissionSets' {nextToken} -> nextToken) (\s@ListPermissionSets' {} a -> s {nextToken = a} :: ListPermissionSets)

-- | The maximum number of results to display for the assignment.
listPermissionSets_maxResults :: Lens.Lens' ListPermissionSets (Prelude.Maybe Prelude.Natural)
listPermissionSets_maxResults = Lens.lens (\ListPermissionSets' {maxResults} -> maxResults) (\s@ListPermissionSets' {} a -> s {maxResults = a} :: ListPermissionSets)

-- | The ARN of the IAM Identity Center instance under which the operation
-- will be executed. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
listPermissionSets_instanceArn :: Lens.Lens' ListPermissionSets Prelude.Text
listPermissionSets_instanceArn = Lens.lens (\ListPermissionSets' {instanceArn} -> instanceArn) (\s@ListPermissionSets' {} a -> s {instanceArn = a} :: ListPermissionSets)

instance Core.AWSPager ListPermissionSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPermissionSetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPermissionSetsResponse_permissionSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPermissionSets_nextToken
          Lens..~ rs
          Lens.^? listPermissionSetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPermissionSets where
  type
    AWSResponse ListPermissionSets =
      ListPermissionSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionSetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PermissionSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissionSets where
  hashWithSalt _salt ListPermissionSets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceArn

instance Prelude.NFData ListPermissionSets where
  rnf ListPermissionSets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceArn

instance Data.ToHeaders ListPermissionSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListPermissionSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPermissionSets where
  toJSON ListPermissionSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("InstanceArn" Data..= instanceArn)
          ]
      )

instance Data.ToPath ListPermissionSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPermissionSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionSetsResponse' smart constructor.
data ListPermissionSetsResponse = ListPermissionSetsResponse'
  { -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Defines the level of access on an AWS account.
    permissionSets :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionSetsResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'permissionSets', 'listPermissionSetsResponse_permissionSets' - Defines the level of access on an AWS account.
--
-- 'httpStatus', 'listPermissionSetsResponse_httpStatus' - The response's http status code.
newListPermissionSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionSetsResponse
newListPermissionSetsResponse pHttpStatus_ =
  ListPermissionSetsResponse'
    { nextToken =
        Prelude.Nothing,
      permissionSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listPermissionSetsResponse_nextToken :: Lens.Lens' ListPermissionSetsResponse (Prelude.Maybe Prelude.Text)
listPermissionSetsResponse_nextToken = Lens.lens (\ListPermissionSetsResponse' {nextToken} -> nextToken) (\s@ListPermissionSetsResponse' {} a -> s {nextToken = a} :: ListPermissionSetsResponse)

-- | Defines the level of access on an AWS account.
listPermissionSetsResponse_permissionSets :: Lens.Lens' ListPermissionSetsResponse (Prelude.Maybe [Prelude.Text])
listPermissionSetsResponse_permissionSets = Lens.lens (\ListPermissionSetsResponse' {permissionSets} -> permissionSets) (\s@ListPermissionSetsResponse' {} a -> s {permissionSets = a} :: ListPermissionSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionSetsResponse_httpStatus :: Lens.Lens' ListPermissionSetsResponse Prelude.Int
listPermissionSetsResponse_httpStatus = Lens.lens (\ListPermissionSetsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionSetsResponse' {} a -> s {httpStatus = a} :: ListPermissionSetsResponse)

instance Prelude.NFData ListPermissionSetsResponse where
  rnf ListPermissionSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionSets
      `Prelude.seq` Prelude.rnf httpStatus

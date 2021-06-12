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
-- Module      : Network.AWS.Connect.ListLambdaFunctions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all Lambda functions that display in the
-- dropdown options in the relevant contact flow blocks.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLambdaFunctions
  ( -- * Creating a Request
    ListLambdaFunctions (..),
    newListLambdaFunctions,

    -- * Request Lenses
    listLambdaFunctions_nextToken,
    listLambdaFunctions_maxResults,
    listLambdaFunctions_instanceId,

    -- * Destructuring the Response
    ListLambdaFunctionsResponse (..),
    newListLambdaFunctionsResponse,

    -- * Response Lenses
    listLambdaFunctionsResponse_nextToken,
    listLambdaFunctionsResponse_lambdaFunctions,
    listLambdaFunctionsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLambdaFunctions' smart constructor.
data ListLambdaFunctions = ListLambdaFunctions'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLambdaFunctions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLambdaFunctions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listLambdaFunctions_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listLambdaFunctions_instanceId' - The identifier of the Amazon Connect instance.
newListLambdaFunctions ::
  -- | 'instanceId'
  Core.Text ->
  ListLambdaFunctions
newListLambdaFunctions pInstanceId_ =
  ListLambdaFunctions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listLambdaFunctions_nextToken :: Lens.Lens' ListLambdaFunctions (Core.Maybe Core.Text)
listLambdaFunctions_nextToken = Lens.lens (\ListLambdaFunctions' {nextToken} -> nextToken) (\s@ListLambdaFunctions' {} a -> s {nextToken = a} :: ListLambdaFunctions)

-- | The maximum number of results to return per page.
listLambdaFunctions_maxResults :: Lens.Lens' ListLambdaFunctions (Core.Maybe Core.Natural)
listLambdaFunctions_maxResults = Lens.lens (\ListLambdaFunctions' {maxResults} -> maxResults) (\s@ListLambdaFunctions' {} a -> s {maxResults = a} :: ListLambdaFunctions)

-- | The identifier of the Amazon Connect instance.
listLambdaFunctions_instanceId :: Lens.Lens' ListLambdaFunctions Core.Text
listLambdaFunctions_instanceId = Lens.lens (\ListLambdaFunctions' {instanceId} -> instanceId) (\s@ListLambdaFunctions' {} a -> s {instanceId = a} :: ListLambdaFunctions)

instance Core.AWSPager ListLambdaFunctions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLambdaFunctionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listLambdaFunctionsResponse_lambdaFunctions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLambdaFunctions_nextToken
          Lens..~ rs
          Lens.^? listLambdaFunctionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListLambdaFunctions where
  type
    AWSResponse ListLambdaFunctions =
      ListLambdaFunctionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLambdaFunctionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "LambdaFunctions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLambdaFunctions

instance Core.NFData ListLambdaFunctions

instance Core.ToHeaders ListLambdaFunctions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListLambdaFunctions where
  toPath ListLambdaFunctions' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/lambda-functions"
      ]

instance Core.ToQuery ListLambdaFunctions where
  toQuery ListLambdaFunctions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLambdaFunctionsResponse' smart constructor.
data ListLambdaFunctionsResponse = ListLambdaFunctionsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The Lambdafunction ARNs associated with the specified instance.
    lambdaFunctions :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListLambdaFunctionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLambdaFunctionsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'lambdaFunctions', 'listLambdaFunctionsResponse_lambdaFunctions' - The Lambdafunction ARNs associated with the specified instance.
--
-- 'httpStatus', 'listLambdaFunctionsResponse_httpStatus' - The response's http status code.
newListLambdaFunctionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListLambdaFunctionsResponse
newListLambdaFunctionsResponse pHttpStatus_ =
  ListLambdaFunctionsResponse'
    { nextToken =
        Core.Nothing,
      lambdaFunctions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listLambdaFunctionsResponse_nextToken :: Lens.Lens' ListLambdaFunctionsResponse (Core.Maybe Core.Text)
listLambdaFunctionsResponse_nextToken = Lens.lens (\ListLambdaFunctionsResponse' {nextToken} -> nextToken) (\s@ListLambdaFunctionsResponse' {} a -> s {nextToken = a} :: ListLambdaFunctionsResponse)

-- | The Lambdafunction ARNs associated with the specified instance.
listLambdaFunctionsResponse_lambdaFunctions :: Lens.Lens' ListLambdaFunctionsResponse (Core.Maybe [Core.Text])
listLambdaFunctionsResponse_lambdaFunctions = Lens.lens (\ListLambdaFunctionsResponse' {lambdaFunctions} -> lambdaFunctions) (\s@ListLambdaFunctionsResponse' {} a -> s {lambdaFunctions = a} :: ListLambdaFunctionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLambdaFunctionsResponse_httpStatus :: Lens.Lens' ListLambdaFunctionsResponse Core.Int
listLambdaFunctionsResponse_httpStatus = Lens.lens (\ListLambdaFunctionsResponse' {httpStatus} -> httpStatus) (\s@ListLambdaFunctionsResponse' {} a -> s {httpStatus = a} :: ListLambdaFunctionsResponse)

instance Core.NFData ListLambdaFunctionsResponse

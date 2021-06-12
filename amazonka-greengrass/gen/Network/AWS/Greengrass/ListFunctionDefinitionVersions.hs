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
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitionVersions
  ( -- * Creating a Request
    ListFunctionDefinitionVersions (..),
    newListFunctionDefinitionVersions,

    -- * Request Lenses
    listFunctionDefinitionVersions_nextToken,
    listFunctionDefinitionVersions_maxResults,
    listFunctionDefinitionVersions_functionDefinitionId,

    -- * Destructuring the Response
    ListFunctionDefinitionVersionsResponse (..),
    newListFunctionDefinitionVersionsResponse,

    -- * Response Lenses
    listFunctionDefinitionVersionsResponse_nextToken,
    listFunctionDefinitionVersionsResponse_versions,
    listFunctionDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFunctionDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctionDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listFunctionDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'functionDefinitionId', 'listFunctionDefinitionVersions_functionDefinitionId' - The ID of the Lambda function definition.
newListFunctionDefinitionVersions ::
  -- | 'functionDefinitionId'
  Core.Text ->
  ListFunctionDefinitionVersions
newListFunctionDefinitionVersions
  pFunctionDefinitionId_ =
    ListFunctionDefinitionVersions'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        functionDefinitionId =
          pFunctionDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionVersions_nextToken :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
listFunctionDefinitionVersions_nextToken = Lens.lens (\ListFunctionDefinitionVersions' {nextToken} -> nextToken) (\s@ListFunctionDefinitionVersions' {} a -> s {nextToken = a} :: ListFunctionDefinitionVersions)

-- | The maximum number of results to be returned per request.
listFunctionDefinitionVersions_maxResults :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
listFunctionDefinitionVersions_maxResults = Lens.lens (\ListFunctionDefinitionVersions' {maxResults} -> maxResults) (\s@ListFunctionDefinitionVersions' {} a -> s {maxResults = a} :: ListFunctionDefinitionVersions)

-- | The ID of the Lambda function definition.
listFunctionDefinitionVersions_functionDefinitionId :: Lens.Lens' ListFunctionDefinitionVersions Core.Text
listFunctionDefinitionVersions_functionDefinitionId = Lens.lens (\ListFunctionDefinitionVersions' {functionDefinitionId} -> functionDefinitionId) (\s@ListFunctionDefinitionVersions' {} a -> s {functionDefinitionId = a} :: ListFunctionDefinitionVersions)

instance Core.AWSPager ListFunctionDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listFunctionDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFunctionDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listFunctionDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListFunctionDefinitionVersions
  where
  type
    AWSResponse ListFunctionDefinitionVersions =
      ListFunctionDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListFunctionDefinitionVersions

instance Core.NFData ListFunctionDefinitionVersions

instance
  Core.ToHeaders
    ListFunctionDefinitionVersions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListFunctionDefinitionVersions where
  toPath ListFunctionDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/functions/",
        Core.toBS functionDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListFunctionDefinitionVersions where
  toQuery ListFunctionDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFunctionDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFunctionDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listFunctionDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listFunctionDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListFunctionDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFunctionDefinitionVersionsResponse
newListFunctionDefinitionVersionsResponse
  pHttpStatus_ =
    ListFunctionDefinitionVersionsResponse'
      { nextToken =
          Core.Nothing,
        versions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listFunctionDefinitionVersionsResponse_nextToken :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe Core.Text)
listFunctionDefinitionVersionsResponse_nextToken = Lens.lens (\ListFunctionDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListFunctionDefinitionVersionsResponse)

-- | Information about a version.
listFunctionDefinitionVersionsResponse_versions :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listFunctionDefinitionVersionsResponse_versions = Lens.lens (\ListFunctionDefinitionVersionsResponse' {versions} -> versions) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {versions = a} :: ListFunctionDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listFunctionDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListFunctionDefinitionVersionsResponse Core.Int
listFunctionDefinitionVersionsResponse_httpStatus = Lens.lens (\ListFunctionDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListFunctionDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListFunctionDefinitionVersionsResponse)

instance
  Core.NFData
    ListFunctionDefinitionVersionsResponse

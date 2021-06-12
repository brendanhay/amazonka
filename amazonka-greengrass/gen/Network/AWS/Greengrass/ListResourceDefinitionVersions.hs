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
-- Module      : Network.AWS.Greengrass.ListResourceDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a resource definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitionVersions
  ( -- * Creating a Request
    ListResourceDefinitionVersions (..),
    newListResourceDefinitionVersions,

    -- * Request Lenses
    listResourceDefinitionVersions_nextToken,
    listResourceDefinitionVersions_maxResults,
    listResourceDefinitionVersions_resourceDefinitionId,

    -- * Destructuring the Response
    ListResourceDefinitionVersionsResponse (..),
    newListResourceDefinitionVersionsResponse,

    -- * Response Lenses
    listResourceDefinitionVersionsResponse_nextToken,
    listResourceDefinitionVersionsResponse_versions,
    listResourceDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResourceDefinitionVersions' smart constructor.
data ListResourceDefinitionVersions = ListResourceDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listResourceDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'resourceDefinitionId', 'listResourceDefinitionVersions_resourceDefinitionId' - The ID of the resource definition.
newListResourceDefinitionVersions ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  ListResourceDefinitionVersions
newListResourceDefinitionVersions
  pResourceDefinitionId_ =
    ListResourceDefinitionVersions'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        resourceDefinitionId =
          pResourceDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionVersions_nextToken :: Lens.Lens' ListResourceDefinitionVersions (Core.Maybe Core.Text)
listResourceDefinitionVersions_nextToken = Lens.lens (\ListResourceDefinitionVersions' {nextToken} -> nextToken) (\s@ListResourceDefinitionVersions' {} a -> s {nextToken = a} :: ListResourceDefinitionVersions)

-- | The maximum number of results to be returned per request.
listResourceDefinitionVersions_maxResults :: Lens.Lens' ListResourceDefinitionVersions (Core.Maybe Core.Text)
listResourceDefinitionVersions_maxResults = Lens.lens (\ListResourceDefinitionVersions' {maxResults} -> maxResults) (\s@ListResourceDefinitionVersions' {} a -> s {maxResults = a} :: ListResourceDefinitionVersions)

-- | The ID of the resource definition.
listResourceDefinitionVersions_resourceDefinitionId :: Lens.Lens' ListResourceDefinitionVersions Core.Text
listResourceDefinitionVersions_resourceDefinitionId = Lens.lens (\ListResourceDefinitionVersions' {resourceDefinitionId} -> resourceDefinitionId) (\s@ListResourceDefinitionVersions' {} a -> s {resourceDefinitionId = a} :: ListResourceDefinitionVersions)

instance Core.AWSPager ListResourceDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourceDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listResourceDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListResourceDefinitionVersions
  where
  type
    AWSResponse ListResourceDefinitionVersions =
      ListResourceDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourceDefinitionVersions

instance Core.NFData ListResourceDefinitionVersions

instance
  Core.ToHeaders
    ListResourceDefinitionVersions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListResourceDefinitionVersions where
  toPath ListResourceDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListResourceDefinitionVersions where
  toQuery ListResourceDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResourceDefinitionVersionsResponse' smart constructor.
data ListResourceDefinitionVersionsResponse = ListResourceDefinitionVersionsResponse'
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
-- Create a value of 'ListResourceDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listResourceDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listResourceDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListResourceDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourceDefinitionVersionsResponse
newListResourceDefinitionVersionsResponse
  pHttpStatus_ =
    ListResourceDefinitionVersionsResponse'
      { nextToken =
          Core.Nothing,
        versions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listResourceDefinitionVersionsResponse_nextToken :: Lens.Lens' ListResourceDefinitionVersionsResponse (Core.Maybe Core.Text)
listResourceDefinitionVersionsResponse_nextToken = Lens.lens (\ListResourceDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListResourceDefinitionVersionsResponse)

-- | Information about a version.
listResourceDefinitionVersionsResponse_versions :: Lens.Lens' ListResourceDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listResourceDefinitionVersionsResponse_versions = Lens.lens (\ListResourceDefinitionVersionsResponse' {versions} -> versions) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {versions = a} :: ListResourceDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListResourceDefinitionVersionsResponse Core.Int
listResourceDefinitionVersionsResponse_httpStatus = Lens.lens (\ListResourceDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListResourceDefinitionVersionsResponse)

instance
  Core.NFData
    ListResourceDefinitionVersionsResponse

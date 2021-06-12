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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a core definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitionVersions
  ( -- * Creating a Request
    ListCoreDefinitionVersions (..),
    newListCoreDefinitionVersions,

    -- * Request Lenses
    listCoreDefinitionVersions_nextToken,
    listCoreDefinitionVersions_maxResults,
    listCoreDefinitionVersions_coreDefinitionId,

    -- * Destructuring the Response
    ListCoreDefinitionVersionsResponse (..),
    newListCoreDefinitionVersionsResponse,

    -- * Response Lenses
    listCoreDefinitionVersionsResponse_nextToken,
    listCoreDefinitionVersionsResponse_versions,
    listCoreDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the core definition.
    coreDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCoreDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listCoreDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'coreDefinitionId', 'listCoreDefinitionVersions_coreDefinitionId' - The ID of the core definition.
newListCoreDefinitionVersions ::
  -- | 'coreDefinitionId'
  Core.Text ->
  ListCoreDefinitionVersions
newListCoreDefinitionVersions pCoreDefinitionId_ =
  ListCoreDefinitionVersions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersions_nextToken :: Lens.Lens' ListCoreDefinitionVersions (Core.Maybe Core.Text)
listCoreDefinitionVersions_nextToken = Lens.lens (\ListCoreDefinitionVersions' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersions' {} a -> s {nextToken = a} :: ListCoreDefinitionVersions)

-- | The maximum number of results to be returned per request.
listCoreDefinitionVersions_maxResults :: Lens.Lens' ListCoreDefinitionVersions (Core.Maybe Core.Text)
listCoreDefinitionVersions_maxResults = Lens.lens (\ListCoreDefinitionVersions' {maxResults} -> maxResults) (\s@ListCoreDefinitionVersions' {} a -> s {maxResults = a} :: ListCoreDefinitionVersions)

-- | The ID of the core definition.
listCoreDefinitionVersions_coreDefinitionId :: Lens.Lens' ListCoreDefinitionVersions Core.Text
listCoreDefinitionVersions_coreDefinitionId = Lens.lens (\ListCoreDefinitionVersions' {coreDefinitionId} -> coreDefinitionId) (\s@ListCoreDefinitionVersions' {} a -> s {coreDefinitionId = a} :: ListCoreDefinitionVersions)

instance Core.AWSPager ListCoreDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCoreDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listCoreDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCoreDefinitionVersions where
  type
    AWSResponse ListCoreDefinitionVersions =
      ListCoreDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCoreDefinitionVersions

instance Core.NFData ListCoreDefinitionVersions

instance Core.ToHeaders ListCoreDefinitionVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListCoreDefinitionVersions where
  toPath ListCoreDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListCoreDefinitionVersions where
  toQuery ListCoreDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
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
-- Create a value of 'ListCoreDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listCoreDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listCoreDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListCoreDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCoreDefinitionVersionsResponse
newListCoreDefinitionVersionsResponse pHttpStatus_ =
  ListCoreDefinitionVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listCoreDefinitionVersionsResponse_nextToken :: Lens.Lens' ListCoreDefinitionVersionsResponse (Core.Maybe Core.Text)
listCoreDefinitionVersionsResponse_nextToken = Lens.lens (\ListCoreDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListCoreDefinitionVersionsResponse)

-- | Information about a version.
listCoreDefinitionVersionsResponse_versions :: Lens.Lens' ListCoreDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listCoreDefinitionVersionsResponse_versions = Lens.lens (\ListCoreDefinitionVersionsResponse' {versions} -> versions) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {versions = a} :: ListCoreDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCoreDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListCoreDefinitionVersionsResponse Core.Int
listCoreDefinitionVersionsResponse_httpStatus = Lens.lens (\ListCoreDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListCoreDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListCoreDefinitionVersionsResponse)

instance
  Core.NFData
    ListCoreDefinitionVersionsResponse

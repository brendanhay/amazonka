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
-- Module      : Network.AWS.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListGroupVersions
  ( -- * Creating a Request
    ListGroupVersions (..),
    newListGroupVersions,

    -- * Request Lenses
    listGroupVersions_nextToken,
    listGroupVersions_maxResults,
    listGroupVersions_groupId,

    -- * Destructuring the Response
    ListGroupVersionsResponse (..),
    newListGroupVersionsResponse,

    -- * Response Lenses
    listGroupVersionsResponse_nextToken,
    listGroupVersionsResponse_versions,
    listGroupVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroupVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listGroupVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'groupId', 'listGroupVersions_groupId' - The ID of the Greengrass group.
newListGroupVersions ::
  -- | 'groupId'
  Core.Text ->
  ListGroupVersions
newListGroupVersions pGroupId_ =
  ListGroupVersions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      groupId = pGroupId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listGroupVersions_nextToken :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
listGroupVersions_nextToken = Lens.lens (\ListGroupVersions' {nextToken} -> nextToken) (\s@ListGroupVersions' {} a -> s {nextToken = a} :: ListGroupVersions)

-- | The maximum number of results to be returned per request.
listGroupVersions_maxResults :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
listGroupVersions_maxResults = Lens.lens (\ListGroupVersions' {maxResults} -> maxResults) (\s@ListGroupVersions' {} a -> s {maxResults = a} :: ListGroupVersions)

-- | The ID of the Greengrass group.
listGroupVersions_groupId :: Lens.Lens' ListGroupVersions Core.Text
listGroupVersions_groupId = Lens.lens (\ListGroupVersions' {groupId} -> groupId) (\s@ListGroupVersions' {} a -> s {groupId = a} :: ListGroupVersions)

instance Core.AWSPager ListGroupVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupVersionsResponse_versions Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGroupVersions_nextToken
          Lens..~ rs
          Lens.^? listGroupVersionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListGroupVersions where
  type
    AWSResponse ListGroupVersions =
      ListGroupVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGroupVersions

instance Core.NFData ListGroupVersions

instance Core.ToHeaders ListGroupVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListGroupVersions where
  toPath ListGroupVersions' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/versions"
      ]

instance Core.ToQuery ListGroupVersions where
  toQuery ListGroupVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
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
-- Create a value of 'ListGroupVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listGroupVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listGroupVersionsResponse_httpStatus' - The response's http status code.
newListGroupVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGroupVersionsResponse
newListGroupVersionsResponse pHttpStatus_ =
  ListGroupVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listGroupVersionsResponse_nextToken :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe Core.Text)
listGroupVersionsResponse_nextToken = Lens.lens (\ListGroupVersionsResponse' {nextToken} -> nextToken) (\s@ListGroupVersionsResponse' {} a -> s {nextToken = a} :: ListGroupVersionsResponse)

-- | Information about a version.
listGroupVersionsResponse_versions :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe [VersionInformation])
listGroupVersionsResponse_versions = Lens.lens (\ListGroupVersionsResponse' {versions} -> versions) (\s@ListGroupVersionsResponse' {} a -> s {versions = a} :: ListGroupVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGroupVersionsResponse_httpStatus :: Lens.Lens' ListGroupVersionsResponse Core.Int
listGroupVersionsResponse_httpStatus = Lens.lens (\ListGroupVersionsResponse' {httpStatus} -> httpStatus) (\s@ListGroupVersionsResponse' {} a -> s {httpStatus = a} :: ListGroupVersionsResponse)

instance Core.NFData ListGroupVersionsResponse

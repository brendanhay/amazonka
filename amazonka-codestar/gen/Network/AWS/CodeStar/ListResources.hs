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
-- Module      : Network.AWS.CodeStar.ListResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists resources associated with a project in AWS CodeStar.
--
-- This operation returns paginated results.
module Network.AWS.CodeStar.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_nextToken,
    listResources_maxResults,
    listResources_projectId,

    -- * Destructuring the Response
    ListResourcesResponse (..),
    newListResourcesResponse,

    -- * Response Lenses
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
  { -- | The continuation token for the next set of results, if the results
    -- cannot be returned in one response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum amount of data that can be contained in a single set of
    -- results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the project.
    projectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResources_nextToken' - The continuation token for the next set of results, if the results
-- cannot be returned in one response.
--
-- 'maxResults', 'listResources_maxResults' - The maximum amount of data that can be contained in a single set of
-- results.
--
-- 'projectId', 'listResources_projectId' - The ID of the project.
newListResources ::
  -- | 'projectId'
  Core.Text ->
  ListResources
newListResources pProjectId_ =
  ListResources'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      projectId = pProjectId_
    }

-- | The continuation token for the next set of results, if the results
-- cannot be returned in one response.
listResources_nextToken :: Lens.Lens' ListResources (Core.Maybe Core.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | The maximum amount of data that can be contained in a single set of
-- results.
listResources_maxResults :: Lens.Lens' ListResources (Core.Maybe Core.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

-- | The ID of the project.
listResources_projectId :: Lens.Lens' ListResources Core.Text
listResources_projectId = Lens.lens (\ListResources' {projectId} -> projectId) (\s@ListResources' {} a -> s {projectId = a} :: ListResources)

instance Core.AWSPager ListResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_resources Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResources_nextToken
          Lens..~ rs
          Lens.^? listResourcesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "resources" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResources

instance Core.NFData ListResources

instance Core.ToHeaders ListResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.ListResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResources where
  toJSON ListResources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just ("projectId" Core..= projectId)
          ]
      )

instance Core.ToPath ListResources where
  toPath = Core.const "/"

instance Core.ToQuery ListResources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | The continuation token to use when requesting the next set of results,
    -- if there are more results to be returned.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of resources associated with the project.
    resources :: Core.Maybe [Resource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcesResponse_nextToken' - The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
--
-- 'resources', 'listResourcesResponse_resources' - An array of resources associated with the project.
--
-- 'httpStatus', 'listResourcesResponse_httpStatus' - The response's http status code.
newListResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourcesResponse
newListResourcesResponse pHttpStatus_ =
  ListResourcesResponse'
    { nextToken = Core.Nothing,
      resources = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The continuation token to use when requesting the next set of results,
-- if there are more results to be returned.
listResourcesResponse_nextToken :: Lens.Lens' ListResourcesResponse (Core.Maybe Core.Text)
listResourcesResponse_nextToken = Lens.lens (\ListResourcesResponse' {nextToken} -> nextToken) (\s@ListResourcesResponse' {} a -> s {nextToken = a} :: ListResourcesResponse)

-- | An array of resources associated with the project.
listResourcesResponse_resources :: Lens.Lens' ListResourcesResponse (Core.Maybe [Resource])
listResourcesResponse_resources = Lens.lens (\ListResourcesResponse' {resources} -> resources) (\s@ListResourcesResponse' {} a -> s {resources = a} :: ListResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Core.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Core.NFData ListResourcesResponse

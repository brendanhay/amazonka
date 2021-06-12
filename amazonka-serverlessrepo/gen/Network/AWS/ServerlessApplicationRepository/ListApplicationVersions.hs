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
-- Module      : Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions for the specified application.
--
-- This operation returns paginated results.
module Network.AWS.ServerlessApplicationRepository.ListApplicationVersions
  ( -- * Creating a Request
    ListApplicationVersions (..),
    newListApplicationVersions,

    -- * Request Lenses
    listApplicationVersions_nextToken,
    listApplicationVersions_maxItems,
    listApplicationVersions_applicationId,

    -- * Destructuring the Response
    ListApplicationVersionsResponse (..),
    newListApplicationVersionsResponse,

    -- * Response Lenses
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_versions,
    listApplicationVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { -- | A token to specify where to start paginating.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return.
    maxItems :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationVersions_nextToken' - A token to specify where to start paginating.
--
-- 'maxItems', 'listApplicationVersions_maxItems' - The total number of items to return.
--
-- 'applicationId', 'listApplicationVersions_applicationId' - The Amazon Resource Name (ARN) of the application.
newListApplicationVersions ::
  -- | 'applicationId'
  Core.Text ->
  ListApplicationVersions
newListApplicationVersions pApplicationId_ =
  ListApplicationVersions'
    { nextToken = Core.Nothing,
      maxItems = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | A token to specify where to start paginating.
listApplicationVersions_nextToken :: Lens.Lens' ListApplicationVersions (Core.Maybe Core.Text)
listApplicationVersions_nextToken = Lens.lens (\ListApplicationVersions' {nextToken} -> nextToken) (\s@ListApplicationVersions' {} a -> s {nextToken = a} :: ListApplicationVersions)

-- | The total number of items to return.
listApplicationVersions_maxItems :: Lens.Lens' ListApplicationVersions (Core.Maybe Core.Natural)
listApplicationVersions_maxItems = Lens.lens (\ListApplicationVersions' {maxItems} -> maxItems) (\s@ListApplicationVersions' {} a -> s {maxItems = a} :: ListApplicationVersions)

-- | The Amazon Resource Name (ARN) of the application.
listApplicationVersions_applicationId :: Lens.Lens' ListApplicationVersions Core.Text
listApplicationVersions_applicationId = Lens.lens (\ListApplicationVersions' {applicationId} -> applicationId) (\s@ListApplicationVersions' {} a -> s {applicationId = a} :: ListApplicationVersions)

instance Core.AWSPager ListApplicationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApplicationVersions_nextToken
          Lens..~ rs
          Lens.^? listApplicationVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListApplicationVersions where
  type
    AWSResponse ListApplicationVersions =
      ListApplicationVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationVersionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApplicationVersions

instance Core.NFData ListApplicationVersions

instance Core.ToHeaders ListApplicationVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListApplicationVersions where
  toPath ListApplicationVersions' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/versions"
      ]

instance Core.ToQuery ListApplicationVersions where
  toQuery ListApplicationVersions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxItems" Core.=: maxItems
      ]

-- | /See:/ 'newListApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of version summaries for the application.
    versions :: Core.Maybe [VersionSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApplicationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationVersionsResponse_nextToken' - The token to request the next page of results.
--
-- 'versions', 'listApplicationVersionsResponse_versions' - An array of version summaries for the application.
--
-- 'httpStatus', 'listApplicationVersionsResponse_httpStatus' - The response's http status code.
newListApplicationVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApplicationVersionsResponse
newListApplicationVersionsResponse pHttpStatus_ =
  ListApplicationVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationVersionsResponse_nextToken :: Lens.Lens' ListApplicationVersionsResponse (Core.Maybe Core.Text)
listApplicationVersionsResponse_nextToken = Lens.lens (\ListApplicationVersionsResponse' {nextToken} -> nextToken) (\s@ListApplicationVersionsResponse' {} a -> s {nextToken = a} :: ListApplicationVersionsResponse)

-- | An array of version summaries for the application.
listApplicationVersionsResponse_versions :: Lens.Lens' ListApplicationVersionsResponse (Core.Maybe [VersionSummary])
listApplicationVersionsResponse_versions = Lens.lens (\ListApplicationVersionsResponse' {versions} -> versions) (\s@ListApplicationVersionsResponse' {} a -> s {versions = a} :: ListApplicationVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listApplicationVersionsResponse_httpStatus :: Lens.Lens' ListApplicationVersionsResponse Core.Int
listApplicationVersionsResponse_httpStatus = Lens.lens (\ListApplicationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationVersionsResponse' {} a -> s {httpStatus = a} :: ListApplicationVersionsResponse)

instance Core.NFData ListApplicationVersionsResponse

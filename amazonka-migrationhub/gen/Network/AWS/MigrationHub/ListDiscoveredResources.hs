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
-- Module      : Network.AWS.MigrationHub.ListDiscoveredResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists discovered resources associated with the given @MigrationTask@.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListDiscoveredResources
  ( -- * Creating a Request
    ListDiscoveredResources (..),
    newListDiscoveredResources,

    -- * Request Lenses
    listDiscoveredResources_nextToken,
    listDiscoveredResources_maxResults,
    listDiscoveredResources_progressUpdateStream,
    listDiscoveredResources_migrationTaskName,

    -- * Destructuring the Response
    ListDiscoveredResourcesResponse (..),
    newListDiscoveredResourcesResponse,

    -- * Response Lenses
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_discoveredResourceList,
    listDiscoveredResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results returned per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Core.Text,
    -- | The name of the MigrationTask. /Do not store personal data in this
    -- field./
    migrationTaskName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDiscoveredResources_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'maxResults', 'listDiscoveredResources_maxResults' - The maximum number of results returned per page.
--
-- 'progressUpdateStream', 'listDiscoveredResources_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'listDiscoveredResources_migrationTaskName' - The name of the MigrationTask. /Do not store personal data in this
-- field./
newListDiscoveredResources ::
  -- | 'progressUpdateStream'
  Core.Text ->
  -- | 'migrationTaskName'
  Core.Text ->
  ListDiscoveredResources
newListDiscoveredResources
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListDiscoveredResources'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The maximum number of results returned per page.
listDiscoveredResources_maxResults :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Natural)
listDiscoveredResources_maxResults = Lens.lens (\ListDiscoveredResources' {maxResults} -> maxResults) (\s@ListDiscoveredResources' {} a -> s {maxResults = a} :: ListDiscoveredResources)

-- | The name of the ProgressUpdateStream.
listDiscoveredResources_progressUpdateStream :: Lens.Lens' ListDiscoveredResources Core.Text
listDiscoveredResources_progressUpdateStream = Lens.lens (\ListDiscoveredResources' {progressUpdateStream} -> progressUpdateStream) (\s@ListDiscoveredResources' {} a -> s {progressUpdateStream = a} :: ListDiscoveredResources)

-- | The name of the MigrationTask. /Do not store personal data in this
-- field./
listDiscoveredResources_migrationTaskName :: Lens.Lens' ListDiscoveredResources Core.Text
listDiscoveredResources_migrationTaskName = Lens.lens (\ListDiscoveredResources' {migrationTaskName} -> migrationTaskName) (\s@ListDiscoveredResources' {} a -> s {migrationTaskName = a} :: ListDiscoveredResources)

instance Core.AWSPager ListDiscoveredResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_discoveredResourceList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listDiscoveredResourcesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDiscoveredResources where
  type
    AWSResponse ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "DiscoveredResourceList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDiscoveredResources

instance Core.NFData ListDiscoveredResources

instance Core.ToHeaders ListDiscoveredResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListDiscoveredResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Core.Just
              ("MigrationTaskName" Core..= migrationTaskName)
          ]
      )

instance Core.ToPath ListDiscoveredResources where
  toPath = Core.const "/"

instance Core.ToQuery ListDiscoveredResources where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | If there are more discovered resources than the max result, return the
    -- next token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Core.Maybe Core.Text,
    -- | Returned list of discovered resources associated with the given
    -- MigrationTask.
    discoveredResourceList :: Core.Maybe [DiscoveredResource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDiscoveredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDiscoveredResourcesResponse_nextToken' - If there are more discovered resources than the max result, return the
-- next token to be passed to the next call as a bookmark of where to start
-- from.
--
-- 'discoveredResourceList', 'listDiscoveredResourcesResponse_discoveredResourceList' - Returned list of discovered resources associated with the given
-- MigrationTask.
--
-- 'httpStatus', 'listDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { nextToken =
        Core.Nothing,
      discoveredResourceList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more discovered resources than the max result, return the
-- next token to be passed to the next call as a bookmark of where to start
-- from.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe Core.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | Returned list of discovered resources associated with the given
-- MigrationTask.
listDiscoveredResourcesResponse_discoveredResourceList :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe [DiscoveredResource])
listDiscoveredResourcesResponse_discoveredResourceList = Lens.lens (\ListDiscoveredResourcesResponse' {discoveredResourceList} -> discoveredResourceList) (\s@ListDiscoveredResourcesResponse' {} a -> s {discoveredResourceList = a} :: ListDiscoveredResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Core.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance Core.NFData ListDiscoveredResourcesResponse

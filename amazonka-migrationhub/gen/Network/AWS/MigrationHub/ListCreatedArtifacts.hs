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
-- Module      : Network.AWS.MigrationHub.ListCreatedArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the created artifacts attached to a given migration task in an
-- update stream. This API has the following traits:
--
-- -   Gets the list of the created artifacts while migration is taking
--     place.
--
-- -   Shows the artifacts created by the migration tool that was
--     associated by the @AssociateCreatedArtifact@ API.
--
-- -   Lists created artifacts in a paginated interface.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListCreatedArtifacts
  ( -- * Creating a Request
    ListCreatedArtifacts (..),
    newListCreatedArtifacts,

    -- * Request Lenses
    listCreatedArtifacts_nextToken,
    listCreatedArtifacts_maxResults,
    listCreatedArtifacts_progressUpdateStream,
    listCreatedArtifacts_migrationTaskName,

    -- * Destructuring the Response
    ListCreatedArtifactsResponse (..),
    newListCreatedArtifactsResponse,

    -- * Response Lenses
    listCreatedArtifactsResponse_nextToken,
    listCreatedArtifactsResponse_createdArtifactList,
    listCreatedArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCreatedArtifacts' smart constructor.
data ListCreatedArtifacts = ListCreatedArtifacts'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of results to be returned per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Core.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCreatedArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCreatedArtifacts_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'maxResults', 'listCreatedArtifacts_maxResults' - Maximum number of results to be returned per page.
--
-- 'progressUpdateStream', 'listCreatedArtifacts_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'listCreatedArtifacts_migrationTaskName' - Unique identifier that references the migration task. /Do not store
-- personal data in this field./
newListCreatedArtifacts ::
  -- | 'progressUpdateStream'
  Core.Text ->
  -- | 'migrationTaskName'
  Core.Text ->
  ListCreatedArtifacts
newListCreatedArtifacts
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListCreatedArtifacts'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listCreatedArtifacts_nextToken :: Lens.Lens' ListCreatedArtifacts (Core.Maybe Core.Text)
listCreatedArtifacts_nextToken = Lens.lens (\ListCreatedArtifacts' {nextToken} -> nextToken) (\s@ListCreatedArtifacts' {} a -> s {nextToken = a} :: ListCreatedArtifacts)

-- | Maximum number of results to be returned per page.
listCreatedArtifacts_maxResults :: Lens.Lens' ListCreatedArtifacts (Core.Maybe Core.Natural)
listCreatedArtifacts_maxResults = Lens.lens (\ListCreatedArtifacts' {maxResults} -> maxResults) (\s@ListCreatedArtifacts' {} a -> s {maxResults = a} :: ListCreatedArtifacts)

-- | The name of the ProgressUpdateStream.
listCreatedArtifacts_progressUpdateStream :: Lens.Lens' ListCreatedArtifacts Core.Text
listCreatedArtifacts_progressUpdateStream = Lens.lens (\ListCreatedArtifacts' {progressUpdateStream} -> progressUpdateStream) (\s@ListCreatedArtifacts' {} a -> s {progressUpdateStream = a} :: ListCreatedArtifacts)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
listCreatedArtifacts_migrationTaskName :: Lens.Lens' ListCreatedArtifacts Core.Text
listCreatedArtifacts_migrationTaskName = Lens.lens (\ListCreatedArtifacts' {migrationTaskName} -> migrationTaskName) (\s@ListCreatedArtifacts' {} a -> s {migrationTaskName = a} :: ListCreatedArtifacts)

instance Core.AWSPager ListCreatedArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCreatedArtifactsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listCreatedArtifactsResponse_createdArtifactList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCreatedArtifacts_nextToken
          Lens..~ rs
          Lens.^? listCreatedArtifactsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCreatedArtifacts where
  type
    AWSResponse ListCreatedArtifacts =
      ListCreatedArtifactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCreatedArtifactsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "CreatedArtifactList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCreatedArtifacts

instance Core.NFData ListCreatedArtifacts

instance Core.ToHeaders ListCreatedArtifacts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListCreatedArtifacts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCreatedArtifacts where
  toJSON ListCreatedArtifacts' {..} =
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

instance Core.ToPath ListCreatedArtifacts where
  toPath = Core.const "/"

instance Core.ToQuery ListCreatedArtifacts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCreatedArtifactsResponse' smart constructor.
data ListCreatedArtifactsResponse = ListCreatedArtifactsResponse'
  { -- | If there are more created artifacts than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Core.Maybe Core.Text,
    -- | List of created artifacts up to the maximum number of results specified
    -- in the request.
    createdArtifactList :: Core.Maybe [CreatedArtifact],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCreatedArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCreatedArtifactsResponse_nextToken' - If there are more created artifacts than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
--
-- 'createdArtifactList', 'listCreatedArtifactsResponse_createdArtifactList' - List of created artifacts up to the maximum number of results specified
-- in the request.
--
-- 'httpStatus', 'listCreatedArtifactsResponse_httpStatus' - The response's http status code.
newListCreatedArtifactsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCreatedArtifactsResponse
newListCreatedArtifactsResponse pHttpStatus_ =
  ListCreatedArtifactsResponse'
    { nextToken =
        Core.Nothing,
      createdArtifactList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more created artifacts than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listCreatedArtifactsResponse_nextToken :: Lens.Lens' ListCreatedArtifactsResponse (Core.Maybe Core.Text)
listCreatedArtifactsResponse_nextToken = Lens.lens (\ListCreatedArtifactsResponse' {nextToken} -> nextToken) (\s@ListCreatedArtifactsResponse' {} a -> s {nextToken = a} :: ListCreatedArtifactsResponse)

-- | List of created artifacts up to the maximum number of results specified
-- in the request.
listCreatedArtifactsResponse_createdArtifactList :: Lens.Lens' ListCreatedArtifactsResponse (Core.Maybe [CreatedArtifact])
listCreatedArtifactsResponse_createdArtifactList = Lens.lens (\ListCreatedArtifactsResponse' {createdArtifactList} -> createdArtifactList) (\s@ListCreatedArtifactsResponse' {} a -> s {createdArtifactList = a} :: ListCreatedArtifactsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCreatedArtifactsResponse_httpStatus :: Lens.Lens' ListCreatedArtifactsResponse Core.Int
listCreatedArtifactsResponse_httpStatus = Lens.lens (\ListCreatedArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListCreatedArtifactsResponse' {} a -> s {httpStatus = a} :: ListCreatedArtifactsResponse)

instance Core.NFData ListCreatedArtifactsResponse

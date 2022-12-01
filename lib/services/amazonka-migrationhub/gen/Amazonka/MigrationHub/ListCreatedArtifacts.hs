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
-- Module      : Amazonka.MigrationHub.ListCreatedArtifacts
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.MigrationHub.ListCreatedArtifacts
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCreatedArtifacts' smart constructor.
data ListCreatedArtifacts = ListCreatedArtifacts'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to be returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | Unique identifier that references the migration task. /Do not store
    -- personal data in this field./
    migrationTaskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  ListCreatedArtifacts
newListCreatedArtifacts
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListCreatedArtifacts'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listCreatedArtifacts_nextToken :: Lens.Lens' ListCreatedArtifacts (Prelude.Maybe Prelude.Text)
listCreatedArtifacts_nextToken = Lens.lens (\ListCreatedArtifacts' {nextToken} -> nextToken) (\s@ListCreatedArtifacts' {} a -> s {nextToken = a} :: ListCreatedArtifacts)

-- | Maximum number of results to be returned per page.
listCreatedArtifacts_maxResults :: Lens.Lens' ListCreatedArtifacts (Prelude.Maybe Prelude.Natural)
listCreatedArtifacts_maxResults = Lens.lens (\ListCreatedArtifacts' {maxResults} -> maxResults) (\s@ListCreatedArtifacts' {} a -> s {maxResults = a} :: ListCreatedArtifacts)

-- | The name of the ProgressUpdateStream.
listCreatedArtifacts_progressUpdateStream :: Lens.Lens' ListCreatedArtifacts Prelude.Text
listCreatedArtifacts_progressUpdateStream = Lens.lens (\ListCreatedArtifacts' {progressUpdateStream} -> progressUpdateStream) (\s@ListCreatedArtifacts' {} a -> s {progressUpdateStream = a} :: ListCreatedArtifacts)

-- | Unique identifier that references the migration task. /Do not store
-- personal data in this field./
listCreatedArtifacts_migrationTaskName :: Lens.Lens' ListCreatedArtifacts Prelude.Text
listCreatedArtifacts_migrationTaskName = Lens.lens (\ListCreatedArtifacts' {migrationTaskName} -> migrationTaskName) (\s@ListCreatedArtifacts' {} a -> s {migrationTaskName = a} :: ListCreatedArtifacts)

instance Core.AWSPager ListCreatedArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCreatedArtifactsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCreatedArtifactsResponse_createdArtifactList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCreatedArtifacts_nextToken
          Lens..~ rs
          Lens.^? listCreatedArtifactsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCreatedArtifacts where
  type
    AWSResponse ListCreatedArtifacts =
      ListCreatedArtifactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCreatedArtifactsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CreatedArtifactList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCreatedArtifacts where
  hashWithSalt _salt ListCreatedArtifacts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName

instance Prelude.NFData ListCreatedArtifacts where
  rnf ListCreatedArtifacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName

instance Core.ToHeaders ListCreatedArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.ListCreatedArtifacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCreatedArtifacts where
  toJSON ListCreatedArtifacts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Core..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Core..= migrationTaskName)
          ]
      )

instance Core.ToPath ListCreatedArtifacts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCreatedArtifacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCreatedArtifactsResponse' smart constructor.
data ListCreatedArtifactsResponse = ListCreatedArtifactsResponse'
  { -- | If there are more created artifacts than the max result, return the next
    -- token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of created artifacts up to the maximum number of results specified
    -- in the request.
    createdArtifactList :: Prelude.Maybe [CreatedArtifact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListCreatedArtifactsResponse
newListCreatedArtifactsResponse pHttpStatus_ =
  ListCreatedArtifactsResponse'
    { nextToken =
        Prelude.Nothing,
      createdArtifactList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more created artifacts than the max result, return the next
-- token to be passed to the next call as a bookmark of where to start
-- from.
listCreatedArtifactsResponse_nextToken :: Lens.Lens' ListCreatedArtifactsResponse (Prelude.Maybe Prelude.Text)
listCreatedArtifactsResponse_nextToken = Lens.lens (\ListCreatedArtifactsResponse' {nextToken} -> nextToken) (\s@ListCreatedArtifactsResponse' {} a -> s {nextToken = a} :: ListCreatedArtifactsResponse)

-- | List of created artifacts up to the maximum number of results specified
-- in the request.
listCreatedArtifactsResponse_createdArtifactList :: Lens.Lens' ListCreatedArtifactsResponse (Prelude.Maybe [CreatedArtifact])
listCreatedArtifactsResponse_createdArtifactList = Lens.lens (\ListCreatedArtifactsResponse' {createdArtifactList} -> createdArtifactList) (\s@ListCreatedArtifactsResponse' {} a -> s {createdArtifactList = a} :: ListCreatedArtifactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCreatedArtifactsResponse_httpStatus :: Lens.Lens' ListCreatedArtifactsResponse Prelude.Int
listCreatedArtifactsResponse_httpStatus = Lens.lens (\ListCreatedArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListCreatedArtifactsResponse' {} a -> s {httpStatus = a} :: ListCreatedArtifactsResponse)

instance Prelude.NFData ListCreatedArtifactsResponse where
  rnf ListCreatedArtifactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf createdArtifactList
      `Prelude.seq` Prelude.rnf httpStatus

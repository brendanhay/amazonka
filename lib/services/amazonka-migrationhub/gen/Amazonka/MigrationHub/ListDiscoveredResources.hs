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
-- Module      : Amazonka.MigrationHub.ListDiscoveredResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists discovered resources associated with the given @MigrationTask@.
--
-- This operation returns paginated results.
module Amazonka.MigrationHub.ListDiscoveredResources
  ( -- * Creating a Request
    ListDiscoveredResources (..),
    newListDiscoveredResources,

    -- * Request Lenses
    listDiscoveredResources_maxResults,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_progressUpdateStream,
    listDiscoveredResources_migrationTaskName,

    -- * Destructuring the Response
    ListDiscoveredResourcesResponse (..),
    newListDiscoveredResourcesResponse,

    -- * Response Lenses
    listDiscoveredResourcesResponse_discoveredResourceList,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | The maximum number of results returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | The name of the MigrationTask. /Do not store personal data in this
    -- field./
    migrationTaskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDiscoveredResources_maxResults' - The maximum number of results returned per page.
--
-- 'nextToken', 'listDiscoveredResources_nextToken' - If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
--
-- 'progressUpdateStream', 'listDiscoveredResources_progressUpdateStream' - The name of the ProgressUpdateStream.
--
-- 'migrationTaskName', 'listDiscoveredResources_migrationTaskName' - The name of the MigrationTask. /Do not store personal data in this
-- field./
newListDiscoveredResources ::
  -- | 'progressUpdateStream'
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  ListDiscoveredResources
newListDiscoveredResources
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListDiscoveredResources'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | The maximum number of results returned per page.
listDiscoveredResources_maxResults :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Natural)
listDiscoveredResources_maxResults = Lens.lens (\ListDiscoveredResources' {maxResults} -> maxResults) (\s@ListDiscoveredResources' {} a -> s {maxResults = a} :: ListDiscoveredResources)

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The name of the ProgressUpdateStream.
listDiscoveredResources_progressUpdateStream :: Lens.Lens' ListDiscoveredResources Prelude.Text
listDiscoveredResources_progressUpdateStream = Lens.lens (\ListDiscoveredResources' {progressUpdateStream} -> progressUpdateStream) (\s@ListDiscoveredResources' {} a -> s {progressUpdateStream = a} :: ListDiscoveredResources)

-- | The name of the MigrationTask. /Do not store personal data in this
-- field./
listDiscoveredResources_migrationTaskName :: Lens.Lens' ListDiscoveredResources Prelude.Text
listDiscoveredResources_migrationTaskName = Lens.lens (\ListDiscoveredResources' {migrationTaskName} -> migrationTaskName) (\s@ListDiscoveredResources' {} a -> s {migrationTaskName = a} :: ListDiscoveredResources)

instance Core.AWSPager ListDiscoveredResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_discoveredResourceList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listDiscoveredResourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDiscoveredResources where
  type
    AWSResponse ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Prelude.<$> ( x
                            Data..?> "DiscoveredResourceList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoveredResources where
  hashWithSalt _salt ListDiscoveredResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` progressUpdateStream
      `Prelude.hashWithSalt` migrationTaskName

instance Prelude.NFData ListDiscoveredResources where
  rnf ListDiscoveredResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf progressUpdateStream
      `Prelude.seq` Prelude.rnf migrationTaskName

instance Data.ToHeaders ListDiscoveredResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHub.ListDiscoveredResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Data..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Data..= migrationTaskName)
          ]
      )

instance Data.ToPath ListDiscoveredResources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDiscoveredResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | Returned list of discovered resources associated with the given
    -- MigrationTask.
    discoveredResourceList :: Prelude.Maybe [DiscoveredResource],
    -- | If there are more discovered resources than the max result, return the
    -- next token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveredResourceList', 'listDiscoveredResourcesResponse_discoveredResourceList' - Returned list of discovered resources associated with the given
-- MigrationTask.
--
-- 'nextToken', 'listDiscoveredResourcesResponse_nextToken' - If there are more discovered resources than the max result, return the
-- next token to be passed to the next call as a bookmark of where to start
-- from.
--
-- 'httpStatus', 'listDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { discoveredResourceList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returned list of discovered resources associated with the given
-- MigrationTask.
listDiscoveredResourcesResponse_discoveredResourceList :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe [DiscoveredResource])
listDiscoveredResourcesResponse_discoveredResourceList = Lens.lens (\ListDiscoveredResourcesResponse' {discoveredResourceList} -> discoveredResourceList) (\s@ListDiscoveredResourcesResponse' {} a -> s {discoveredResourceList = a} :: ListDiscoveredResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more discovered resources than the max result, return the
-- next token to be passed to the next call as a bookmark of where to start
-- from.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe Prelude.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Prelude.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance
  Prelude.NFData
    ListDiscoveredResourcesResponse
  where
  rnf ListDiscoveredResourcesResponse' {..} =
    Prelude.rnf discoveredResourceList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

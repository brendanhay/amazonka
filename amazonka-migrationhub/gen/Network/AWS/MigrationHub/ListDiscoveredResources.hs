{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | If a @NextToken@ was returned by a previous call, there are more results
    -- available. To retrieve the next page of results, make the call again
    -- using the returned token in @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Prelude.Text,
    -- | The name of the MigrationTask. /Do not store personal data in this
    -- field./
    migrationTaskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'migrationTaskName'
  Prelude.Text ->
  ListDiscoveredResources
newListDiscoveredResources
  pProgressUpdateStream_
  pMigrationTaskName_ =
    ListDiscoveredResources'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_
      }

-- | If a @NextToken@ was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again
-- using the returned token in @NextToken@.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The maximum number of results returned per page.
listDiscoveredResources_maxResults :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Natural)
listDiscoveredResources_maxResults = Lens.lens (\ListDiscoveredResources' {maxResults} -> maxResults) (\s@ListDiscoveredResources' {} a -> s {maxResults = a} :: ListDiscoveredResources)

-- | The name of the ProgressUpdateStream.
listDiscoveredResources_progressUpdateStream :: Lens.Lens' ListDiscoveredResources Prelude.Text
listDiscoveredResources_progressUpdateStream = Lens.lens (\ListDiscoveredResources' {progressUpdateStream} -> progressUpdateStream) (\s@ListDiscoveredResources' {} a -> s {progressUpdateStream = a} :: ListDiscoveredResources)

-- | The name of the MigrationTask. /Do not store personal data in this
-- field./
listDiscoveredResources_migrationTaskName :: Lens.Lens' ListDiscoveredResources Prelude.Text
listDiscoveredResources_migrationTaskName = Lens.lens (\ListDiscoveredResources' {migrationTaskName} -> migrationTaskName) (\s@ListDiscoveredResources' {} a -> s {migrationTaskName = a} :: ListDiscoveredResources)

instance Pager.AWSPager ListDiscoveredResources where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_discoveredResourceList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listDiscoveredResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDiscoveredResources where
  type
    Rs ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "DiscoveredResourceList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoveredResources

instance Prelude.NFData ListDiscoveredResources

instance Prelude.ToHeaders ListDiscoveredResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.ListDiscoveredResources" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "ProgressUpdateStream"
                  Prelude..= progressUpdateStream
              ),
            Prelude.Just
              ("MigrationTaskName" Prelude..= migrationTaskName)
          ]
      )

instance Prelude.ToPath ListDiscoveredResources where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDiscoveredResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | If there are more discovered resources than the max result, return the
    -- next token to be passed to the next call as a bookmark of where to start
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returned list of discovered resources associated with the given
    -- MigrationTask.
    discoveredResourceList :: Prelude.Maybe [DiscoveredResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      discoveredResourceList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more discovered resources than the max result, return the
-- next token to be passed to the next call as a bookmark of where to start
-- from.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe Prelude.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | Returned list of discovered resources associated with the given
-- MigrationTask.
listDiscoveredResourcesResponse_discoveredResourceList :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe [DiscoveredResource])
listDiscoveredResourcesResponse_discoveredResourceList = Lens.lens (\ListDiscoveredResourcesResponse' {discoveredResourceList} -> discoveredResourceList) (\s@ListDiscoveredResourcesResponse' {} a -> s {discoveredResourceList = a} :: ListDiscoveredResourcesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Prelude.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance
  Prelude.NFData
    ListDiscoveredResourcesResponse

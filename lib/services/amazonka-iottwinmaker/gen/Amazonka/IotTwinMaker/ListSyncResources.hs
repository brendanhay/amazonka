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
-- Module      : Amazonka.IotTwinMaker.ListSyncResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the sync resources.
module Amazonka.IotTwinMaker.ListSyncResources
  ( -- * Creating a Request
    ListSyncResources (..),
    newListSyncResources,

    -- * Request Lenses
    listSyncResources_filters,
    listSyncResources_maxResults,
    listSyncResources_nextToken,
    listSyncResources_workspaceId,
    listSyncResources_syncSource,

    -- * Destructuring the Response
    ListSyncResourcesResponse (..),
    newListSyncResourcesResponse,

    -- * Response Lenses
    listSyncResourcesResponse_nextToken,
    listSyncResourcesResponse_syncResources,
    listSyncResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSyncResources' smart constructor.
data ListSyncResources = ListSyncResources'
  { -- | A list of objects that filter the request.
    filters :: Prelude.Maybe [SyncResourceFilter],
    -- | The maximum number of results to return at one time. The default is 50.
    --
    -- Valid Range: Minimum value of 0. Maximum value of 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace that contains the sync job.
    workspaceId :: Prelude.Text,
    -- | The sync soucre.
    --
    -- Currently the only supported syncSoucre is @SITEWISE @.
    syncSource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSyncResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listSyncResources_filters' - A list of objects that filter the request.
--
-- 'maxResults', 'listSyncResources_maxResults' - The maximum number of results to return at one time. The default is 50.
--
-- Valid Range: Minimum value of 0. Maximum value of 200.
--
-- 'nextToken', 'listSyncResources_nextToken' - The string that specifies the next page of results.
--
-- 'workspaceId', 'listSyncResources_workspaceId' - The ID of the workspace that contains the sync job.
--
-- 'syncSource', 'listSyncResources_syncSource' - The sync soucre.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
newListSyncResources ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'syncSource'
  Prelude.Text ->
  ListSyncResources
newListSyncResources pWorkspaceId_ pSyncSource_ =
  ListSyncResources'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_,
      syncSource = pSyncSource_
    }

-- | A list of objects that filter the request.
listSyncResources_filters :: Lens.Lens' ListSyncResources (Prelude.Maybe [SyncResourceFilter])
listSyncResources_filters = Lens.lens (\ListSyncResources' {filters} -> filters) (\s@ListSyncResources' {} a -> s {filters = a} :: ListSyncResources) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return at one time. The default is 50.
--
-- Valid Range: Minimum value of 0. Maximum value of 200.
listSyncResources_maxResults :: Lens.Lens' ListSyncResources (Prelude.Maybe Prelude.Natural)
listSyncResources_maxResults = Lens.lens (\ListSyncResources' {maxResults} -> maxResults) (\s@ListSyncResources' {} a -> s {maxResults = a} :: ListSyncResources)

-- | The string that specifies the next page of results.
listSyncResources_nextToken :: Lens.Lens' ListSyncResources (Prelude.Maybe Prelude.Text)
listSyncResources_nextToken = Lens.lens (\ListSyncResources' {nextToken} -> nextToken) (\s@ListSyncResources' {} a -> s {nextToken = a} :: ListSyncResources)

-- | The ID of the workspace that contains the sync job.
listSyncResources_workspaceId :: Lens.Lens' ListSyncResources Prelude.Text
listSyncResources_workspaceId = Lens.lens (\ListSyncResources' {workspaceId} -> workspaceId) (\s@ListSyncResources' {} a -> s {workspaceId = a} :: ListSyncResources)

-- | The sync soucre.
--
-- Currently the only supported syncSoucre is @SITEWISE @.
listSyncResources_syncSource :: Lens.Lens' ListSyncResources Prelude.Text
listSyncResources_syncSource = Lens.lens (\ListSyncResources' {syncSource} -> syncSource) (\s@ListSyncResources' {} a -> s {syncSource = a} :: ListSyncResources)

instance Core.AWSRequest ListSyncResources where
  type
    AWSResponse ListSyncResources =
      ListSyncResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSyncResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "syncResources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSyncResources where
  hashWithSalt _salt ListSyncResources' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` syncSource

instance Prelude.NFData ListSyncResources where
  rnf ListSyncResources' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf workspaceId `Prelude.seq`
            Prelude.rnf syncSource

instance Data.ToHeaders ListSyncResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSyncResources where
  toJSON ListSyncResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSyncResources where
  toPath ListSyncResources' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/sync-jobs/",
        Data.toBS syncSource,
        "/resources-list"
      ]

instance Data.ToQuery ListSyncResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSyncResourcesResponse' smart constructor.
data ListSyncResourcesResponse = ListSyncResourcesResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sync resources.
    syncResources :: Prelude.Maybe [SyncResourceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSyncResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSyncResourcesResponse_nextToken' - The string that specifies the next page of results.
--
-- 'syncResources', 'listSyncResourcesResponse_syncResources' - The sync resources.
--
-- 'httpStatus', 'listSyncResourcesResponse_httpStatus' - The response's http status code.
newListSyncResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSyncResourcesResponse
newListSyncResourcesResponse pHttpStatus_ =
  ListSyncResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      syncResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that specifies the next page of results.
listSyncResourcesResponse_nextToken :: Lens.Lens' ListSyncResourcesResponse (Prelude.Maybe Prelude.Text)
listSyncResourcesResponse_nextToken = Lens.lens (\ListSyncResourcesResponse' {nextToken} -> nextToken) (\s@ListSyncResourcesResponse' {} a -> s {nextToken = a} :: ListSyncResourcesResponse)

-- | The sync resources.
listSyncResourcesResponse_syncResources :: Lens.Lens' ListSyncResourcesResponse (Prelude.Maybe [SyncResourceSummary])
listSyncResourcesResponse_syncResources = Lens.lens (\ListSyncResourcesResponse' {syncResources} -> syncResources) (\s@ListSyncResourcesResponse' {} a -> s {syncResources = a} :: ListSyncResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSyncResourcesResponse_httpStatus :: Lens.Lens' ListSyncResourcesResponse Prelude.Int
listSyncResourcesResponse_httpStatus = Lens.lens (\ListSyncResourcesResponse' {httpStatus} -> httpStatus) (\s@ListSyncResourcesResponse' {} a -> s {httpStatus = a} :: ListSyncResourcesResponse)

instance Prelude.NFData ListSyncResourcesResponse where
  rnf ListSyncResourcesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf syncResources `Prelude.seq`
        Prelude.rnf httpStatus

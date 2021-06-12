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
-- Module      : Network.AWS.AppSync.ListDataSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sources for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListDataSources
  ( -- * Creating a Request
    ListDataSources (..),
    newListDataSources,

    -- * Request Lenses
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_apiId,

    -- * Destructuring the Response
    ListDataSourcesResponse (..),
    newListDataSourcesResponse,

    -- * Response Lenses
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_dataSources,
    listDataSourcesResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The API ID.
    apiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDataSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSources_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listDataSources_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listDataSources_apiId' - The API ID.
newListDataSources ::
  -- | 'apiId'
  Core.Text ->
  ListDataSources
newListDataSources pApiId_ =
  ListDataSources'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      apiId = pApiId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDataSources_nextToken :: Lens.Lens' ListDataSources (Core.Maybe Core.Text)
listDataSources_nextToken = Lens.lens (\ListDataSources' {nextToken} -> nextToken) (\s@ListDataSources' {} a -> s {nextToken = a} :: ListDataSources)

-- | The maximum number of results you want the request to return.
listDataSources_maxResults :: Lens.Lens' ListDataSources (Core.Maybe Core.Natural)
listDataSources_maxResults = Lens.lens (\ListDataSources' {maxResults} -> maxResults) (\s@ListDataSources' {} a -> s {maxResults = a} :: ListDataSources)

-- | The API ID.
listDataSources_apiId :: Lens.Lens' ListDataSources Core.Text
listDataSources_apiId = Lens.lens (\ListDataSources' {apiId} -> apiId) (\s@ListDataSources' {} a -> s {apiId = a} :: ListDataSources)

instance Core.AWSPager ListDataSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSourcesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataSourcesResponse_dataSources
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDataSources_nextToken
          Lens..~ rs
          Lens.^? listDataSourcesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDataSources where
  type
    AWSResponse ListDataSources =
      ListDataSourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "dataSources" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDataSources

instance Core.NFData ListDataSources

instance Core.ToHeaders ListDataSources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListDataSources where
  toPath ListDataSources' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/datasources"]

instance Core.ToQuery ListDataSources where
  toQuery ListDataSources' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The @DataSource@ objects.
    dataSources :: Core.Maybe [DataSource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDataSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSourcesResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
--
-- 'dataSources', 'listDataSourcesResponse_dataSources' - The @DataSource@ objects.
--
-- 'httpStatus', 'listDataSourcesResponse_httpStatus' - The response's http status code.
newListDataSourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDataSourcesResponse
newListDataSourcesResponse pHttpStatus_ =
  ListDataSourcesResponse'
    { nextToken = Core.Nothing,
      dataSources = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listDataSourcesResponse_nextToken :: Lens.Lens' ListDataSourcesResponse (Core.Maybe Core.Text)
listDataSourcesResponse_nextToken = Lens.lens (\ListDataSourcesResponse' {nextToken} -> nextToken) (\s@ListDataSourcesResponse' {} a -> s {nextToken = a} :: ListDataSourcesResponse)

-- | The @DataSource@ objects.
listDataSourcesResponse_dataSources :: Lens.Lens' ListDataSourcesResponse (Core.Maybe [DataSource])
listDataSourcesResponse_dataSources = Lens.lens (\ListDataSourcesResponse' {dataSources} -> dataSources) (\s@ListDataSourcesResponse' {} a -> s {dataSources = a} :: ListDataSourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDataSourcesResponse_httpStatus :: Lens.Lens' ListDataSourcesResponse Core.Int
listDataSourcesResponse_httpStatus = Lens.lens (\ListDataSourcesResponse' {httpStatus} -> httpStatus) (\s@ListDataSourcesResponse' {} a -> s {httpStatus = a} :: ListDataSourcesResponse)

instance Core.NFData ListDataSourcesResponse

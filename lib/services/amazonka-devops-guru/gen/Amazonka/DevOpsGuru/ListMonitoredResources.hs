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
-- Module      : Amazonka.DevOpsGuru.ListMonitoredResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of all log groups that are being monitored and tagged
-- by DevOps Guru.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.ListMonitoredResources
  ( -- * Creating a Request
    ListMonitoredResources (..),
    newListMonitoredResources,

    -- * Request Lenses
    listMonitoredResources_filters,
    listMonitoredResources_maxResults,
    listMonitoredResources_nextToken,

    -- * Destructuring the Response
    ListMonitoredResourcesResponse (..),
    newListMonitoredResourcesResponse,

    -- * Response Lenses
    listMonitoredResourcesResponse_nextToken,
    listMonitoredResourcesResponse_httpStatus,
    listMonitoredResourcesResponse_monitoredResourceIdentifiers,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMonitoredResources' smart constructor.
data ListMonitoredResources = ListMonitoredResources'
  { -- | Filters to determine which monitored resources you want to retrieve. You
    -- can filter by resource type or resource permission status.
    filters :: Prelude.Maybe ListMonitoredResourcesFilters,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listMonitoredResources_filters' - Filters to determine which monitored resources you want to retrieve. You
-- can filter by resource type or resource permission status.
--
-- 'maxResults', 'listMonitoredResources_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listMonitoredResources_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
newListMonitoredResources ::
  ListMonitoredResources
newListMonitoredResources =
  ListMonitoredResources'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters to determine which monitored resources you want to retrieve. You
-- can filter by resource type or resource permission status.
listMonitoredResources_filters :: Lens.Lens' ListMonitoredResources (Prelude.Maybe ListMonitoredResourcesFilters)
listMonitoredResources_filters = Lens.lens (\ListMonitoredResources' {filters} -> filters) (\s@ListMonitoredResources' {} a -> s {filters = a} :: ListMonitoredResources)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listMonitoredResources_maxResults :: Lens.Lens' ListMonitoredResources (Prelude.Maybe Prelude.Natural)
listMonitoredResources_maxResults = Lens.lens (\ListMonitoredResources' {maxResults} -> maxResults) (\s@ListMonitoredResources' {} a -> s {maxResults = a} :: ListMonitoredResources)

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
listMonitoredResources_nextToken :: Lens.Lens' ListMonitoredResources (Prelude.Maybe Prelude.Text)
listMonitoredResources_nextToken = Lens.lens (\ListMonitoredResources' {nextToken} -> nextToken) (\s@ListMonitoredResources' {} a -> s {nextToken = a} :: ListMonitoredResources)

instance Core.AWSPager ListMonitoredResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoredResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listMonitoredResourcesResponse_monitoredResourceIdentifiers
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listMonitoredResources_nextToken
              Lens..~ rs
              Lens.^? listMonitoredResourcesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListMonitoredResources where
  type
    AWSResponse ListMonitoredResources =
      ListMonitoredResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoredResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "MonitoredResourceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMonitoredResources where
  hashWithSalt _salt ListMonitoredResources' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMonitoredResources where
  rnf ListMonitoredResources' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListMonitoredResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitoredResources where
  toJSON ListMonitoredResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListMonitoredResources where
  toPath = Prelude.const "/monitoredResources"

instance Data.ToQuery ListMonitoredResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitoredResourcesResponse' smart constructor.
data ListMonitoredResourcesResponse = ListMonitoredResourcesResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the resource that is being monitored, including the
    -- name of the resource, the type of resource, and whether or not
    -- permission is given to DevOps Guru to access that resource.
    monitoredResourceIdentifiers :: [MonitoredResourceIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitoredResourcesResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listMonitoredResourcesResponse_httpStatus' - The response's http status code.
--
-- 'monitoredResourceIdentifiers', 'listMonitoredResourcesResponse_monitoredResourceIdentifiers' - Information about the resource that is being monitored, including the
-- name of the resource, the type of resource, and whether or not
-- permission is given to DevOps Guru to access that resource.
newListMonitoredResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitoredResourcesResponse
newListMonitoredResourcesResponse pHttpStatus_ =
  ListMonitoredResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      monitoredResourceIdentifiers =
        Prelude.mempty
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
listMonitoredResourcesResponse_nextToken :: Lens.Lens' ListMonitoredResourcesResponse (Prelude.Maybe Prelude.Text)
listMonitoredResourcesResponse_nextToken = Lens.lens (\ListMonitoredResourcesResponse' {nextToken} -> nextToken) (\s@ListMonitoredResourcesResponse' {} a -> s {nextToken = a} :: ListMonitoredResourcesResponse)

-- | The response's http status code.
listMonitoredResourcesResponse_httpStatus :: Lens.Lens' ListMonitoredResourcesResponse Prelude.Int
listMonitoredResourcesResponse_httpStatus = Lens.lens (\ListMonitoredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListMonitoredResourcesResponse' {} a -> s {httpStatus = a} :: ListMonitoredResourcesResponse)

-- | Information about the resource that is being monitored, including the
-- name of the resource, the type of resource, and whether or not
-- permission is given to DevOps Guru to access that resource.
listMonitoredResourcesResponse_monitoredResourceIdentifiers :: Lens.Lens' ListMonitoredResourcesResponse [MonitoredResourceIdentifier]
listMonitoredResourcesResponse_monitoredResourceIdentifiers = Lens.lens (\ListMonitoredResourcesResponse' {monitoredResourceIdentifiers} -> monitoredResourceIdentifiers) (\s@ListMonitoredResourcesResponse' {} a -> s {monitoredResourceIdentifiers = a} :: ListMonitoredResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListMonitoredResourcesResponse
  where
  rnf ListMonitoredResourcesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf monitoredResourceIdentifiers

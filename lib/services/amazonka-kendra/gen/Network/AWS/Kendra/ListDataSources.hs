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
-- Module      : Network.AWS.Kendra.ListDataSources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sources that you have created.
module Network.AWS.Kendra.ListDataSources
  ( -- * Creating a Request
    ListDataSources (..),
    newListDataSources,

    -- * Request Lenses
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_indexId,

    -- * Destructuring the Response
    ListDataSourcesResponse (..),
    newListDataSourcesResponse,

    -- * Response Lenses
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_summaryItems,
    listDataSourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of data sources
    -- (@DataSourceSummaryItems@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of data sources to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index that contains the data source.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSources_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of data sources
-- (@DataSourceSummaryItems@).
--
-- 'maxResults', 'listDataSources_maxResults' - The maximum number of data sources to return.
--
-- 'indexId', 'listDataSources_indexId' - The identifier of the index that contains the data source.
newListDataSources ::
  -- | 'indexId'
  Prelude.Text ->
  ListDataSources
newListDataSources pIndexId_ =
  ListDataSources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of data sources
-- (@DataSourceSummaryItems@).
listDataSources_nextToken :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Text)
listDataSources_nextToken = Lens.lens (\ListDataSources' {nextToken} -> nextToken) (\s@ListDataSources' {} a -> s {nextToken = a} :: ListDataSources)

-- | The maximum number of data sources to return.
listDataSources_maxResults :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Natural)
listDataSources_maxResults = Lens.lens (\ListDataSources' {maxResults} -> maxResults) (\s@ListDataSources' {} a -> s {maxResults = a} :: ListDataSources)

-- | The identifier of the index that contains the data source.
listDataSources_indexId :: Lens.Lens' ListDataSources Prelude.Text
listDataSources_indexId = Lens.lens (\ListDataSources' {indexId} -> indexId) (\s@ListDataSources' {} a -> s {indexId = a} :: ListDataSources)

instance Core.AWSRequest ListDataSources where
  type
    AWSResponse ListDataSources =
      ListDataSourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSources

instance Prelude.NFData ListDataSources

instance Core.ToHeaders ListDataSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.ListDataSources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDataSources where
  toJSON ListDataSources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath ListDataSources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDataSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of data
    -- sources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more data sources.
    summaryItems :: Prelude.Maybe [DataSourceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSourcesResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of data
-- sources.
--
-- 'summaryItems', 'listDataSourcesResponse_summaryItems' - An array of summary information for one or more data sources.
--
-- 'httpStatus', 'listDataSourcesResponse_httpStatus' - The response's http status code.
newListDataSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataSourcesResponse
newListDataSourcesResponse pHttpStatus_ =
  ListDataSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of data
-- sources.
listDataSourcesResponse_nextToken :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe Prelude.Text)
listDataSourcesResponse_nextToken = Lens.lens (\ListDataSourcesResponse' {nextToken} -> nextToken) (\s@ListDataSourcesResponse' {} a -> s {nextToken = a} :: ListDataSourcesResponse)

-- | An array of summary information for one or more data sources.
listDataSourcesResponse_summaryItems :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe [DataSourceSummary])
listDataSourcesResponse_summaryItems = Lens.lens (\ListDataSourcesResponse' {summaryItems} -> summaryItems) (\s@ListDataSourcesResponse' {} a -> s {summaryItems = a} :: ListDataSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataSourcesResponse_httpStatus :: Lens.Lens' ListDataSourcesResponse Prelude.Int
listDataSourcesResponse_httpStatus = Lens.lens (\ListDataSourcesResponse' {httpStatus} -> httpStatus) (\s@ListDataSourcesResponse' {} a -> s {httpStatus = a} :: ListDataSourcesResponse)

instance Prelude.NFData ListDataSourcesResponse

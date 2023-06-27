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
-- Module      : Amazonka.Kendra.ListDataSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data source connectors that you have created.
module Amazonka.Kendra.ListDataSources
  ( -- * Creating a Request
    ListDataSources (..),
    newListDataSources,

    -- * Request Lenses
    listDataSources_maxResults,
    listDataSources_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { -- | The maximum number of data source connectors to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of data source
    -- connectors.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index used with one or more data source
    -- connectors.
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
-- 'maxResults', 'listDataSources_maxResults' - The maximum number of data source connectors to return.
--
-- 'nextToken', 'listDataSources_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of data source
-- connectors.
--
-- 'indexId', 'listDataSources_indexId' - The identifier of the index used with one or more data source
-- connectors.
newListDataSources ::
  -- | 'indexId'
  Prelude.Text ->
  ListDataSources
newListDataSources pIndexId_ =
  ListDataSources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | The maximum number of data source connectors to return.
listDataSources_maxResults :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Natural)
listDataSources_maxResults = Lens.lens (\ListDataSources' {maxResults} -> maxResults) (\s@ListDataSources' {} a -> s {maxResults = a} :: ListDataSources)

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of data source
-- connectors.
listDataSources_nextToken :: Lens.Lens' ListDataSources (Prelude.Maybe Prelude.Text)
listDataSources_nextToken = Lens.lens (\ListDataSources' {nextToken} -> nextToken) (\s@ListDataSources' {} a -> s {nextToken = a} :: ListDataSources)

-- | The identifier of the index used with one or more data source
-- connectors.
listDataSources_indexId :: Lens.Lens' ListDataSources Prelude.Text
listDataSources_indexId = Lens.lens (\ListDataSources' {indexId} -> indexId) (\s@ListDataSources' {} a -> s {indexId = a} :: ListDataSources)

instance Core.AWSRequest ListDataSources where
  type
    AWSResponse ListDataSources =
      ListDataSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSources where
  hashWithSalt _salt ListDataSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListDataSources where
  rnf ListDataSources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders ListDataSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListDataSources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataSources where
  toJSON ListDataSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListDataSources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDataSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of data
    -- source connectors.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more data source connector.
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
-- source connectors.
--
-- 'summaryItems', 'listDataSourcesResponse_summaryItems' - An array of summary information for one or more data source connector.
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
-- source connectors.
listDataSourcesResponse_nextToken :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe Prelude.Text)
listDataSourcesResponse_nextToken = Lens.lens (\ListDataSourcesResponse' {nextToken} -> nextToken) (\s@ListDataSourcesResponse' {} a -> s {nextToken = a} :: ListDataSourcesResponse)

-- | An array of summary information for one or more data source connector.
listDataSourcesResponse_summaryItems :: Lens.Lens' ListDataSourcesResponse (Prelude.Maybe [DataSourceSummary])
listDataSourcesResponse_summaryItems = Lens.lens (\ListDataSourcesResponse' {summaryItems} -> summaryItems) (\s@ListDataSourcesResponse' {} a -> s {summaryItems = a} :: ListDataSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataSourcesResponse_httpStatus :: Lens.Lens' ListDataSourcesResponse Prelude.Int
listDataSourcesResponse_httpStatus = Lens.lens (\ListDataSourcesResponse' {httpStatus} -> httpStatus) (\s@ListDataSourcesResponse' {} a -> s {httpStatus = a} :: ListDataSourcesResponse)

instance Prelude.NFData ListDataSourcesResponse where
  rnf ListDataSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaryItems
      `Prelude.seq` Prelude.rnf httpStatus

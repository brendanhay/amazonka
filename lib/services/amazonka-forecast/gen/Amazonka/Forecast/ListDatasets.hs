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
-- Module      : Amazonka.Forecast.ListDatasets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of datasets created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDataset.html CreateDataset>
-- operation. For each dataset, a summary of its properties, including its
-- Amazon Resource Name (ARN), is returned. To retrieve the complete set of
-- properties, use the ARN with the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDataset.html DescribeDataset>
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListDatasets
  ( -- * Creating a Request
    ListDatasets (..),
    newListDatasets,

    -- * Request Lenses
    listDatasets_nextToken,
    listDatasets_maxResults,

    -- * Destructuring the Response
    ListDatasetsResponse (..),
    newListDatasetsResponse,

    -- * Response Lenses
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasets' smart constructor.
data ListDatasets = ListDatasets'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasets_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'maxResults', 'listDatasets_maxResults' - The number of items to return in the response.
newListDatasets ::
  ListDatasets
newListDatasets =
  ListDatasets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listDatasets_nextToken :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Text)
listDatasets_nextToken = Lens.lens (\ListDatasets' {nextToken} -> nextToken) (\s@ListDatasets' {} a -> s {nextToken = a} :: ListDatasets)

-- | The number of items to return in the response.
listDatasets_maxResults :: Lens.Lens' ListDatasets (Prelude.Maybe Prelude.Natural)
listDatasets_maxResults = Lens.lens (\ListDatasets' {maxResults} -> maxResults) (\s@ListDatasets' {} a -> s {maxResults = a} :: ListDatasets)

instance Core.AWSPager ListDatasets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetsResponse_datasets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasets_nextToken
          Lens..~ rs
          Lens.^? listDatasetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDatasets where
  type AWSResponse ListDatasets = ListDatasetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Datasets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasets where
  hashWithSalt _salt ListDatasets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDatasets where
  rnf ListDatasets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListDatasets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.ListDatasets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatasets where
  toJSON ListDatasets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListDatasets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatasets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetsResponse' smart constructor.
data ListDatasetsResponse = ListDatasetsResponse'
  { -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that summarize each dataset\'s properties.
    datasets :: Prelude.Maybe [DatasetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'datasets', 'listDatasetsResponse_datasets' - An array of objects that summarize each dataset\'s properties.
--
-- 'httpStatus', 'listDatasetsResponse_httpStatus' - The response's http status code.
newListDatasetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetsResponse
newListDatasetsResponse pHttpStatus_ =
  ListDatasetsResponse'
    { nextToken = Prelude.Nothing,
      datasets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listDatasetsResponse_nextToken :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe Prelude.Text)
listDatasetsResponse_nextToken = Lens.lens (\ListDatasetsResponse' {nextToken} -> nextToken) (\s@ListDatasetsResponse' {} a -> s {nextToken = a} :: ListDatasetsResponse)

-- | An array of objects that summarize each dataset\'s properties.
listDatasetsResponse_datasets :: Lens.Lens' ListDatasetsResponse (Prelude.Maybe [DatasetSummary])
listDatasetsResponse_datasets = Lens.lens (\ListDatasetsResponse' {datasets} -> datasets) (\s@ListDatasetsResponse' {} a -> s {datasets = a} :: ListDatasetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetsResponse_httpStatus :: Lens.Lens' ListDatasetsResponse Prelude.Int
listDatasetsResponse_httpStatus = Lens.lens (\ListDatasetsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetsResponse' {} a -> s {httpStatus = a} :: ListDatasetsResponse)

instance Prelude.NFData ListDatasetsResponse where
  rnf ListDatasetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasets
      `Prelude.seq` Prelude.rnf httpStatus

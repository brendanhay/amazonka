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
-- Module      : Amazonka.Forecast.ListDatasetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of dataset groups created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>
-- operation. For each dataset group, this operation returns a summary of
-- its properties, including its Amazon Resource Name (ARN). You can
-- retrieve the complete set of properties by using the dataset group ARN
-- with the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_DescribeDatasetGroup.html DescribeDatasetGroup>
-- operation.
--
-- This operation returns paginated results.
module Amazonka.Forecast.ListDatasetGroups
  ( -- * Creating a Request
    ListDatasetGroups (..),
    newListDatasetGroups,

    -- * Request Lenses
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,

    -- * Destructuring the Response
    ListDatasetGroupsResponse (..),
    newListDatasetGroupsResponse,

    -- * Response Lenses
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetGroups' smart constructor.
data ListDatasetGroups = ListDatasetGroups'
  { -- | If the result of the previous request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of results, use the
    -- token in the next request. Tokens expire after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of items to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetGroups_nextToken' - If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
--
-- 'maxResults', 'listDatasetGroups_maxResults' - The number of items to return in the response.
newListDatasetGroups ::
  ListDatasetGroups
newListDatasetGroups =
  ListDatasetGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of results, use the
-- token in the next request. Tokens expire after 24 hours.
listDatasetGroups_nextToken :: Lens.Lens' ListDatasetGroups (Prelude.Maybe Prelude.Text)
listDatasetGroups_nextToken = Lens.lens (\ListDatasetGroups' {nextToken} -> nextToken) (\s@ListDatasetGroups' {} a -> s {nextToken = a} :: ListDatasetGroups)

-- | The number of items to return in the response.
listDatasetGroups_maxResults :: Lens.Lens' ListDatasetGroups (Prelude.Maybe Prelude.Natural)
listDatasetGroups_maxResults = Lens.lens (\ListDatasetGroups' {maxResults} -> maxResults) (\s@ListDatasetGroups' {} a -> s {maxResults = a} :: ListDatasetGroups)

instance Core.AWSPager ListDatasetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetGroupsResponse_datasetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasetGroups_nextToken
          Lens..~ rs
          Lens.^? listDatasetGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetGroups where
  type
    AWSResponse ListDatasetGroups =
      ListDatasetGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DatasetGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetGroups where
  hashWithSalt _salt ListDatasetGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDatasetGroups where
  rnf ListDatasetGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDatasetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.ListDatasetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDatasetGroups where
  toJSON ListDatasetGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDatasetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDatasetGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatasetGroupsResponse' smart constructor.
data ListDatasetGroupsResponse = ListDatasetGroupsResponse'
  { -- | If the response is truncated, Amazon Forecast returns this token. To
    -- retrieve the next set of results, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that summarize each dataset group\'s properties.
    datasetGroups :: Prelude.Maybe [DatasetGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetGroupsResponse_nextToken' - If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
--
-- 'datasetGroups', 'listDatasetGroupsResponse_datasetGroups' - An array of objects that summarize each dataset group\'s properties.
--
-- 'httpStatus', 'listDatasetGroupsResponse_httpStatus' - The response's http status code.
newListDatasetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetGroupsResponse
newListDatasetGroupsResponse pHttpStatus_ =
  ListDatasetGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      datasetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Forecast returns this token. To
-- retrieve the next set of results, use the token in the next request.
listDatasetGroupsResponse_nextToken :: Lens.Lens' ListDatasetGroupsResponse (Prelude.Maybe Prelude.Text)
listDatasetGroupsResponse_nextToken = Lens.lens (\ListDatasetGroupsResponse' {nextToken} -> nextToken) (\s@ListDatasetGroupsResponse' {} a -> s {nextToken = a} :: ListDatasetGroupsResponse)

-- | An array of objects that summarize each dataset group\'s properties.
listDatasetGroupsResponse_datasetGroups :: Lens.Lens' ListDatasetGroupsResponse (Prelude.Maybe [DatasetGroupSummary])
listDatasetGroupsResponse_datasetGroups = Lens.lens (\ListDatasetGroupsResponse' {datasetGroups} -> datasetGroups) (\s@ListDatasetGroupsResponse' {} a -> s {datasetGroups = a} :: ListDatasetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDatasetGroupsResponse_httpStatus :: Lens.Lens' ListDatasetGroupsResponse Prelude.Int
listDatasetGroupsResponse_httpStatus = Lens.lens (\ListDatasetGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetGroupsResponse' {} a -> s {httpStatus = a} :: ListDatasetGroupsResponse)

instance Prelude.NFData ListDatasetGroupsResponse where
  rnf ListDatasetGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetGroups
      `Prelude.seq` Prelude.rnf httpStatus

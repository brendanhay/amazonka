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
-- Module      : Amazonka.Glue.ListDataQualityResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all data quality execution results for your account.
module Amazonka.Glue.ListDataQualityResults
  ( -- * Creating a Request
    ListDataQualityResults (..),
    newListDataQualityResults,

    -- * Request Lenses
    listDataQualityResults_filter,
    listDataQualityResults_maxResults,
    listDataQualityResults_nextToken,

    -- * Destructuring the Response
    ListDataQualityResultsResponse (..),
    newListDataQualityResultsResponse,

    -- * Response Lenses
    listDataQualityResultsResponse_nextToken,
    listDataQualityResultsResponse_httpStatus,
    listDataQualityResultsResponse_results,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataQualityResults' smart constructor.
data ListDataQualityResults = ListDataQualityResults'
  { -- | The filter criteria.
    filter' :: Prelude.Maybe DataQualityResultFilterCriteria,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listDataQualityResults_filter' - The filter criteria.
--
-- 'maxResults', 'listDataQualityResults_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listDataQualityResults_nextToken' - A paginated token to offset the results.
newListDataQualityResults ::
  ListDataQualityResults
newListDataQualityResults =
  ListDataQualityResults'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The filter criteria.
listDataQualityResults_filter :: Lens.Lens' ListDataQualityResults (Prelude.Maybe DataQualityResultFilterCriteria)
listDataQualityResults_filter = Lens.lens (\ListDataQualityResults' {filter'} -> filter') (\s@ListDataQualityResults' {} a -> s {filter' = a} :: ListDataQualityResults)

-- | The maximum number of results to return.
listDataQualityResults_maxResults :: Lens.Lens' ListDataQualityResults (Prelude.Maybe Prelude.Natural)
listDataQualityResults_maxResults = Lens.lens (\ListDataQualityResults' {maxResults} -> maxResults) (\s@ListDataQualityResults' {} a -> s {maxResults = a} :: ListDataQualityResults)

-- | A paginated token to offset the results.
listDataQualityResults_nextToken :: Lens.Lens' ListDataQualityResults (Prelude.Maybe Prelude.Text)
listDataQualityResults_nextToken = Lens.lens (\ListDataQualityResults' {nextToken} -> nextToken) (\s@ListDataQualityResults' {} a -> s {nextToken = a} :: ListDataQualityResults)

instance Core.AWSRequest ListDataQualityResults where
  type
    AWSResponse ListDataQualityResults =
      ListDataQualityResultsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataQualityResultsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDataQualityResults where
  hashWithSalt _salt ListDataQualityResults' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDataQualityResults where
  rnf ListDataQualityResults' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDataQualityResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.ListDataQualityResults" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataQualityResults where
  toJSON ListDataQualityResults' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDataQualityResults where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDataQualityResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataQualityResultsResponse' smart constructor.
data ListDataQualityResultsResponse = ListDataQualityResultsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @DataQualityResultDescription@ objects.
    results :: [DataQualityResultDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataQualityResultsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'httpStatus', 'listDataQualityResultsResponse_httpStatus' - The response's http status code.
--
-- 'results', 'listDataQualityResultsResponse_results' - A list of @DataQualityResultDescription@ objects.
newListDataQualityResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataQualityResultsResponse
newListDataQualityResultsResponse pHttpStatus_ =
  ListDataQualityResultsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      results = Prelude.mempty
    }

-- | A pagination token, if more results are available.
listDataQualityResultsResponse_nextToken :: Lens.Lens' ListDataQualityResultsResponse (Prelude.Maybe Prelude.Text)
listDataQualityResultsResponse_nextToken = Lens.lens (\ListDataQualityResultsResponse' {nextToken} -> nextToken) (\s@ListDataQualityResultsResponse' {} a -> s {nextToken = a} :: ListDataQualityResultsResponse)

-- | The response's http status code.
listDataQualityResultsResponse_httpStatus :: Lens.Lens' ListDataQualityResultsResponse Prelude.Int
listDataQualityResultsResponse_httpStatus = Lens.lens (\ListDataQualityResultsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityResultsResponse' {} a -> s {httpStatus = a} :: ListDataQualityResultsResponse)

-- | A list of @DataQualityResultDescription@ objects.
listDataQualityResultsResponse_results :: Lens.Lens' ListDataQualityResultsResponse [DataQualityResultDescription]
listDataQualityResultsResponse_results = Lens.lens (\ListDataQualityResultsResponse' {results} -> results) (\s@ListDataQualityResultsResponse' {} a -> s {results = a} :: ListDataQualityResultsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDataQualityResultsResponse
  where
  rnf ListDataQualityResultsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results

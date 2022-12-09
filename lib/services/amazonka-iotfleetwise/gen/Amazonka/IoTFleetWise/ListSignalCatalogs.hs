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
-- Module      : Amazonka.IoTFleetWise.ListSignalCatalogs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the created signal catalogs in an Amazon Web Services account.
--
-- You can use to list information about each signal (node) specified in a
-- signal catalog.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListSignalCatalogs
  ( -- * Creating a Request
    ListSignalCatalogs (..),
    newListSignalCatalogs,

    -- * Request Lenses
    listSignalCatalogs_maxResults,
    listSignalCatalogs_nextToken,

    -- * Destructuring the Response
    ListSignalCatalogsResponse (..),
    newListSignalCatalogsResponse,

    -- * Response Lenses
    listSignalCatalogsResponse_nextToken,
    listSignalCatalogsResponse_summaries,
    listSignalCatalogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSignalCatalogs' smart constructor.
data ListSignalCatalogs = ListSignalCatalogs'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSignalCatalogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSignalCatalogs_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listSignalCatalogs_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
newListSignalCatalogs ::
  ListSignalCatalogs
newListSignalCatalogs =
  ListSignalCatalogs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listSignalCatalogs_maxResults :: Lens.Lens' ListSignalCatalogs (Prelude.Maybe Prelude.Natural)
listSignalCatalogs_maxResults = Lens.lens (\ListSignalCatalogs' {maxResults} -> maxResults) (\s@ListSignalCatalogs' {} a -> s {maxResults = a} :: ListSignalCatalogs)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listSignalCatalogs_nextToken :: Lens.Lens' ListSignalCatalogs (Prelude.Maybe Prelude.Text)
listSignalCatalogs_nextToken = Lens.lens (\ListSignalCatalogs' {nextToken} -> nextToken) (\s@ListSignalCatalogs' {} a -> s {nextToken = a} :: ListSignalCatalogs)

instance Core.AWSPager ListSignalCatalogs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSignalCatalogsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSignalCatalogsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSignalCatalogs_nextToken
          Lens..~ rs
          Lens.^? listSignalCatalogsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSignalCatalogs where
  type
    AWSResponse ListSignalCatalogs =
      ListSignalCatalogsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSignalCatalogsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSignalCatalogs where
  hashWithSalt _salt ListSignalCatalogs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSignalCatalogs where
  rnf ListSignalCatalogs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSignalCatalogs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListSignalCatalogs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSignalCatalogs where
  toJSON ListSignalCatalogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSignalCatalogs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSignalCatalogs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSignalCatalogsResponse' smart constructor.
data ListSignalCatalogsResponse = ListSignalCatalogsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information about each signal catalog.
    summaries :: Prelude.Maybe [SignalCatalogSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSignalCatalogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSignalCatalogsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'summaries', 'listSignalCatalogsResponse_summaries' - A list of information about each signal catalog.
--
-- 'httpStatus', 'listSignalCatalogsResponse_httpStatus' - The response's http status code.
newListSignalCatalogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSignalCatalogsResponse
newListSignalCatalogsResponse pHttpStatus_ =
  ListSignalCatalogsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listSignalCatalogsResponse_nextToken :: Lens.Lens' ListSignalCatalogsResponse (Prelude.Maybe Prelude.Text)
listSignalCatalogsResponse_nextToken = Lens.lens (\ListSignalCatalogsResponse' {nextToken} -> nextToken) (\s@ListSignalCatalogsResponse' {} a -> s {nextToken = a} :: ListSignalCatalogsResponse)

-- | A list of information about each signal catalog.
listSignalCatalogsResponse_summaries :: Lens.Lens' ListSignalCatalogsResponse (Prelude.Maybe [SignalCatalogSummary])
listSignalCatalogsResponse_summaries = Lens.lens (\ListSignalCatalogsResponse' {summaries} -> summaries) (\s@ListSignalCatalogsResponse' {} a -> s {summaries = a} :: ListSignalCatalogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSignalCatalogsResponse_httpStatus :: Lens.Lens' ListSignalCatalogsResponse Prelude.Int
listSignalCatalogsResponse_httpStatus = Lens.lens (\ListSignalCatalogsResponse' {httpStatus} -> httpStatus) (\s@ListSignalCatalogsResponse' {} a -> s {httpStatus = a} :: ListSignalCatalogsResponse)

instance Prelude.NFData ListSignalCatalogsResponse where
  rnf ListSignalCatalogsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus

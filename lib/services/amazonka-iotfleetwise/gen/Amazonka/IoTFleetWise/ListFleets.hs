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
-- Module      : Amazonka.IoTFleetWise.ListFleets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information for each created fleet in an Amazon Web Services
-- account.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListFleets
  ( -- * Creating a Request
    ListFleets (..),
    newListFleets,

    -- * Request Lenses
    listFleets_nextToken,
    listFleets_maxResults,

    -- * Destructuring the Response
    ListFleetsResponse (..),
    newListFleetsResponse,

    -- * Response Lenses
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetSummaries,
    listFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleets_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'maxResults', 'listFleets_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listFleets_nextToken :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | The maximum number of items to return, between 1 and 100, inclusive.
listFleets_maxResults :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Natural)
listFleets_maxResults = Lens.lens (\ListFleets' {maxResults} -> maxResults) (\s@ListFleets' {} a -> s {maxResults = a} :: ListFleets)

instance Core.AWSPager ListFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFleetsResponse_fleetSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFleets_nextToken
          Lens..~ rs
          Lens.^? listFleetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "fleetSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleets where
  hashWithSalt _salt ListFleets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFleets where
  rnf ListFleets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListFleets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information for each fleet.
    fleetSummaries :: Prelude.Maybe [FleetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFleetsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'fleetSummaries', 'listFleetsResponse_fleetSummaries' - A list of information for each fleet.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { nextToken = Prelude.Nothing,
      fleetSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Prelude.Maybe Prelude.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | A list of information for each fleet.
listFleetsResponse_fleetSummaries :: Lens.Lens' ListFleetsResponse (Prelude.Maybe [FleetSummary])
listFleetsResponse_fleetSummaries = Lens.lens (\ListFleetsResponse' {fleetSummaries} -> fleetSummaries) (\s@ListFleetsResponse' {} a -> s {fleetSummaries = a} :: ListFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Prelude.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Prelude.NFData ListFleetsResponse where
  rnf ListFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetSummaries
      `Prelude.seq` Prelude.rnf httpStatus

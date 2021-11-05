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
-- Module      : Amazonka.WorkLink.ListFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of fleets for the current account and Region.
module Amazonka.WorkLink.ListFleets
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
    listFleetsResponse_fleetSummaryList,
    listFleetsResponse_nextToken,
    listFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newListFleets' smart constructor.
data ListFleets = ListFleets'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
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
-- 'nextToken', 'listFleets_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation. If this value is null, it retrieves the first page.
--
-- 'maxResults', 'listFleets_maxResults' - The maximum number of results to be included in the next page.
newListFleets ::
  ListFleets
newListFleets =
  ListFleets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation. If this value is null, it retrieves the first page.
listFleets_nextToken :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Text)
listFleets_nextToken = Lens.lens (\ListFleets' {nextToken} -> nextToken) (\s@ListFleets' {} a -> s {nextToken = a} :: ListFleets)

-- | The maximum number of results to be included in the next page.
listFleets_maxResults :: Lens.Lens' ListFleets (Prelude.Maybe Prelude.Natural)
listFleets_maxResults = Lens.lens (\ListFleets' {maxResults} -> maxResults) (\s@ListFleets' {} a -> s {maxResults = a} :: ListFleets)

instance Core.AWSRequest ListFleets where
  type AWSResponse ListFleets = ListFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFleetsResponse'
            Prelude.<$> ( x Core..?> "FleetSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFleets

instance Prelude.NFData ListFleets

instance Core.ToHeaders ListFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFleets where
  toJSON ListFleets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFleets where
  toPath = Prelude.const "/listFleets"

instance Core.ToQuery ListFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { -- | The summary list of the fleets.
    fleetSummaryList :: Prelude.Maybe [FleetSummary],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'fleetSummaryList', 'listFleetsResponse_fleetSummaryList' - The summary list of the fleets.
--
-- 'nextToken', 'listFleetsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation. If there are no more pages, this value is null.
--
-- 'httpStatus', 'listFleetsResponse_httpStatus' - The response's http status code.
newListFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFleetsResponse
newListFleetsResponse pHttpStatus_ =
  ListFleetsResponse'
    { fleetSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary list of the fleets.
listFleetsResponse_fleetSummaryList :: Lens.Lens' ListFleetsResponse (Prelude.Maybe [FleetSummary])
listFleetsResponse_fleetSummaryList = Lens.lens (\ListFleetsResponse' {fleetSummaryList} -> fleetSummaryList) (\s@ListFleetsResponse' {} a -> s {fleetSummaryList = a} :: ListFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation. If there are no more pages, this value is null.
listFleetsResponse_nextToken :: Lens.Lens' ListFleetsResponse (Prelude.Maybe Prelude.Text)
listFleetsResponse_nextToken = Lens.lens (\ListFleetsResponse' {nextToken} -> nextToken) (\s@ListFleetsResponse' {} a -> s {nextToken = a} :: ListFleetsResponse)

-- | The response's http status code.
listFleetsResponse_httpStatus :: Lens.Lens' ListFleetsResponse Prelude.Int
listFleetsResponse_httpStatus = Lens.lens (\ListFleetsResponse' {httpStatus} -> httpStatus) (\s@ListFleetsResponse' {} a -> s {httpStatus = a} :: ListFleetsResponse)

instance Prelude.NFData ListFleetsResponse

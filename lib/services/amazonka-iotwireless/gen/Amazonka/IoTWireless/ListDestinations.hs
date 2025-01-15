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
-- Module      : Amazonka.IoTWireless.ListDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the destinations registered to your AWS account.
module Amazonka.IoTWireless.ListDestinations
  ( -- * Creating a Request
    ListDestinations (..),
    newListDestinations,

    -- * Request Lenses
    listDestinations_maxResults,
    listDestinations_nextToken,

    -- * Destructuring the Response
    ListDestinationsResponse (..),
    newListDestinationsResponse,

    -- * Response Lenses
    listDestinationsResponse_destinationList,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDestinations' smart constructor.
data ListDestinations = ListDestinations'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDestinations_maxResults' - The maximum number of results to return in this operation.
--
-- 'nextToken', 'listDestinations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
newListDestinations ::
  ListDestinations
newListDestinations =
  ListDestinations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this operation.
listDestinations_maxResults :: Lens.Lens' ListDestinations (Prelude.Maybe Prelude.Natural)
listDestinations_maxResults = Lens.lens (\ListDestinations' {maxResults} -> maxResults) (\s@ListDestinations' {} a -> s {maxResults = a} :: ListDestinations)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listDestinations_nextToken :: Lens.Lens' ListDestinations (Prelude.Maybe Prelude.Text)
listDestinations_nextToken = Lens.lens (\ListDestinations' {nextToken} -> nextToken) (\s@ListDestinations' {} a -> s {nextToken = a} :: ListDestinations)

instance Core.AWSRequest ListDestinations where
  type
    AWSResponse ListDestinations =
      ListDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDestinationsResponse'
            Prelude.<$> ( x
                            Data..?> "DestinationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDestinations where
  hashWithSalt _salt ListDestinations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDestinations where
  rnf ListDestinations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListDestinations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDestinations where
  toPath = Prelude.const "/destinations"

instance Data.ToQuery ListDestinations where
  toQuery ListDestinations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDestinationsResponse' smart constructor.
data ListDestinationsResponse = ListDestinationsResponse'
  { -- | The list of destinations.
    destinationList :: Prelude.Maybe [Destinations],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationList', 'listDestinationsResponse_destinationList' - The list of destinations.
--
-- 'nextToken', 'listDestinationsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listDestinationsResponse_httpStatus' - The response's http status code.
newListDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDestinationsResponse
newListDestinationsResponse pHttpStatus_ =
  ListDestinationsResponse'
    { destinationList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of destinations.
listDestinationsResponse_destinationList :: Lens.Lens' ListDestinationsResponse (Prelude.Maybe [Destinations])
listDestinationsResponse_destinationList = Lens.lens (\ListDestinationsResponse' {destinationList} -> destinationList) (\s@ListDestinationsResponse' {} a -> s {destinationList = a} :: ListDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listDestinationsResponse_nextToken :: Lens.Lens' ListDestinationsResponse (Prelude.Maybe Prelude.Text)
listDestinationsResponse_nextToken = Lens.lens (\ListDestinationsResponse' {nextToken} -> nextToken) (\s@ListDestinationsResponse' {} a -> s {nextToken = a} :: ListDestinationsResponse)

-- | The response's http status code.
listDestinationsResponse_httpStatus :: Lens.Lens' ListDestinationsResponse Prelude.Int
listDestinationsResponse_httpStatus = Lens.lens (\ListDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListDestinationsResponse' {} a -> s {httpStatus = a} :: ListDestinationsResponse)

instance Prelude.NFData ListDestinationsResponse where
  rnf ListDestinationsResponse' {..} =
    Prelude.rnf destinationList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

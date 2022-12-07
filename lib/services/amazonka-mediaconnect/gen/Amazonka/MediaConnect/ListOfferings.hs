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
-- Module      : Amazonka.MediaConnect.ListOfferings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of all offerings that are available to this account in
-- the current AWS Region. If you have an active reservation (which means
-- you\'ve purchased an offering that has already started and hasn\'t
-- expired yet), your account isn\'t eligible for other offerings.
--
-- This operation returns paginated results.
module Amazonka.MediaConnect.ListOfferings
  ( -- * Creating a Request
    ListOfferings (..),
    newListOfferings,

    -- * Request Lenses
    listOfferings_nextToken,
    listOfferings_maxResults,

    -- * Destructuring the Response
    ListOfferingsResponse (..),
    newListOfferingsResponse,

    -- * Response Lenses
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOfferings' smart constructor.
data ListOfferings = ListOfferings'
  { -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListOfferings request with MaxResults set at
    -- 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListOfferings request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per API request. For example,
    -- you submit a ListOfferings request with MaxResults set at 5. Although 20
    -- items match your request, the service returns no more than the first 5
    -- items. (The service also returns a NextToken value that you can use to
    -- fetch the next batch of results.) The service might return fewer results
    -- than the MaxResults value. If MaxResults is not included in the request,
    -- the service defaults to pagination with a maximum of 10 results per
    -- page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferings_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListOfferings request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListOfferings request a second time and specify the NextToken value.
--
-- 'maxResults', 'listOfferings_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a ListOfferings request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
newListOfferings ::
  ListOfferings
newListOfferings =
  ListOfferings'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListOfferings request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListOfferings request a second time and specify the NextToken value.
listOfferings_nextToken :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Text)
listOfferings_nextToken = Lens.lens (\ListOfferings' {nextToken} -> nextToken) (\s@ListOfferings' {} a -> s {nextToken = a} :: ListOfferings)

-- | The maximum number of results to return per API request. For example,
-- you submit a ListOfferings request with MaxResults set at 5. Although 20
-- items match your request, the service returns no more than the first 5
-- items. (The service also returns a NextToken value that you can use to
-- fetch the next batch of results.) The service might return fewer results
-- than the MaxResults value. If MaxResults is not included in the request,
-- the service defaults to pagination with a maximum of 10 results per
-- page.
listOfferings_maxResults :: Lens.Lens' ListOfferings (Prelude.Maybe Prelude.Natural)
listOfferings_maxResults = Lens.lens (\ListOfferings' {maxResults} -> maxResults) (\s@ListOfferings' {} a -> s {maxResults = a} :: ListOfferings)

instance Core.AWSPager ListOfferings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOfferingsResponse_offerings Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOfferings_nextToken
          Lens..~ rs
          Lens.^? listOfferingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListOfferings where
  type
    AWSResponse ListOfferings =
      ListOfferingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOfferingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "offerings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOfferings where
  hashWithSalt _salt ListOfferings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListOfferings where
  rnf ListOfferings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListOfferings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListOfferings where
  toPath = Prelude.const "/v1/offerings"

instance Data.ToQuery ListOfferings where
  toQuery ListOfferings' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListOfferingsResponse' smart constructor.
data ListOfferingsResponse = ListOfferingsResponse'
  { -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a ListOfferings request with MaxResults set at
    -- 5. The service returns the first batch of results (up to 5) and a
    -- NextToken value. To see the next batch of results, you can submit the
    -- ListOfferings request a second time and specify the NextToken value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of offerings that are available to this account in the current
    -- AWS Region.
    offerings :: Prelude.Maybe [Offering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOfferingsResponse_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a ListOfferings request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListOfferings request a second time and specify the NextToken value.
--
-- 'offerings', 'listOfferingsResponse_offerings' - A list of offerings that are available to this account in the current
-- AWS Region.
--
-- 'httpStatus', 'listOfferingsResponse_httpStatus' - The response's http status code.
newListOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOfferingsResponse
newListOfferingsResponse pHttpStatus_ =
  ListOfferingsResponse'
    { nextToken = Prelude.Nothing,
      offerings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a ListOfferings request with MaxResults set at
-- 5. The service returns the first batch of results (up to 5) and a
-- NextToken value. To see the next batch of results, you can submit the
-- ListOfferings request a second time and specify the NextToken value.
listOfferingsResponse_nextToken :: Lens.Lens' ListOfferingsResponse (Prelude.Maybe Prelude.Text)
listOfferingsResponse_nextToken = Lens.lens (\ListOfferingsResponse' {nextToken} -> nextToken) (\s@ListOfferingsResponse' {} a -> s {nextToken = a} :: ListOfferingsResponse)

-- | A list of offerings that are available to this account in the current
-- AWS Region.
listOfferingsResponse_offerings :: Lens.Lens' ListOfferingsResponse (Prelude.Maybe [Offering])
listOfferingsResponse_offerings = Lens.lens (\ListOfferingsResponse' {offerings} -> offerings) (\s@ListOfferingsResponse' {} a -> s {offerings = a} :: ListOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOfferingsResponse_httpStatus :: Lens.Lens' ListOfferingsResponse Prelude.Int
listOfferingsResponse_httpStatus = Lens.lens (\ListOfferingsResponse' {httpStatus} -> httpStatus) (\s@ListOfferingsResponse' {} a -> s {httpStatus = a} :: ListOfferingsResponse)

instance Prelude.NFData ListOfferingsResponse where
  rnf ListOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf offerings
      `Prelude.seq` Prelude.rnf httpStatus

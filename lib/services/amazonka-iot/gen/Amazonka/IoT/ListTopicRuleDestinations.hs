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
-- Module      : Amazonka.IoT.ListTopicRuleDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the topic rule destinations in your Amazon Web Services
-- account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListTopicRuleDestinations>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListTopicRuleDestinations
  ( -- * Creating a Request
    ListTopicRuleDestinations (..),
    newListTopicRuleDestinations,

    -- * Request Lenses
    listTopicRuleDestinations_maxResults,
    listTopicRuleDestinations_nextToken,

    -- * Destructuring the Response
    ListTopicRuleDestinationsResponse (..),
    newListTopicRuleDestinationsResponse,

    -- * Response Lenses
    listTopicRuleDestinationsResponse_destinationSummaries,
    listTopicRuleDestinationsResponse_nextToken,
    listTopicRuleDestinationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTopicRuleDestinations' smart constructor.
data ListTopicRuleDestinations = ListTopicRuleDestinations'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRuleDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTopicRuleDestinations_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listTopicRuleDestinations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
newListTopicRuleDestinations ::
  ListTopicRuleDestinations
newListTopicRuleDestinations =
  ListTopicRuleDestinations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return at one time.
listTopicRuleDestinations_maxResults :: Lens.Lens' ListTopicRuleDestinations (Prelude.Maybe Prelude.Natural)
listTopicRuleDestinations_maxResults = Lens.lens (\ListTopicRuleDestinations' {maxResults} -> maxResults) (\s@ListTopicRuleDestinations' {} a -> s {maxResults = a} :: ListTopicRuleDestinations)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listTopicRuleDestinations_nextToken :: Lens.Lens' ListTopicRuleDestinations (Prelude.Maybe Prelude.Text)
listTopicRuleDestinations_nextToken = Lens.lens (\ListTopicRuleDestinations' {nextToken} -> nextToken) (\s@ListTopicRuleDestinations' {} a -> s {nextToken = a} :: ListTopicRuleDestinations)

instance Core.AWSPager ListTopicRuleDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicRuleDestinationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicRuleDestinationsResponse_destinationSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTopicRuleDestinations_nextToken
          Lens..~ rs
          Lens.^? listTopicRuleDestinationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTopicRuleDestinations where
  type
    AWSResponse ListTopicRuleDestinations =
      ListTopicRuleDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRuleDestinationsResponse'
            Prelude.<$> ( x
                            Data..?> "destinationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopicRuleDestinations where
  hashWithSalt _salt ListTopicRuleDestinations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTopicRuleDestinations where
  rnf ListTopicRuleDestinations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListTopicRuleDestinations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTopicRuleDestinations where
  toPath = Prelude.const "/destinations"

instance Data.ToQuery ListTopicRuleDestinations where
  toQuery ListTopicRuleDestinations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListTopicRuleDestinationsResponse' smart constructor.
data ListTopicRuleDestinationsResponse = ListTopicRuleDestinationsResponse'
  { -- | Information about a topic rule destination.
    destinationSummaries :: Prelude.Maybe [TopicRuleDestinationSummary],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicRuleDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationSummaries', 'listTopicRuleDestinationsResponse_destinationSummaries' - Information about a topic rule destination.
--
-- 'nextToken', 'listTopicRuleDestinationsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listTopicRuleDestinationsResponse_httpStatus' - The response's http status code.
newListTopicRuleDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTopicRuleDestinationsResponse
newListTopicRuleDestinationsResponse pHttpStatus_ =
  ListTopicRuleDestinationsResponse'
    { destinationSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a topic rule destination.
listTopicRuleDestinationsResponse_destinationSummaries :: Lens.Lens' ListTopicRuleDestinationsResponse (Prelude.Maybe [TopicRuleDestinationSummary])
listTopicRuleDestinationsResponse_destinationSummaries = Lens.lens (\ListTopicRuleDestinationsResponse' {destinationSummaries} -> destinationSummaries) (\s@ListTopicRuleDestinationsResponse' {} a -> s {destinationSummaries = a} :: ListTopicRuleDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listTopicRuleDestinationsResponse_nextToken :: Lens.Lens' ListTopicRuleDestinationsResponse (Prelude.Maybe Prelude.Text)
listTopicRuleDestinationsResponse_nextToken = Lens.lens (\ListTopicRuleDestinationsResponse' {nextToken} -> nextToken) (\s@ListTopicRuleDestinationsResponse' {} a -> s {nextToken = a} :: ListTopicRuleDestinationsResponse)

-- | The response's http status code.
listTopicRuleDestinationsResponse_httpStatus :: Lens.Lens' ListTopicRuleDestinationsResponse Prelude.Int
listTopicRuleDestinationsResponse_httpStatus = Lens.lens (\ListTopicRuleDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListTopicRuleDestinationsResponse' {} a -> s {httpStatus = a} :: ListTopicRuleDestinationsResponse)

instance
  Prelude.NFData
    ListTopicRuleDestinationsResponse
  where
  rnf ListTopicRuleDestinationsResponse' {..} =
    Prelude.rnf destinationSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

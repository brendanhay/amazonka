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
-- Module      : Network.AWS.IoT.ListTopicRuleDestinations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the topic rule destinations in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRuleDestinations
  ( -- * Creating a Request
    ListTopicRuleDestinations (..),
    newListTopicRuleDestinations,

    -- * Request Lenses
    listTopicRuleDestinations_nextToken,
    listTopicRuleDestinations_maxResults,

    -- * Destructuring the Response
    ListTopicRuleDestinationsResponse (..),
    newListTopicRuleDestinationsResponse,

    -- * Response Lenses
    listTopicRuleDestinationsResponse_nextToken,
    listTopicRuleDestinationsResponse_destinationSummaries,
    listTopicRuleDestinationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTopicRuleDestinations' smart constructor.
data ListTopicRuleDestinations = ListTopicRuleDestinations'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopicRuleDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicRuleDestinations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listTopicRuleDestinations_maxResults' - The maximum number of results to return at one time.
newListTopicRuleDestinations ::
  ListTopicRuleDestinations
newListTopicRuleDestinations =
  ListTopicRuleDestinations'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listTopicRuleDestinations_nextToken :: Lens.Lens' ListTopicRuleDestinations (Core.Maybe Core.Text)
listTopicRuleDestinations_nextToken = Lens.lens (\ListTopicRuleDestinations' {nextToken} -> nextToken) (\s@ListTopicRuleDestinations' {} a -> s {nextToken = a} :: ListTopicRuleDestinations)

-- | The maximum number of results to return at one time.
listTopicRuleDestinations_maxResults :: Lens.Lens' ListTopicRuleDestinations (Core.Maybe Core.Natural)
listTopicRuleDestinations_maxResults = Lens.lens (\ListTopicRuleDestinations' {maxResults} -> maxResults) (\s@ListTopicRuleDestinations' {} a -> s {maxResults = a} :: ListTopicRuleDestinations)

instance Core.AWSPager ListTopicRuleDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicRuleDestinationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicRuleDestinationsResponse_destinationSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTopicRuleDestinations_nextToken
          Lens..~ rs
          Lens.^? listTopicRuleDestinationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTopicRuleDestinations where
  type
    AWSResponse ListTopicRuleDestinations =
      ListTopicRuleDestinationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRuleDestinationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "destinationSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTopicRuleDestinations

instance Core.NFData ListTopicRuleDestinations

instance Core.ToHeaders ListTopicRuleDestinations where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTopicRuleDestinations where
  toPath = Core.const "/destinations"

instance Core.ToQuery ListTopicRuleDestinations where
  toQuery ListTopicRuleDestinations' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTopicRuleDestinationsResponse' smart constructor.
data ListTopicRuleDestinationsResponse = ListTopicRuleDestinationsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a topic rule destination.
    destinationSummaries :: Core.Maybe [TopicRuleDestinationSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopicRuleDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicRuleDestinationsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'destinationSummaries', 'listTopicRuleDestinationsResponse_destinationSummaries' - Information about a topic rule destination.
--
-- 'httpStatus', 'listTopicRuleDestinationsResponse_httpStatus' - The response's http status code.
newListTopicRuleDestinationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTopicRuleDestinationsResponse
newListTopicRuleDestinationsResponse pHttpStatus_ =
  ListTopicRuleDestinationsResponse'
    { nextToken =
        Core.Nothing,
      destinationSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listTopicRuleDestinationsResponse_nextToken :: Lens.Lens' ListTopicRuleDestinationsResponse (Core.Maybe Core.Text)
listTopicRuleDestinationsResponse_nextToken = Lens.lens (\ListTopicRuleDestinationsResponse' {nextToken} -> nextToken) (\s@ListTopicRuleDestinationsResponse' {} a -> s {nextToken = a} :: ListTopicRuleDestinationsResponse)

-- | Information about a topic rule destination.
listTopicRuleDestinationsResponse_destinationSummaries :: Lens.Lens' ListTopicRuleDestinationsResponse (Core.Maybe [TopicRuleDestinationSummary])
listTopicRuleDestinationsResponse_destinationSummaries = Lens.lens (\ListTopicRuleDestinationsResponse' {destinationSummaries} -> destinationSummaries) (\s@ListTopicRuleDestinationsResponse' {} a -> s {destinationSummaries = a} :: ListTopicRuleDestinationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTopicRuleDestinationsResponse_httpStatus :: Lens.Lens' ListTopicRuleDestinationsResponse Core.Int
listTopicRuleDestinationsResponse_httpStatus = Lens.lens (\ListTopicRuleDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListTopicRuleDestinationsResponse' {} a -> s {httpStatus = a} :: ListTopicRuleDestinationsResponse)

instance
  Core.NFData
    ListTopicRuleDestinationsResponse

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
-- Module      : Network.AWS.LexV2Models.ListBots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of available bots.
module Network.AWS.LexV2Models.ListBots
  ( -- * Creating a Request
    ListBots (..),
    newListBots,

    -- * Request Lenses
    listBots_filters,
    listBots_nextToken,
    listBots_maxResults,
    listBots_sortBy,

    -- * Destructuring the Response
    ListBotsResponse (..),
    newListBotsResponse,

    -- * Response Lenses
    listBotsResponse_botSummaries,
    listBotsResponse_nextToken,
    listBotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBots' smart constructor.
data ListBots = ListBots'
  { -- | Provides the specification of a filter used to limit the bots in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty BotFilter),
    -- | If the response from the @ListBots@ operation contains more results than
    -- specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of bots to return in each page of results. If there
    -- are fewer results than the maximum page size, only the actual number of
    -- results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies sorting parameters for the list of bots. You can specify that
    -- the list be sorted by bot name in ascending or descending order.
    sortBy :: Prelude.Maybe BotSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listBots_filters' - Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'nextToken', 'listBots_nextToken' - If the response from the @ListBots@ operation contains more results than
-- specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'maxResults', 'listBots_maxResults' - The maximum number of bots to return in each page of results. If there
-- are fewer results than the maximum page size, only the actual number of
-- results are returned.
--
-- 'sortBy', 'listBots_sortBy' - Specifies sorting parameters for the list of bots. You can specify that
-- the list be sorted by bot name in ascending or descending order.
newListBots ::
  ListBots
newListBots =
  ListBots'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listBots_filters :: Lens.Lens' ListBots (Prelude.Maybe (Prelude.NonEmpty BotFilter))
listBots_filters = Lens.lens (\ListBots' {filters} -> filters) (\s@ListBots' {} a -> s {filters = a} :: ListBots) Prelude.. Lens.mapping Lens.coerced

-- | If the response from the @ListBots@ operation contains more results than
-- specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listBots_nextToken :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Text)
listBots_nextToken = Lens.lens (\ListBots' {nextToken} -> nextToken) (\s@ListBots' {} a -> s {nextToken = a} :: ListBots)

-- | The maximum number of bots to return in each page of results. If there
-- are fewer results than the maximum page size, only the actual number of
-- results are returned.
listBots_maxResults :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Natural)
listBots_maxResults = Lens.lens (\ListBots' {maxResults} -> maxResults) (\s@ListBots' {} a -> s {maxResults = a} :: ListBots)

-- | Specifies sorting parameters for the list of bots. You can specify that
-- the list be sorted by bot name in ascending or descending order.
listBots_sortBy :: Lens.Lens' ListBots (Prelude.Maybe BotSortBy)
listBots_sortBy = Lens.lens (\ListBots' {sortBy} -> sortBy) (\s@ListBots' {} a -> s {sortBy = a} :: ListBots)

instance Core.AWSRequest ListBots where
  type AWSResponse ListBots = ListBotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotsResponse'
            Prelude.<$> (x Core..?> "botSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBots

instance Prelude.NFData ListBots

instance Core.ToHeaders ListBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBots where
  toJSON ListBots' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListBots where
  toPath = Prelude.const "/bots/"

instance Core.ToQuery ListBots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotsResponse' smart constructor.
data ListBotsResponse = ListBotsResponse'
  { -- | Summary information for the bots that meet the filter criteria specified
    -- in the request. The length of the list is specified in the @maxResults@
    -- parameter of the request. If there are more bots available, the
    -- @nextToken@ field contains a token to the next page of results.
    botSummaries :: Prelude.Maybe [BotSummary],
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBots@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListBots@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botSummaries', 'listBotsResponse_botSummaries' - Summary information for the bots that meet the filter criteria specified
-- in the request. The length of the list is specified in the @maxResults@
-- parameter of the request. If there are more bots available, the
-- @nextToken@ field contains a token to the next page of results.
--
-- 'nextToken', 'listBotsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBots@ operation request to get the next page of results.
--
-- 'httpStatus', 'listBotsResponse_httpStatus' - The response's http status code.
newListBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotsResponse
newListBotsResponse pHttpStatus_ =
  ListBotsResponse'
    { botSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for the bots that meet the filter criteria specified
-- in the request. The length of the list is specified in the @maxResults@
-- parameter of the request. If there are more bots available, the
-- @nextToken@ field contains a token to the next page of results.
listBotsResponse_botSummaries :: Lens.Lens' ListBotsResponse (Prelude.Maybe [BotSummary])
listBotsResponse_botSummaries = Lens.lens (\ListBotsResponse' {botSummaries} -> botSummaries) (\s@ListBotsResponse' {} a -> s {botSummaries = a} :: ListBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBots@ operation request to get the next page of results.
listBotsResponse_nextToken :: Lens.Lens' ListBotsResponse (Prelude.Maybe Prelude.Text)
listBotsResponse_nextToken = Lens.lens (\ListBotsResponse' {nextToken} -> nextToken) (\s@ListBotsResponse' {} a -> s {nextToken = a} :: ListBotsResponse)

-- | The response's http status code.
listBotsResponse_httpStatus :: Lens.Lens' ListBotsResponse Prelude.Int
listBotsResponse_httpStatus = Lens.lens (\ListBotsResponse' {httpStatus} -> httpStatus) (\s@ListBotsResponse' {} a -> s {httpStatus = a} :: ListBotsResponse)

instance Prelude.NFData ListBotsResponse

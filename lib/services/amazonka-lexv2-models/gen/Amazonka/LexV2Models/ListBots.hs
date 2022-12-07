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
-- Module      : Amazonka.LexV2Models.ListBots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of available bots.
module Amazonka.LexV2Models.ListBots
  ( -- * Creating a Request
    ListBots (..),
    newListBots,

    -- * Request Lenses
    listBots_nextToken,
    listBots_filters,
    listBots_sortBy,
    listBots_maxResults,

    -- * Destructuring the Response
    ListBotsResponse (..),
    newListBotsResponse,

    -- * Response Lenses
    listBotsResponse_nextToken,
    listBotsResponse_botSummaries,
    listBotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBots' smart constructor.
data ListBots = ListBots'
  { -- | If the response from the @ListBots@ operation contains more results than
    -- specified in the @maxResults@ parameter, a token is returned in the
    -- response.
    --
    -- Use the returned token in the @nextToken@ parameter of a @ListBots@
    -- request to return the next page of results. For a complete set of
    -- results, call the @ListBots@ operation until the @nextToken@ returned in
    -- the response is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides the specification of a filter used to limit the bots in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty BotFilter),
    -- | Specifies sorting parameters for the list of bots. You can specify that
    -- the list be sorted by bot name in ascending or descending order.
    sortBy :: Prelude.Maybe BotSortBy,
    -- | The maximum number of bots to return in each page of results. If there
    -- are fewer results than the maximum page size, only the actual number of
    -- results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listBots_nextToken' - If the response from the @ListBots@ operation contains more results than
-- specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListBots@
-- request to return the next page of results. For a complete set of
-- results, call the @ListBots@ operation until the @nextToken@ returned in
-- the response is null.
--
-- 'filters', 'listBots_filters' - Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'sortBy', 'listBots_sortBy' - Specifies sorting parameters for the list of bots. You can specify that
-- the list be sorted by bot name in ascending or descending order.
--
-- 'maxResults', 'listBots_maxResults' - The maximum number of bots to return in each page of results. If there
-- are fewer results than the maximum page size, only the actual number of
-- results are returned.
newListBots ::
  ListBots
newListBots =
  ListBots'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the response from the @ListBots@ operation contains more results than
-- specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListBots@
-- request to return the next page of results. For a complete set of
-- results, call the @ListBots@ operation until the @nextToken@ returned in
-- the response is null.
listBots_nextToken :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Text)
listBots_nextToken = Lens.lens (\ListBots' {nextToken} -> nextToken) (\s@ListBots' {} a -> s {nextToken = a} :: ListBots)

-- | Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listBots_filters :: Lens.Lens' ListBots (Prelude.Maybe (Prelude.NonEmpty BotFilter))
listBots_filters = Lens.lens (\ListBots' {filters} -> filters) (\s@ListBots' {} a -> s {filters = a} :: ListBots) Prelude.. Lens.mapping Lens.coerced

-- | Specifies sorting parameters for the list of bots. You can specify that
-- the list be sorted by bot name in ascending or descending order.
listBots_sortBy :: Lens.Lens' ListBots (Prelude.Maybe BotSortBy)
listBots_sortBy = Lens.lens (\ListBots' {sortBy} -> sortBy) (\s@ListBots' {} a -> s {sortBy = a} :: ListBots)

-- | The maximum number of bots to return in each page of results. If there
-- are fewer results than the maximum page size, only the actual number of
-- results are returned.
listBots_maxResults :: Lens.Lens' ListBots (Prelude.Maybe Prelude.Natural)
listBots_maxResults = Lens.lens (\ListBots' {maxResults} -> maxResults) (\s@ListBots' {} a -> s {maxResults = a} :: ListBots)

instance Core.AWSRequest ListBots where
  type AWSResponse ListBots = ListBotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "botSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBots where
  hashWithSalt _salt ListBots' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListBots where
  rnf ListBots' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBots where
  toJSON ListBots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListBots where
  toPath = Prelude.const "/bots/"

instance Data.ToQuery ListBots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotsResponse' smart constructor.
data ListBotsResponse = ListBotsResponse'
  { -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBots@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListBots@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the bots that meet the filter criteria specified
    -- in the request. The length of the list is specified in the @maxResults@
    -- parameter of the request. If there are more bots available, the
    -- @nextToken@ field contains a token to the next page of results.
    botSummaries :: Prelude.Maybe [BotSummary],
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
-- 'nextToken', 'listBotsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBots@ operation request to get the next page of results.
--
-- 'botSummaries', 'listBotsResponse_botSummaries' - Summary information for the bots that meet the filter criteria specified
-- in the request. The length of the list is specified in the @maxResults@
-- parameter of the request. If there are more bots available, the
-- @nextToken@ field contains a token to the next page of results.
--
-- 'httpStatus', 'listBotsResponse_httpStatus' - The response's http status code.
newListBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotsResponse
newListBotsResponse pHttpStatus_ =
  ListBotsResponse'
    { nextToken = Prelude.Nothing,
      botSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBots@ operation request to get the next page of results.
listBotsResponse_nextToken :: Lens.Lens' ListBotsResponse (Prelude.Maybe Prelude.Text)
listBotsResponse_nextToken = Lens.lens (\ListBotsResponse' {nextToken} -> nextToken) (\s@ListBotsResponse' {} a -> s {nextToken = a} :: ListBotsResponse)

-- | Summary information for the bots that meet the filter criteria specified
-- in the request. The length of the list is specified in the @maxResults@
-- parameter of the request. If there are more bots available, the
-- @nextToken@ field contains a token to the next page of results.
listBotsResponse_botSummaries :: Lens.Lens' ListBotsResponse (Prelude.Maybe [BotSummary])
listBotsResponse_botSummaries = Lens.lens (\ListBotsResponse' {botSummaries} -> botSummaries) (\s@ListBotsResponse' {} a -> s {botSummaries = a} :: ListBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBotsResponse_httpStatus :: Lens.Lens' ListBotsResponse Prelude.Int
listBotsResponse_httpStatus = Lens.lens (\ListBotsResponse' {httpStatus} -> httpStatus) (\s@ListBotsResponse' {} a -> s {httpStatus = a} :: ListBotsResponse)

instance Prelude.NFData ListBotsResponse where
  rnf ListBotsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botSummaries
      `Prelude.seq` Prelude.rnf httpStatus

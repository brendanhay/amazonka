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
-- Module      : Amazonka.LexV2Models.ListBotLocales
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of locales for the specified bot.
module Amazonka.LexV2Models.ListBotLocales
  ( -- * Creating a Request
    ListBotLocales (..),
    newListBotLocales,

    -- * Request Lenses
    listBotLocales_nextToken,
    listBotLocales_filters,
    listBotLocales_sortBy,
    listBotLocales_maxResults,
    listBotLocales_botId,
    listBotLocales_botVersion,

    -- * Destructuring the Response
    ListBotLocalesResponse (..),
    newListBotLocalesResponse,

    -- * Response Lenses
    listBotLocalesResponse_nextToken,
    listBotLocalesResponse_botVersion,
    listBotLocalesResponse_botLocaleSummaries,
    listBotLocalesResponse_botId,
    listBotLocalesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBotLocales' smart constructor.
data ListBotLocales = ListBotLocales'
  { -- | If the response from the @ListBotLocales@ operation contains more
    -- results than specified in the @maxResults@ parameter, a token is
    -- returned in the response. Use that token as the @nextToken@ parameter to
    -- return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides the specification for a filter used to limit the response to
    -- only those locales that match the filter specification. You can only
    -- specify one filter and one value to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty BotLocaleFilter),
    -- | Specifies sorting parameters for the list of locales. You can sort by
    -- locale name in ascending or descending order.
    sortBy :: Prelude.Maybe BotLocaleSortBy,
    -- | The maximum number of aliases to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the bot to list locales for.
    botId :: Prelude.Text,
    -- | The version of the bot to list locales for.
    botVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotLocales' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBotLocales_nextToken' - If the response from the @ListBotLocales@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token as the @nextToken@ parameter to
-- return the next page of results.
--
-- 'filters', 'listBotLocales_filters' - Provides the specification for a filter used to limit the response to
-- only those locales that match the filter specification. You can only
-- specify one filter and one value to filter on.
--
-- 'sortBy', 'listBotLocales_sortBy' - Specifies sorting parameters for the list of locales. You can sort by
-- locale name in ascending or descending order.
--
-- 'maxResults', 'listBotLocales_maxResults' - The maximum number of aliases to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'botId', 'listBotLocales_botId' - The identifier of the bot to list locales for.
--
-- 'botVersion', 'listBotLocales_botVersion' - The version of the bot to list locales for.
newListBotLocales ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  ListBotLocales
newListBotLocales pBotId_ pBotVersion_ =
  ListBotLocales'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      botId = pBotId_,
      botVersion = pBotVersion_
    }

-- | If the response from the @ListBotLocales@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token as the @nextToken@ parameter to
-- return the next page of results.
listBotLocales_nextToken :: Lens.Lens' ListBotLocales (Prelude.Maybe Prelude.Text)
listBotLocales_nextToken = Lens.lens (\ListBotLocales' {nextToken} -> nextToken) (\s@ListBotLocales' {} a -> s {nextToken = a} :: ListBotLocales)

-- | Provides the specification for a filter used to limit the response to
-- only those locales that match the filter specification. You can only
-- specify one filter and one value to filter on.
listBotLocales_filters :: Lens.Lens' ListBotLocales (Prelude.Maybe (Prelude.NonEmpty BotLocaleFilter))
listBotLocales_filters = Lens.lens (\ListBotLocales' {filters} -> filters) (\s@ListBotLocales' {} a -> s {filters = a} :: ListBotLocales) Prelude.. Lens.mapping Lens.coerced

-- | Specifies sorting parameters for the list of locales. You can sort by
-- locale name in ascending or descending order.
listBotLocales_sortBy :: Lens.Lens' ListBotLocales (Prelude.Maybe BotLocaleSortBy)
listBotLocales_sortBy = Lens.lens (\ListBotLocales' {sortBy} -> sortBy) (\s@ListBotLocales' {} a -> s {sortBy = a} :: ListBotLocales)

-- | The maximum number of aliases to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listBotLocales_maxResults :: Lens.Lens' ListBotLocales (Prelude.Maybe Prelude.Natural)
listBotLocales_maxResults = Lens.lens (\ListBotLocales' {maxResults} -> maxResults) (\s@ListBotLocales' {} a -> s {maxResults = a} :: ListBotLocales)

-- | The identifier of the bot to list locales for.
listBotLocales_botId :: Lens.Lens' ListBotLocales Prelude.Text
listBotLocales_botId = Lens.lens (\ListBotLocales' {botId} -> botId) (\s@ListBotLocales' {} a -> s {botId = a} :: ListBotLocales)

-- | The version of the bot to list locales for.
listBotLocales_botVersion :: Lens.Lens' ListBotLocales Prelude.Text
listBotLocales_botVersion = Lens.lens (\ListBotLocales' {botVersion} -> botVersion) (\s@ListBotLocales' {} a -> s {botVersion = a} :: ListBotLocales)

instance Core.AWSRequest ListBotLocales where
  type
    AWSResponse ListBotLocales =
      ListBotLocalesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotLocalesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> ( x Core..?> "botLocaleSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBotLocales where
  hashWithSalt _salt ListBotLocales' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion

instance Prelude.NFData ListBotLocales where
  rnf ListBotLocales' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion

instance Core.ToHeaders ListBotLocales where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBotLocales where
  toJSON ListBotLocales' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListBotLocales where
  toPath ListBotLocales' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/"
      ]

instance Core.ToQuery ListBotLocales where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotLocalesResponse' smart constructor.
data ListBotLocalesResponse = ListBotLocalesResponse'
  { -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBotLocales@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListBotLocales@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the locales that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more locales
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    botLocaleSummaries :: Prelude.Maybe [BotLocaleSummary],
    -- | The identifier of the bot to list locales for.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotLocalesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBotLocalesResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBotLocales@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotLocales@ operation request to get the next page of results.
--
-- 'botVersion', 'listBotLocalesResponse_botVersion' - The version of the bot.
--
-- 'botLocaleSummaries', 'listBotLocalesResponse_botLocaleSummaries' - Summary information for the locales that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more locales
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'botId', 'listBotLocalesResponse_botId' - The identifier of the bot to list locales for.
--
-- 'httpStatus', 'listBotLocalesResponse_httpStatus' - The response's http status code.
newListBotLocalesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotLocalesResponse
newListBotLocalesResponse pHttpStatus_ =
  ListBotLocalesResponse'
    { nextToken =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botLocaleSummaries = Prelude.Nothing,
      botId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBotLocales@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotLocales@ operation request to get the next page of results.
listBotLocalesResponse_nextToken :: Lens.Lens' ListBotLocalesResponse (Prelude.Maybe Prelude.Text)
listBotLocalesResponse_nextToken = Lens.lens (\ListBotLocalesResponse' {nextToken} -> nextToken) (\s@ListBotLocalesResponse' {} a -> s {nextToken = a} :: ListBotLocalesResponse)

-- | The version of the bot.
listBotLocalesResponse_botVersion :: Lens.Lens' ListBotLocalesResponse (Prelude.Maybe Prelude.Text)
listBotLocalesResponse_botVersion = Lens.lens (\ListBotLocalesResponse' {botVersion} -> botVersion) (\s@ListBotLocalesResponse' {} a -> s {botVersion = a} :: ListBotLocalesResponse)

-- | Summary information for the locales that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more locales
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listBotLocalesResponse_botLocaleSummaries :: Lens.Lens' ListBotLocalesResponse (Prelude.Maybe [BotLocaleSummary])
listBotLocalesResponse_botLocaleSummaries = Lens.lens (\ListBotLocalesResponse' {botLocaleSummaries} -> botLocaleSummaries) (\s@ListBotLocalesResponse' {} a -> s {botLocaleSummaries = a} :: ListBotLocalesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the bot to list locales for.
listBotLocalesResponse_botId :: Lens.Lens' ListBotLocalesResponse (Prelude.Maybe Prelude.Text)
listBotLocalesResponse_botId = Lens.lens (\ListBotLocalesResponse' {botId} -> botId) (\s@ListBotLocalesResponse' {} a -> s {botId = a} :: ListBotLocalesResponse)

-- | The response's http status code.
listBotLocalesResponse_httpStatus :: Lens.Lens' ListBotLocalesResponse Prelude.Int
listBotLocalesResponse_httpStatus = Lens.lens (\ListBotLocalesResponse' {httpStatus} -> httpStatus) (\s@ListBotLocalesResponse' {} a -> s {httpStatus = a} :: ListBotLocalesResponse)

instance Prelude.NFData ListBotLocalesResponse where
  rnf ListBotLocalesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf botLocaleSummaries
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.LexV2Models.ListIntents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of intents that meet the specified criteria.
module Network.AWS.LexV2Models.ListIntents
  ( -- * Creating a Request
    ListIntents (..),
    newListIntents,

    -- * Request Lenses
    listIntents_filters,
    listIntents_nextToken,
    listIntents_maxResults,
    listIntents_sortBy,
    listIntents_botId,
    listIntents_botVersion,
    listIntents_localeId,

    -- * Destructuring the Response
    ListIntentsResponse (..),
    newListIntentsResponse,

    -- * Response Lenses
    listIntentsResponse_botVersion,
    listIntentsResponse_nextToken,
    listIntentsResponse_botId,
    listIntentsResponse_localeId,
    listIntentsResponse_intentSummaries,
    listIntentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListIntents' smart constructor.
data ListIntents = ListIntents'
  { -- | Provides the specification of a filter used to limit the intents in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and only one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty IntentFilter),
    -- | If the response from the @ListIntents@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of intents to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Determines the sort order for the response from the @ListIntents@
    -- operation. You can choose to sort by the intent name or last updated
    -- date in either ascending or descending order.
    sortBy :: Prelude.Maybe IntentSortBy,
    -- | The unique identifier of the bot that contains the intent.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the intent.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the intents to list. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIntents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listIntents_filters' - Provides the specification of a filter used to limit the intents in the
-- response to only those that match the filter specification. You can only
-- specify one filter and only one string to filter on.
--
-- 'nextToken', 'listIntents_nextToken' - If the response from the @ListIntents@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'maxResults', 'listIntents_maxResults' - The maximum number of intents to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'sortBy', 'listIntents_sortBy' - Determines the sort order for the response from the @ListIntents@
-- operation. You can choose to sort by the intent name or last updated
-- date in either ascending or descending order.
--
-- 'botId', 'listIntents_botId' - The unique identifier of the bot that contains the intent.
--
-- 'botVersion', 'listIntents_botVersion' - The version of the bot that contains the intent.
--
-- 'localeId', 'listIntents_localeId' - The identifier of the language and locale of the intents to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newListIntents ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  ListIntents
newListIntents pBotId_ pBotVersion_ pLocaleId_ =
  ListIntents'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      botId = pBotId_,
      botVersion = pBotVersion_,
      localeId = pLocaleId_
    }

-- | Provides the specification of a filter used to limit the intents in the
-- response to only those that match the filter specification. You can only
-- specify one filter and only one string to filter on.
listIntents_filters :: Lens.Lens' ListIntents (Prelude.Maybe (Prelude.NonEmpty IntentFilter))
listIntents_filters = Lens.lens (\ListIntents' {filters} -> filters) (\s@ListIntents' {} a -> s {filters = a} :: ListIntents) Prelude.. Lens.mapping Lens.coerced

-- | If the response from the @ListIntents@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listIntents_nextToken :: Lens.Lens' ListIntents (Prelude.Maybe Prelude.Text)
listIntents_nextToken = Lens.lens (\ListIntents' {nextToken} -> nextToken) (\s@ListIntents' {} a -> s {nextToken = a} :: ListIntents)

-- | The maximum number of intents to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listIntents_maxResults :: Lens.Lens' ListIntents (Prelude.Maybe Prelude.Natural)
listIntents_maxResults = Lens.lens (\ListIntents' {maxResults} -> maxResults) (\s@ListIntents' {} a -> s {maxResults = a} :: ListIntents)

-- | Determines the sort order for the response from the @ListIntents@
-- operation. You can choose to sort by the intent name or last updated
-- date in either ascending or descending order.
listIntents_sortBy :: Lens.Lens' ListIntents (Prelude.Maybe IntentSortBy)
listIntents_sortBy = Lens.lens (\ListIntents' {sortBy} -> sortBy) (\s@ListIntents' {} a -> s {sortBy = a} :: ListIntents)

-- | The unique identifier of the bot that contains the intent.
listIntents_botId :: Lens.Lens' ListIntents Prelude.Text
listIntents_botId = Lens.lens (\ListIntents' {botId} -> botId) (\s@ListIntents' {} a -> s {botId = a} :: ListIntents)

-- | The version of the bot that contains the intent.
listIntents_botVersion :: Lens.Lens' ListIntents Prelude.Text
listIntents_botVersion = Lens.lens (\ListIntents' {botVersion} -> botVersion) (\s@ListIntents' {} a -> s {botVersion = a} :: ListIntents)

-- | The identifier of the language and locale of the intents to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
listIntents_localeId :: Lens.Lens' ListIntents Prelude.Text
listIntents_localeId = Lens.lens (\ListIntents' {localeId} -> localeId) (\s@ListIntents' {} a -> s {localeId = a} :: ListIntents)

instance Core.AWSRequest ListIntents where
  type AWSResponse ListIntents = ListIntentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIntentsResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> ( x Core..?> "intentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIntents

instance Prelude.NFData ListIntents

instance Core.ToHeaders ListIntents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListIntents where
  toJSON ListIntents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListIntents where
  toPath ListIntents' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/"
      ]

instance Core.ToQuery ListIntents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIntentsResponse' smart constructor.
data ListIntentsResponse = ListIntentsResponse'
  { -- | The version of the bot that contains the intent.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListIntents@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListIntents@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the intent.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the intents in the list.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the intents that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more intents
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    intentSummaries :: Prelude.Maybe [IntentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIntentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botVersion', 'listIntentsResponse_botVersion' - The version of the bot that contains the intent.
--
-- 'nextToken', 'listIntentsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListIntents@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListIntents@ operation request to get the next page of results.
--
-- 'botId', 'listIntentsResponse_botId' - The identifier of the bot that contains the intent.
--
-- 'localeId', 'listIntentsResponse_localeId' - The language and locale of the intents in the list.
--
-- 'intentSummaries', 'listIntentsResponse_intentSummaries' - Summary information for the intents that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more intents
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'httpStatus', 'listIntentsResponse_httpStatus' - The response's http status code.
newListIntentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIntentsResponse
newListIntentsResponse pHttpStatus_ =
  ListIntentsResponse'
    { botVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      intentSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that contains the intent.
listIntentsResponse_botVersion :: Lens.Lens' ListIntentsResponse (Prelude.Maybe Prelude.Text)
listIntentsResponse_botVersion = Lens.lens (\ListIntentsResponse' {botVersion} -> botVersion) (\s@ListIntentsResponse' {} a -> s {botVersion = a} :: ListIntentsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListIntents@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListIntents@ operation request to get the next page of results.
listIntentsResponse_nextToken :: Lens.Lens' ListIntentsResponse (Prelude.Maybe Prelude.Text)
listIntentsResponse_nextToken = Lens.lens (\ListIntentsResponse' {nextToken} -> nextToken) (\s@ListIntentsResponse' {} a -> s {nextToken = a} :: ListIntentsResponse)

-- | The identifier of the bot that contains the intent.
listIntentsResponse_botId :: Lens.Lens' ListIntentsResponse (Prelude.Maybe Prelude.Text)
listIntentsResponse_botId = Lens.lens (\ListIntentsResponse' {botId} -> botId) (\s@ListIntentsResponse' {} a -> s {botId = a} :: ListIntentsResponse)

-- | The language and locale of the intents in the list.
listIntentsResponse_localeId :: Lens.Lens' ListIntentsResponse (Prelude.Maybe Prelude.Text)
listIntentsResponse_localeId = Lens.lens (\ListIntentsResponse' {localeId} -> localeId) (\s@ListIntentsResponse' {} a -> s {localeId = a} :: ListIntentsResponse)

-- | Summary information for the intents that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more intents
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listIntentsResponse_intentSummaries :: Lens.Lens' ListIntentsResponse (Prelude.Maybe [IntentSummary])
listIntentsResponse_intentSummaries = Lens.lens (\ListIntentsResponse' {intentSummaries} -> intentSummaries) (\s@ListIntentsResponse' {} a -> s {intentSummaries = a} :: ListIntentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listIntentsResponse_httpStatus :: Lens.Lens' ListIntentsResponse Prelude.Int
listIntentsResponse_httpStatus = Lens.lens (\ListIntentsResponse' {httpStatus} -> httpStatus) (\s@ListIntentsResponse' {} a -> s {httpStatus = a} :: ListIntentsResponse)

instance Prelude.NFData ListIntentsResponse

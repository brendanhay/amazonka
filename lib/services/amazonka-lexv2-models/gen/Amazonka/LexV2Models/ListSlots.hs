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
-- Module      : Amazonka.LexV2Models.ListSlots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of slots that match the specified criteria.
module Amazonka.LexV2Models.ListSlots
  ( -- * Creating a Request
    ListSlots (..),
    newListSlots,

    -- * Request Lenses
    listSlots_nextToken,
    listSlots_filters,
    listSlots_sortBy,
    listSlots_maxResults,
    listSlots_botId,
    listSlots_botVersion,
    listSlots_localeId,
    listSlots_intentId,

    -- * Destructuring the Response
    ListSlotsResponse (..),
    newListSlotsResponse,

    -- * Response Lenses
    listSlotsResponse_slotSummaries,
    listSlotsResponse_nextToken,
    listSlotsResponse_botVersion,
    listSlotsResponse_localeId,
    listSlotsResponse_botId,
    listSlotsResponse_intentId,
    listSlotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSlots' smart constructor.
data ListSlots = ListSlots'
  { -- | If the response from the @ListSlots@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides the specification of a filter used to limit the slots in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and only one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty SlotFilter),
    -- | Determines the sort order for the response from the @ListSlots@
    -- operation. You can choose to sort by the slot name or last updated date
    -- in either ascending or descending order.
    sortBy :: Prelude.Maybe SlotSortBy,
    -- | The maximum number of slots to return in each page of results. If there
    -- are fewer results than the max page size, only the actual number of
    -- results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the bot that contains the slot.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the slot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the slots to list. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The unique identifier of the intent that contains the slot.
    intentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSlots_nextToken' - If the response from the @ListSlots@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'filters', 'listSlots_filters' - Provides the specification of a filter used to limit the slots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and only one string to filter on.
--
-- 'sortBy', 'listSlots_sortBy' - Determines the sort order for the response from the @ListSlots@
-- operation. You can choose to sort by the slot name or last updated date
-- in either ascending or descending order.
--
-- 'maxResults', 'listSlots_maxResults' - The maximum number of slots to return in each page of results. If there
-- are fewer results than the max page size, only the actual number of
-- results are returned.
--
-- 'botId', 'listSlots_botId' - The identifier of the bot that contains the slot.
--
-- 'botVersion', 'listSlots_botVersion' - The version of the bot that contains the slot.
--
-- 'localeId', 'listSlots_localeId' - The identifier of the language and locale of the slots to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'intentId', 'listSlots_intentId' - The unique identifier of the intent that contains the slot.
newListSlots ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'intentId'
  Prelude.Text ->
  ListSlots
newListSlots
  pBotId_
  pBotVersion_
  pLocaleId_
  pIntentId_ =
    ListSlots'
      { nextToken = Prelude.Nothing,
        filters = Prelude.Nothing,
        sortBy = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        intentId = pIntentId_
      }

-- | If the response from the @ListSlots@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listSlots_nextToken :: Lens.Lens' ListSlots (Prelude.Maybe Prelude.Text)
listSlots_nextToken = Lens.lens (\ListSlots' {nextToken} -> nextToken) (\s@ListSlots' {} a -> s {nextToken = a} :: ListSlots)

-- | Provides the specification of a filter used to limit the slots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and only one string to filter on.
listSlots_filters :: Lens.Lens' ListSlots (Prelude.Maybe (Prelude.NonEmpty SlotFilter))
listSlots_filters = Lens.lens (\ListSlots' {filters} -> filters) (\s@ListSlots' {} a -> s {filters = a} :: ListSlots) Prelude.. Lens.mapping Lens.coerced

-- | Determines the sort order for the response from the @ListSlots@
-- operation. You can choose to sort by the slot name or last updated date
-- in either ascending or descending order.
listSlots_sortBy :: Lens.Lens' ListSlots (Prelude.Maybe SlotSortBy)
listSlots_sortBy = Lens.lens (\ListSlots' {sortBy} -> sortBy) (\s@ListSlots' {} a -> s {sortBy = a} :: ListSlots)

-- | The maximum number of slots to return in each page of results. If there
-- are fewer results than the max page size, only the actual number of
-- results are returned.
listSlots_maxResults :: Lens.Lens' ListSlots (Prelude.Maybe Prelude.Natural)
listSlots_maxResults = Lens.lens (\ListSlots' {maxResults} -> maxResults) (\s@ListSlots' {} a -> s {maxResults = a} :: ListSlots)

-- | The identifier of the bot that contains the slot.
listSlots_botId :: Lens.Lens' ListSlots Prelude.Text
listSlots_botId = Lens.lens (\ListSlots' {botId} -> botId) (\s@ListSlots' {} a -> s {botId = a} :: ListSlots)

-- | The version of the bot that contains the slot.
listSlots_botVersion :: Lens.Lens' ListSlots Prelude.Text
listSlots_botVersion = Lens.lens (\ListSlots' {botVersion} -> botVersion) (\s@ListSlots' {} a -> s {botVersion = a} :: ListSlots)

-- | The identifier of the language and locale of the slots to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
listSlots_localeId :: Lens.Lens' ListSlots Prelude.Text
listSlots_localeId = Lens.lens (\ListSlots' {localeId} -> localeId) (\s@ListSlots' {} a -> s {localeId = a} :: ListSlots)

-- | The unique identifier of the intent that contains the slot.
listSlots_intentId :: Lens.Lens' ListSlots Prelude.Text
listSlots_intentId = Lens.lens (\ListSlots' {intentId} -> intentId) (\s@ListSlots' {} a -> s {intentId = a} :: ListSlots)

instance Core.AWSRequest ListSlots where
  type AWSResponse ListSlots = ListSlotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSlotsResponse'
            Prelude.<$> (x Core..?> "slotSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "intentId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSlots where
  hashWithSalt _salt ListSlots' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` intentId

instance Prelude.NFData ListSlots where
  rnf ListSlots' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf intentId

instance Core.ToHeaders ListSlots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSlots where
  toJSON ListSlots' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListSlots where
  toPath ListSlots' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/intents/",
        Core.toBS intentId,
        "/slots/"
      ]

instance Core.ToQuery ListSlots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSlotsResponse' smart constructor.
data ListSlotsResponse = ListSlotsResponse'
  { -- | Summary information for the slots that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more slots
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    slotSummaries :: Prelude.Maybe [SlotSummary],
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListSlots@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListSlots@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that contains the slots.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The language and locale of the slots in the list.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the slots.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the intent that contains the slots.
    intentId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotSummaries', 'listSlotsResponse_slotSummaries' - Summary information for the slots that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slots
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'nextToken', 'listSlotsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListSlots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlots@ operation request to get the next page of results.
--
-- 'botVersion', 'listSlotsResponse_botVersion' - The version of the bot that contains the slots.
--
-- 'localeId', 'listSlotsResponse_localeId' - The language and locale of the slots in the list.
--
-- 'botId', 'listSlotsResponse_botId' - The identifier of the bot that contains the slots.
--
-- 'intentId', 'listSlotsResponse_intentId' - The identifier of the intent that contains the slots.
--
-- 'httpStatus', 'listSlotsResponse_httpStatus' - The response's http status code.
newListSlotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSlotsResponse
newListSlotsResponse pHttpStatus_ =
  ListSlotsResponse'
    { slotSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      localeId = Prelude.Nothing,
      botId = Prelude.Nothing,
      intentId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for the slots that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slots
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listSlotsResponse_slotSummaries :: Lens.Lens' ListSlotsResponse (Prelude.Maybe [SlotSummary])
listSlotsResponse_slotSummaries = Lens.lens (\ListSlotsResponse' {slotSummaries} -> slotSummaries) (\s@ListSlotsResponse' {} a -> s {slotSummaries = a} :: ListSlotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates whether there are more results to return in a
-- response to the @ListSlots@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlots@ operation request to get the next page of results.
listSlotsResponse_nextToken :: Lens.Lens' ListSlotsResponse (Prelude.Maybe Prelude.Text)
listSlotsResponse_nextToken = Lens.lens (\ListSlotsResponse' {nextToken} -> nextToken) (\s@ListSlotsResponse' {} a -> s {nextToken = a} :: ListSlotsResponse)

-- | The version of the bot that contains the slots.
listSlotsResponse_botVersion :: Lens.Lens' ListSlotsResponse (Prelude.Maybe Prelude.Text)
listSlotsResponse_botVersion = Lens.lens (\ListSlotsResponse' {botVersion} -> botVersion) (\s@ListSlotsResponse' {} a -> s {botVersion = a} :: ListSlotsResponse)

-- | The language and locale of the slots in the list.
listSlotsResponse_localeId :: Lens.Lens' ListSlotsResponse (Prelude.Maybe Prelude.Text)
listSlotsResponse_localeId = Lens.lens (\ListSlotsResponse' {localeId} -> localeId) (\s@ListSlotsResponse' {} a -> s {localeId = a} :: ListSlotsResponse)

-- | The identifier of the bot that contains the slots.
listSlotsResponse_botId :: Lens.Lens' ListSlotsResponse (Prelude.Maybe Prelude.Text)
listSlotsResponse_botId = Lens.lens (\ListSlotsResponse' {botId} -> botId) (\s@ListSlotsResponse' {} a -> s {botId = a} :: ListSlotsResponse)

-- | The identifier of the intent that contains the slots.
listSlotsResponse_intentId :: Lens.Lens' ListSlotsResponse (Prelude.Maybe Prelude.Text)
listSlotsResponse_intentId = Lens.lens (\ListSlotsResponse' {intentId} -> intentId) (\s@ListSlotsResponse' {} a -> s {intentId = a} :: ListSlotsResponse)

-- | The response's http status code.
listSlotsResponse_httpStatus :: Lens.Lens' ListSlotsResponse Prelude.Int
listSlotsResponse_httpStatus = Lens.lens (\ListSlotsResponse' {httpStatus} -> httpStatus) (\s@ListSlotsResponse' {} a -> s {httpStatus = a} :: ListSlotsResponse)

instance Prelude.NFData ListSlotsResponse where
  rnf ListSlotsResponse' {..} =
    Prelude.rnf slotSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf intentId
      `Prelude.seq` Prelude.rnf httpStatus

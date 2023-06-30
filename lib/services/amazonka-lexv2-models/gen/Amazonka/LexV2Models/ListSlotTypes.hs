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
-- Module      : Amazonka.LexV2Models.ListSlotTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of slot types that match the specified criteria.
module Amazonka.LexV2Models.ListSlotTypes
  ( -- * Creating a Request
    ListSlotTypes (..),
    newListSlotTypes,

    -- * Request Lenses
    listSlotTypes_filters,
    listSlotTypes_maxResults,
    listSlotTypes_nextToken,
    listSlotTypes_sortBy,
    listSlotTypes_botId,
    listSlotTypes_botVersion,
    listSlotTypes_localeId,

    -- * Destructuring the Response
    ListSlotTypesResponse (..),
    newListSlotTypesResponse,

    -- * Response Lenses
    listSlotTypesResponse_botId,
    listSlotTypesResponse_botVersion,
    listSlotTypesResponse_localeId,
    listSlotTypesResponse_nextToken,
    listSlotTypesResponse_slotTypeSummaries,
    listSlotTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSlotTypes' smart constructor.
data ListSlotTypes = ListSlotTypes'
  { -- | Provides the specification of a filter used to limit the slot types in
    -- the response to only those that match the filter specification. You can
    -- only specify one filter and only one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty SlotTypeFilter),
    -- | The maximum number of slot types to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListSlotTypes@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Determines the sort order for the response from the @ListSlotTypes@
    -- operation. You can choose to sort by the slot type name or last updated
    -- date in either ascending or descending order.
    sortBy :: Prelude.Maybe SlotTypeSortBy,
    -- | The unique identifier of the bot that contains the slot types.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the slot type.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale of the slot types to list. The
    -- string must match one of the supported locales. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlotTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listSlotTypes_filters' - Provides the specification of a filter used to limit the slot types in
-- the response to only those that match the filter specification. You can
-- only specify one filter and only one string to filter on.
--
-- 'maxResults', 'listSlotTypes_maxResults' - The maximum number of slot types to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'nextToken', 'listSlotTypes_nextToken' - If the response from the @ListSlotTypes@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'sortBy', 'listSlotTypes_sortBy' - Determines the sort order for the response from the @ListSlotTypes@
-- operation. You can choose to sort by the slot type name or last updated
-- date in either ascending or descending order.
--
-- 'botId', 'listSlotTypes_botId' - The unique identifier of the bot that contains the slot types.
--
-- 'botVersion', 'listSlotTypes_botVersion' - The version of the bot that contains the slot type.
--
-- 'localeId', 'listSlotTypes_localeId' - The identifier of the language and locale of the slot types to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newListSlotTypes ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  ListSlotTypes
newListSlotTypes pBotId_ pBotVersion_ pLocaleId_ =
  ListSlotTypes'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      botId = pBotId_,
      botVersion = pBotVersion_,
      localeId = pLocaleId_
    }

-- | Provides the specification of a filter used to limit the slot types in
-- the response to only those that match the filter specification. You can
-- only specify one filter and only one string to filter on.
listSlotTypes_filters :: Lens.Lens' ListSlotTypes (Prelude.Maybe (Prelude.NonEmpty SlotTypeFilter))
listSlotTypes_filters = Lens.lens (\ListSlotTypes' {filters} -> filters) (\s@ListSlotTypes' {} a -> s {filters = a} :: ListSlotTypes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of slot types to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listSlotTypes_maxResults :: Lens.Lens' ListSlotTypes (Prelude.Maybe Prelude.Natural)
listSlotTypes_maxResults = Lens.lens (\ListSlotTypes' {maxResults} -> maxResults) (\s@ListSlotTypes' {} a -> s {maxResults = a} :: ListSlotTypes)

-- | If the response from the @ListSlotTypes@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listSlotTypes_nextToken :: Lens.Lens' ListSlotTypes (Prelude.Maybe Prelude.Text)
listSlotTypes_nextToken = Lens.lens (\ListSlotTypes' {nextToken} -> nextToken) (\s@ListSlotTypes' {} a -> s {nextToken = a} :: ListSlotTypes)

-- | Determines the sort order for the response from the @ListSlotTypes@
-- operation. You can choose to sort by the slot type name or last updated
-- date in either ascending or descending order.
listSlotTypes_sortBy :: Lens.Lens' ListSlotTypes (Prelude.Maybe SlotTypeSortBy)
listSlotTypes_sortBy = Lens.lens (\ListSlotTypes' {sortBy} -> sortBy) (\s@ListSlotTypes' {} a -> s {sortBy = a} :: ListSlotTypes)

-- | The unique identifier of the bot that contains the slot types.
listSlotTypes_botId :: Lens.Lens' ListSlotTypes Prelude.Text
listSlotTypes_botId = Lens.lens (\ListSlotTypes' {botId} -> botId) (\s@ListSlotTypes' {} a -> s {botId = a} :: ListSlotTypes)

-- | The version of the bot that contains the slot type.
listSlotTypes_botVersion :: Lens.Lens' ListSlotTypes Prelude.Text
listSlotTypes_botVersion = Lens.lens (\ListSlotTypes' {botVersion} -> botVersion) (\s@ListSlotTypes' {} a -> s {botVersion = a} :: ListSlotTypes)

-- | The identifier of the language and locale of the slot types to list. The
-- string must match one of the supported locales. For more information,
-- see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
listSlotTypes_localeId :: Lens.Lens' ListSlotTypes Prelude.Text
listSlotTypes_localeId = Lens.lens (\ListSlotTypes' {localeId} -> localeId) (\s@ListSlotTypes' {} a -> s {localeId = a} :: ListSlotTypes)

instance Core.AWSRequest ListSlotTypes where
  type
    AWSResponse ListSlotTypes =
      ListSlotTypesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSlotTypesResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "slotTypeSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSlotTypes where
  hashWithSalt _salt ListSlotTypes' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData ListSlotTypes where
  rnf ListSlotTypes' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToHeaders ListSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSlotTypes where
  toJSON ListSlotTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListSlotTypes where
  toPath ListSlotTypes' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/slottypes/"
      ]

instance Data.ToQuery ListSlotTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSlotTypesResponse' smart constructor.
data ListSlotTypesResponse = ListSlotTypesResponse'
  { -- | The identifier of the bot that contains the slot types.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that contains the slot types.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The language and local of the slot types in the list.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListSlotTypes@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the slot types that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more slot types
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    slotTypeSummaries :: Prelude.Maybe [SlotTypeSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlotTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listSlotTypesResponse_botId' - The identifier of the bot that contains the slot types.
--
-- 'botVersion', 'listSlotTypesResponse_botVersion' - The version of the bot that contains the slot types.
--
-- 'localeId', 'listSlotTypesResponse_localeId' - The language and local of the slot types in the list.
--
-- 'nextToken', 'listSlotTypesResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlotTypes@ operation request to get the next page of results.
--
-- 'slotTypeSummaries', 'listSlotTypesResponse_slotTypeSummaries' - Summary information for the slot types that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slot types
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'httpStatus', 'listSlotTypesResponse_httpStatus' - The response's http status code.
newListSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSlotTypesResponse
newListSlotTypesResponse pHttpStatus_ =
  ListSlotTypesResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      slotTypeSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that contains the slot types.
listSlotTypesResponse_botId :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_botId = Lens.lens (\ListSlotTypesResponse' {botId} -> botId) (\s@ListSlotTypesResponse' {} a -> s {botId = a} :: ListSlotTypesResponse)

-- | The version of the bot that contains the slot types.
listSlotTypesResponse_botVersion :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_botVersion = Lens.lens (\ListSlotTypesResponse' {botVersion} -> botVersion) (\s@ListSlotTypesResponse' {} a -> s {botVersion = a} :: ListSlotTypesResponse)

-- | The language and local of the slot types in the list.
listSlotTypesResponse_localeId :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_localeId = Lens.lens (\ListSlotTypesResponse' {localeId} -> localeId) (\s@ListSlotTypesResponse' {} a -> s {localeId = a} :: ListSlotTypesResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlotTypes@ operation request to get the next page of results.
listSlotTypesResponse_nextToken :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_nextToken = Lens.lens (\ListSlotTypesResponse' {nextToken} -> nextToken) (\s@ListSlotTypesResponse' {} a -> s {nextToken = a} :: ListSlotTypesResponse)

-- | Summary information for the slot types that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slot types
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listSlotTypesResponse_slotTypeSummaries :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe [SlotTypeSummary])
listSlotTypesResponse_slotTypeSummaries = Lens.lens (\ListSlotTypesResponse' {slotTypeSummaries} -> slotTypeSummaries) (\s@ListSlotTypesResponse' {} a -> s {slotTypeSummaries = a} :: ListSlotTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSlotTypesResponse_httpStatus :: Lens.Lens' ListSlotTypesResponse Prelude.Int
listSlotTypesResponse_httpStatus = Lens.lens (\ListSlotTypesResponse' {httpStatus} -> httpStatus) (\s@ListSlotTypesResponse' {} a -> s {httpStatus = a} :: ListSlotTypesResponse)

instance Prelude.NFData ListSlotTypesResponse where
  rnf ListSlotTypesResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf slotTypeSummaries
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.LexV2Models.ListSlotTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of slot types that match the specified criteria.
module Network.AWS.LexV2Models.ListSlotTypes
  ( -- * Creating a Request
    ListSlotTypes (..),
    newListSlotTypes,

    -- * Request Lenses
    listSlotTypes_filters,
    listSlotTypes_nextToken,
    listSlotTypes_maxResults,
    listSlotTypes_sortBy,
    listSlotTypes_botId,
    listSlotTypes_botVersion,
    listSlotTypes_localeId,

    -- * Destructuring the Response
    ListSlotTypesResponse (..),
    newListSlotTypesResponse,

    -- * Response Lenses
    listSlotTypesResponse_botVersion,
    listSlotTypesResponse_slotTypeSummaries,
    listSlotTypesResponse_nextToken,
    listSlotTypesResponse_botId,
    listSlotTypesResponse_localeId,
    listSlotTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSlotTypes' smart constructor.
data ListSlotTypes = ListSlotTypes'
  { -- | Provides the specification of a filter used to limit the slot types in
    -- the response to only those that match the filter specification. You can
    -- only specify one filter and only one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty SlotTypeFilter),
    -- | If the response from the @ListSlotTypes@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of slot types to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listSlotTypes_nextToken' - If the response from the @ListSlotTypes@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'maxResults', 'listSlotTypes_maxResults' - The maximum number of slot types to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
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
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
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

-- | If the response from the @ListSlotTypes@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listSlotTypes_nextToken :: Lens.Lens' ListSlotTypes (Prelude.Maybe Prelude.Text)
listSlotTypes_nextToken = Lens.lens (\ListSlotTypes' {nextToken} -> nextToken) (\s@ListSlotTypes' {} a -> s {nextToken = a} :: ListSlotTypes)

-- | The maximum number of slot types to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listSlotTypes_maxResults :: Lens.Lens' ListSlotTypes (Prelude.Maybe Prelude.Natural)
listSlotTypes_maxResults = Lens.lens (\ListSlotTypes' {maxResults} -> maxResults) (\s@ListSlotTypes' {} a -> s {maxResults = a} :: ListSlotTypes)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSlotTypesResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> ( x Core..?> "slotTypeSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSlotTypes

instance Prelude.NFData ListSlotTypes

instance Core.ToHeaders ListSlotTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSlotTypes where
  toJSON ListSlotTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListSlotTypes where
  toPath ListSlotTypes' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/slottypes/"
      ]

instance Core.ToQuery ListSlotTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSlotTypesResponse' smart constructor.
data ListSlotTypesResponse = ListSlotTypesResponse'
  { -- | The version of the bot that contains the slot types.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the slot types that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more slot types
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    slotTypeSummaries :: Prelude.Maybe [SlotTypeSummary],
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListSlotTypes@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that contains the slot types.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The language and local of the slot types in the list.
    localeId :: Prelude.Maybe Prelude.Text,
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
-- 'botVersion', 'listSlotTypesResponse_botVersion' - The version of the bot that contains the slot types.
--
-- 'slotTypeSummaries', 'listSlotTypesResponse_slotTypeSummaries' - Summary information for the slot types that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slot types
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'nextToken', 'listSlotTypesResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlotTypes@ operation request to get the next page of results.
--
-- 'botId', 'listSlotTypesResponse_botId' - The identifier of the bot that contains the slot types.
--
-- 'localeId', 'listSlotTypesResponse_localeId' - The language and local of the slot types in the list.
--
-- 'httpStatus', 'listSlotTypesResponse_httpStatus' - The response's http status code.
newListSlotTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSlotTypesResponse
newListSlotTypesResponse pHttpStatus_ =
  ListSlotTypesResponse'
    { botVersion =
        Prelude.Nothing,
      slotTypeSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      localeId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that contains the slot types.
listSlotTypesResponse_botVersion :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_botVersion = Lens.lens (\ListSlotTypesResponse' {botVersion} -> botVersion) (\s@ListSlotTypesResponse' {} a -> s {botVersion = a} :: ListSlotTypesResponse)

-- | Summary information for the slot types that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more slot types
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listSlotTypesResponse_slotTypeSummaries :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe [SlotTypeSummary])
listSlotTypesResponse_slotTypeSummaries = Lens.lens (\ListSlotTypesResponse' {slotTypeSummaries} -> slotTypeSummaries) (\s@ListSlotTypesResponse' {} a -> s {slotTypeSummaries = a} :: ListSlotTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates whether there are more results to return in a
-- response to the @ListSlotTypes@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListSlotTypes@ operation request to get the next page of results.
listSlotTypesResponse_nextToken :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_nextToken = Lens.lens (\ListSlotTypesResponse' {nextToken} -> nextToken) (\s@ListSlotTypesResponse' {} a -> s {nextToken = a} :: ListSlotTypesResponse)

-- | The identifier of the bot that contains the slot types.
listSlotTypesResponse_botId :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_botId = Lens.lens (\ListSlotTypesResponse' {botId} -> botId) (\s@ListSlotTypesResponse' {} a -> s {botId = a} :: ListSlotTypesResponse)

-- | The language and local of the slot types in the list.
listSlotTypesResponse_localeId :: Lens.Lens' ListSlotTypesResponse (Prelude.Maybe Prelude.Text)
listSlotTypesResponse_localeId = Lens.lens (\ListSlotTypesResponse' {localeId} -> localeId) (\s@ListSlotTypesResponse' {} a -> s {localeId = a} :: ListSlotTypesResponse)

-- | The response's http status code.
listSlotTypesResponse_httpStatus :: Lens.Lens' ListSlotTypesResponse Prelude.Int
listSlotTypesResponse_httpStatus = Lens.lens (\ListSlotTypesResponse' {httpStatus} -> httpStatus) (\s@ListSlotTypesResponse' {} a -> s {httpStatus = a} :: ListSlotTypesResponse)

instance Prelude.NFData ListSlotTypesResponse

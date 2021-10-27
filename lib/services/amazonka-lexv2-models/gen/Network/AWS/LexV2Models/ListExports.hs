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
-- Module      : Network.AWS.LexV2Models.ListExports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the exports for a bot or bot locale. Exports are kept in the list
-- for 7 days.
module Network.AWS.LexV2Models.ListExports
  ( -- * Creating a Request
    ListExports (..),
    newListExports,

    -- * Request Lenses
    listExports_filters,
    listExports_botVersion,
    listExports_nextToken,
    listExports_botId,
    listExports_maxResults,
    listExports_sortBy,

    -- * Destructuring the Response
    ListExportsResponse (..),
    newListExportsResponse,

    -- * Response Lenses
    listExportsResponse_botVersion,
    listExportsResponse_exportSummaries,
    listExportsResponse_nextToken,
    listExportsResponse_botId,
    listExportsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListExports' smart constructor.
data ListExports = ListExports'
  { -- | Provides the specification of a filter used to limit the exports in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty ExportFilter),
    -- | The version of the bot to list exports for.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | If the response from the @ListExports@ operation contains more results
    -- that specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Amazon Lex assigned to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of exports to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Determines the field that the list of exports is sorted by. You can sort
    -- by the @LastUpdatedDateTime@ field in ascending or descending order.
    sortBy :: Prelude.Maybe ExportSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listExports_filters' - Provides the specification of a filter used to limit the exports in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'botVersion', 'listExports_botVersion' - The version of the bot to list exports for.
--
-- 'nextToken', 'listExports_nextToken' - If the response from the @ListExports@ operation contains more results
-- that specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'botId', 'listExports_botId' - The unique identifier that Amazon Lex assigned to the bot.
--
-- 'maxResults', 'listExports_maxResults' - The maximum number of exports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'sortBy', 'listExports_sortBy' - Determines the field that the list of exports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
newListExports ::
  ListExports
newListExports =
  ListExports'
    { filters = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Provides the specification of a filter used to limit the exports in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listExports_filters :: Lens.Lens' ListExports (Prelude.Maybe (Prelude.NonEmpty ExportFilter))
listExports_filters = Lens.lens (\ListExports' {filters} -> filters) (\s@ListExports' {} a -> s {filters = a} :: ListExports) Prelude.. Lens.mapping Lens.coerced

-- | The version of the bot to list exports for.
listExports_botVersion :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_botVersion = Lens.lens (\ListExports' {botVersion} -> botVersion) (\s@ListExports' {} a -> s {botVersion = a} :: ListExports)

-- | If the response from the @ListExports@ operation contains more results
-- that specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listExports_nextToken :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_nextToken = Lens.lens (\ListExports' {nextToken} -> nextToken) (\s@ListExports' {} a -> s {nextToken = a} :: ListExports)

-- | The unique identifier that Amazon Lex assigned to the bot.
listExports_botId :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_botId = Lens.lens (\ListExports' {botId} -> botId) (\s@ListExports' {} a -> s {botId = a} :: ListExports)

-- | The maximum number of exports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listExports_maxResults :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Natural)
listExports_maxResults = Lens.lens (\ListExports' {maxResults} -> maxResults) (\s@ListExports' {} a -> s {maxResults = a} :: ListExports)

-- | Determines the field that the list of exports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
listExports_sortBy :: Lens.Lens' ListExports (Prelude.Maybe ExportSortBy)
listExports_sortBy = Lens.lens (\ListExports' {sortBy} -> sortBy) (\s@ListExports' {} a -> s {sortBy = a} :: ListExports)

instance Core.AWSRequest ListExports where
  type AWSResponse ListExports = ListExportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExportsResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> ( x Core..?> "exportSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExports

instance Prelude.NFData ListExports

instance Core.ToHeaders ListExports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListExports where
  toJSON ListExports' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("botVersion" Core..=) Prelude.<$> botVersion,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("botId" Core..=) Prelude.<$> botId,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("sortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListExports where
  toPath = Prelude.const "/exports/"

instance Core.ToQuery ListExports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | The version of the bot that was exported.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the exports that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter. If there are more exports available, the
    -- @nextToken@ field contains a token to get the next page of results.
    exportSummaries :: Prelude.Maybe [ExportSummary],
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListExports@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListExports@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the bot by Amazon Lex.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botVersion', 'listExportsResponse_botVersion' - The version of the bot that was exported.
--
-- 'exportSummaries', 'listExportsResponse_exportSummaries' - Summary information for the exports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more exports available, the
-- @nextToken@ field contains a token to get the next page of results.
--
-- 'nextToken', 'listExportsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListExports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListExports@ operation request to get the next page of results.
--
-- 'botId', 'listExportsResponse_botId' - The unique identifier assigned to the bot by Amazon Lex.
--
-- 'httpStatus', 'listExportsResponse_httpStatus' - The response's http status code.
newListExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExportsResponse
newListExportsResponse pHttpStatus_ =
  ListExportsResponse'
    { botVersion = Prelude.Nothing,
      exportSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that was exported.
listExportsResponse_botVersion :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_botVersion = Lens.lens (\ListExportsResponse' {botVersion} -> botVersion) (\s@ListExportsResponse' {} a -> s {botVersion = a} :: ListExportsResponse)

-- | Summary information for the exports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more exports available, the
-- @nextToken@ field contains a token to get the next page of results.
listExportsResponse_exportSummaries :: Lens.Lens' ListExportsResponse (Prelude.Maybe [ExportSummary])
listExportsResponse_exportSummaries = Lens.lens (\ListExportsResponse' {exportSummaries} -> exportSummaries) (\s@ListExportsResponse' {} a -> s {exportSummaries = a} :: ListExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates whether there are more results to return in a
-- response to the @ListExports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListExports@ operation request to get the next page of results.
listExportsResponse_nextToken :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_nextToken = Lens.lens (\ListExportsResponse' {nextToken} -> nextToken) (\s@ListExportsResponse' {} a -> s {nextToken = a} :: ListExportsResponse)

-- | The unique identifier assigned to the bot by Amazon Lex.
listExportsResponse_botId :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_botId = Lens.lens (\ListExportsResponse' {botId} -> botId) (\s@ListExportsResponse' {} a -> s {botId = a} :: ListExportsResponse)

-- | The response's http status code.
listExportsResponse_httpStatus :: Lens.Lens' ListExportsResponse Prelude.Int
listExportsResponse_httpStatus = Lens.lens (\ListExportsResponse' {httpStatus} -> httpStatus) (\s@ListExportsResponse' {} a -> s {httpStatus = a} :: ListExportsResponse)

instance Prelude.NFData ListExportsResponse

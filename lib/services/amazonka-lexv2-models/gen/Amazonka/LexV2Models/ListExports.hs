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
-- Module      : Amazonka.LexV2Models.ListExports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the exports for a bot, bot locale, or custom vocabulary. Exports
-- are kept in the list for 7 days.
module Amazonka.LexV2Models.ListExports
  ( -- * Creating a Request
    ListExports (..),
    newListExports,

    -- * Request Lenses
    listExports_botId,
    listExports_botVersion,
    listExports_filters,
    listExports_localeId,
    listExports_maxResults,
    listExports_nextToken,
    listExports_sortBy,

    -- * Destructuring the Response
    ListExportsResponse (..),
    newListExportsResponse,

    -- * Response Lenses
    listExportsResponse_botId,
    listExportsResponse_botVersion,
    listExportsResponse_exportSummaries,
    listExportsResponse_localeId,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExports' smart constructor.
data ListExports = ListExports'
  { -- | The unique identifier that Amazon Lex assigned to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot to list exports for.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Provides the specification of a filter used to limit the exports in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty ExportFilter),
    -- | Specifies the resources that should be exported. If you don\'t specify a
    -- resource type in the @filters@ parameter, both bot locales and custom
    -- vocabularies are exported.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of exports to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListExports@ operation contains more results
    -- that specified in the @maxResults@ parameter, a token is returned in the
    -- response.
    --
    -- Use the returned token in the @nextToken@ parameter of a @ListExports@
    -- request to return the next page of results. For a complete set of
    -- results, call the @ListExports@ operation until the @nextToken@ returned
    -- in the response is null.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'botId', 'listExports_botId' - The unique identifier that Amazon Lex assigned to the bot.
--
-- 'botVersion', 'listExports_botVersion' - The version of the bot to list exports for.
--
-- 'filters', 'listExports_filters' - Provides the specification of a filter used to limit the exports in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'localeId', 'listExports_localeId' - Specifies the resources that should be exported. If you don\'t specify a
-- resource type in the @filters@ parameter, both bot locales and custom
-- vocabularies are exported.
--
-- 'maxResults', 'listExports_maxResults' - The maximum number of exports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'nextToken', 'listExports_nextToken' - If the response from the @ListExports@ operation contains more results
-- that specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListExports@
-- request to return the next page of results. For a complete set of
-- results, call the @ListExports@ operation until the @nextToken@ returned
-- in the response is null.
--
-- 'sortBy', 'listExports_sortBy' - Determines the field that the list of exports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
newListExports ::
  ListExports
newListExports =
  ListExports'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      filters = Prelude.Nothing,
      localeId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The unique identifier that Amazon Lex assigned to the bot.
listExports_botId :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_botId = Lens.lens (\ListExports' {botId} -> botId) (\s@ListExports' {} a -> s {botId = a} :: ListExports)

-- | The version of the bot to list exports for.
listExports_botVersion :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_botVersion = Lens.lens (\ListExports' {botVersion} -> botVersion) (\s@ListExports' {} a -> s {botVersion = a} :: ListExports)

-- | Provides the specification of a filter used to limit the exports in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listExports_filters :: Lens.Lens' ListExports (Prelude.Maybe (Prelude.NonEmpty ExportFilter))
listExports_filters = Lens.lens (\ListExports' {filters} -> filters) (\s@ListExports' {} a -> s {filters = a} :: ListExports) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the resources that should be exported. If you don\'t specify a
-- resource type in the @filters@ parameter, both bot locales and custom
-- vocabularies are exported.
listExports_localeId :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_localeId = Lens.lens (\ListExports' {localeId} -> localeId) (\s@ListExports' {} a -> s {localeId = a} :: ListExports)

-- | The maximum number of exports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listExports_maxResults :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Natural)
listExports_maxResults = Lens.lens (\ListExports' {maxResults} -> maxResults) (\s@ListExports' {} a -> s {maxResults = a} :: ListExports)

-- | If the response from the @ListExports@ operation contains more results
-- that specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListExports@
-- request to return the next page of results. For a complete set of
-- results, call the @ListExports@ operation until the @nextToken@ returned
-- in the response is null.
listExports_nextToken :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_nextToken = Lens.lens (\ListExports' {nextToken} -> nextToken) (\s@ListExports' {} a -> s {nextToken = a} :: ListExports)

-- | Determines the field that the list of exports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
listExports_sortBy :: Lens.Lens' ListExports (Prelude.Maybe ExportSortBy)
listExports_sortBy = Lens.lens (\ListExports' {sortBy} -> sortBy) (\s@ListExports' {} a -> s {sortBy = a} :: ListExports)

instance Core.AWSRequest ListExports where
  type AWSResponse ListExports = ListExportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExportsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> ( x Data..?> "exportSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExports where
  hashWithSalt _salt ListExports' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData ListExports where
  rnf ListExports' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToHeaders ListExports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExports where
  toJSON ListExports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botId" Data..=) Prelude.<$> botId,
            ("botVersion" Data..=) Prelude.<$> botVersion,
            ("filters" Data..=) Prelude.<$> filters,
            ("localeId" Data..=) Prelude.<$> localeId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListExports where
  toPath = Prelude.const "/exports/"

instance Data.ToQuery ListExports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | The unique identifier assigned to the bot by Amazon Lex.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that was exported.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the exports that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter. If there are more exports available, the
    -- @nextToken@ field contains a token to get the next page of results.
    exportSummaries :: Prelude.Maybe [ExportSummary],
    -- | The locale specified in the request.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListExports@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListExports@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'botId', 'listExportsResponse_botId' - The unique identifier assigned to the bot by Amazon Lex.
--
-- 'botVersion', 'listExportsResponse_botVersion' - The version of the bot that was exported.
--
-- 'exportSummaries', 'listExportsResponse_exportSummaries' - Summary information for the exports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more exports available, the
-- @nextToken@ field contains a token to get the next page of results.
--
-- 'localeId', 'listExportsResponse_localeId' - The locale specified in the request.
--
-- 'nextToken', 'listExportsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListExports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListExports@ operation request to get the next page of results.
--
-- 'httpStatus', 'listExportsResponse_httpStatus' - The response's http status code.
newListExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExportsResponse
newListExportsResponse pHttpStatus_ =
  ListExportsResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      exportSummaries = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier assigned to the bot by Amazon Lex.
listExportsResponse_botId :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_botId = Lens.lens (\ListExportsResponse' {botId} -> botId) (\s@ListExportsResponse' {} a -> s {botId = a} :: ListExportsResponse)

-- | The version of the bot that was exported.
listExportsResponse_botVersion :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_botVersion = Lens.lens (\ListExportsResponse' {botVersion} -> botVersion) (\s@ListExportsResponse' {} a -> s {botVersion = a} :: ListExportsResponse)

-- | Summary information for the exports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more exports available, the
-- @nextToken@ field contains a token to get the next page of results.
listExportsResponse_exportSummaries :: Lens.Lens' ListExportsResponse (Prelude.Maybe [ExportSummary])
listExportsResponse_exportSummaries = Lens.lens (\ListExportsResponse' {exportSummaries} -> exportSummaries) (\s@ListExportsResponse' {} a -> s {exportSummaries = a} :: ListExportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The locale specified in the request.
listExportsResponse_localeId :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_localeId = Lens.lens (\ListExportsResponse' {localeId} -> localeId) (\s@ListExportsResponse' {} a -> s {localeId = a} :: ListExportsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListExports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListExports@ operation request to get the next page of results.
listExportsResponse_nextToken :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_nextToken = Lens.lens (\ListExportsResponse' {nextToken} -> nextToken) (\s@ListExportsResponse' {} a -> s {nextToken = a} :: ListExportsResponse)

-- | The response's http status code.
listExportsResponse_httpStatus :: Lens.Lens' ListExportsResponse Prelude.Int
listExportsResponse_httpStatus = Lens.lens (\ListExportsResponse' {httpStatus} -> httpStatus) (\s@ListExportsResponse' {} a -> s {httpStatus = a} :: ListExportsResponse)

instance Prelude.NFData ListExportsResponse where
  rnf ListExportsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf exportSummaries
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

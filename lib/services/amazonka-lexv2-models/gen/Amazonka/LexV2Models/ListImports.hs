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
-- Module      : Amazonka.LexV2Models.ListImports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the imports for a bot, bot locale, or custom vocabulary. Imports
-- are kept in the list for 7 days.
module Amazonka.LexV2Models.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_botId,
    listImports_botVersion,
    listImports_filters,
    listImports_localeId,
    listImports_maxResults,
    listImports_nextToken,
    listImports_sortBy,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_botId,
    listImportsResponse_botVersion,
    listImportsResponse_importSummaries,
    listImportsResponse_localeId,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | The unique identifier that Amazon Lex assigned to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot to list imports for.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Provides the specification of a filter used to limit the bots in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty ImportFilter),
    -- | Specifies the locale that should be present in the list. If you don\'t
    -- specify a resource type in the @filters@ parameter, the list contains
    -- both bot locales and custom vocabularies.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of imports to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListImports@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response.
    --
    -- Use the returned token in the @nextToken@ parameter of a @ListImports@
    -- request to return the next page of results. For a complete set of
    -- results, call the @ListImports@ operation until the @nextToken@ returned
    -- in the response is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Determines the field that the list of imports is sorted by. You can sort
    -- by the @LastUpdatedDateTime@ field in ascending or descending order.
    sortBy :: Prelude.Maybe ImportSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listImports_botId' - The unique identifier that Amazon Lex assigned to the bot.
--
-- 'botVersion', 'listImports_botVersion' - The version of the bot to list imports for.
--
-- 'filters', 'listImports_filters' - Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'localeId', 'listImports_localeId' - Specifies the locale that should be present in the list. If you don\'t
-- specify a resource type in the @filters@ parameter, the list contains
-- both bot locales and custom vocabularies.
--
-- 'maxResults', 'listImports_maxResults' - The maximum number of imports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'nextToken', 'listImports_nextToken' - If the response from the @ListImports@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListImports@
-- request to return the next page of results. For a complete set of
-- results, call the @ListImports@ operation until the @nextToken@ returned
-- in the response is null.
--
-- 'sortBy', 'listImports_sortBy' - Determines the field that the list of imports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
newListImports ::
  ListImports
newListImports =
  ListImports'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      filters = Prelude.Nothing,
      localeId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | The unique identifier that Amazon Lex assigned to the bot.
listImports_botId :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_botId = Lens.lens (\ListImports' {botId} -> botId) (\s@ListImports' {} a -> s {botId = a} :: ListImports)

-- | The version of the bot to list imports for.
listImports_botVersion :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_botVersion = Lens.lens (\ListImports' {botVersion} -> botVersion) (\s@ListImports' {} a -> s {botVersion = a} :: ListImports)

-- | Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listImports_filters :: Lens.Lens' ListImports (Prelude.Maybe (Prelude.NonEmpty ImportFilter))
listImports_filters = Lens.lens (\ListImports' {filters} -> filters) (\s@ListImports' {} a -> s {filters = a} :: ListImports) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the locale that should be present in the list. If you don\'t
-- specify a resource type in the @filters@ parameter, the list contains
-- both bot locales and custom vocabularies.
listImports_localeId :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_localeId = Lens.lens (\ListImports' {localeId} -> localeId) (\s@ListImports' {} a -> s {localeId = a} :: ListImports)

-- | The maximum number of imports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listImports_maxResults :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Natural)
listImports_maxResults = Lens.lens (\ListImports' {maxResults} -> maxResults) (\s@ListImports' {} a -> s {maxResults = a} :: ListImports)

-- | If the response from the @ListImports@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response.
--
-- Use the returned token in the @nextToken@ parameter of a @ListImports@
-- request to return the next page of results. For a complete set of
-- results, call the @ListImports@ operation until the @nextToken@ returned
-- in the response is null.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | Determines the field that the list of imports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
listImports_sortBy :: Lens.Lens' ListImports (Prelude.Maybe ImportSortBy)
listImports_sortBy = Lens.lens (\ListImports' {sortBy} -> sortBy) (\s@ListImports' {} a -> s {sortBy = a} :: ListImports)

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> ( x Data..?> "importSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports where
  hashWithSalt _salt ListImports' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData ListImports where
  rnf ListImports' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToHeaders ListImports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImports where
  toJSON ListImports' {..} =
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

instance Data.ToPath ListImports where
  toPath = Prelude.const "/imports/"

instance Data.ToQuery ListImports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | The unique identifier assigned by Amazon Lex to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that was imported. It will always be @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the imports that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter. If there are more imports available, the
    -- @nextToken@ field contains a token to get the next page of results.
    importSummaries :: Prelude.Maybe [ImportSummary],
    -- | The locale specified in the request.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListImports@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListImports@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listImportsResponse_botId' - The unique identifier assigned by Amazon Lex to the bot.
--
-- 'botVersion', 'listImportsResponse_botVersion' - The version of the bot that was imported. It will always be @DRAFT@.
--
-- 'importSummaries', 'listImportsResponse_importSummaries' - Summary information for the imports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more imports available, the
-- @nextToken@ field contains a token to get the next page of results.
--
-- 'localeId', 'listImportsResponse_localeId' - The locale specified in the request.
--
-- 'nextToken', 'listImportsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListImports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListImports@ operation request to get the next page of results.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      importSummaries = Prelude.Nothing,
      localeId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier assigned by Amazon Lex to the bot.
listImportsResponse_botId :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_botId = Lens.lens (\ListImportsResponse' {botId} -> botId) (\s@ListImportsResponse' {} a -> s {botId = a} :: ListImportsResponse)

-- | The version of the bot that was imported. It will always be @DRAFT@.
listImportsResponse_botVersion :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_botVersion = Lens.lens (\ListImportsResponse' {botVersion} -> botVersion) (\s@ListImportsResponse' {} a -> s {botVersion = a} :: ListImportsResponse)

-- | Summary information for the imports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more imports available, the
-- @nextToken@ field contains a token to get the next page of results.
listImportsResponse_importSummaries :: Lens.Lens' ListImportsResponse (Prelude.Maybe [ImportSummary])
listImportsResponse_importSummaries = Lens.lens (\ListImportsResponse' {importSummaries} -> importSummaries) (\s@ListImportsResponse' {} a -> s {importSummaries = a} :: ListImportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The locale specified in the request.
listImportsResponse_localeId :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_localeId = Lens.lens (\ListImportsResponse' {localeId} -> localeId) (\s@ListImportsResponse' {} a -> s {localeId = a} :: ListImportsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListImports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListImports@ operation request to get the next page of results.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse where
  rnf ListImportsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf importSummaries
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

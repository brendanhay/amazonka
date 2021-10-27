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
-- Module      : Network.AWS.LexV2Models.ListImports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the imports for a bot or bot locale. Imports are kept in the list
-- for 7 days.
module Network.AWS.LexV2Models.ListImports
  ( -- * Creating a Request
    ListImports (..),
    newListImports,

    -- * Request Lenses
    listImports_filters,
    listImports_botVersion,
    listImports_nextToken,
    listImports_botId,
    listImports_maxResults,
    listImports_sortBy,

    -- * Destructuring the Response
    ListImportsResponse (..),
    newListImportsResponse,

    -- * Response Lenses
    listImportsResponse_botVersion,
    listImportsResponse_nextToken,
    listImportsResponse_botId,
    listImportsResponse_importSummaries,
    listImportsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListImports' smart constructor.
data ListImports = ListImports'
  { -- | Provides the specification of a filter used to limit the bots in the
    -- response to only those that match the filter specification. You can only
    -- specify one filter and one string to filter on.
    filters :: Prelude.Maybe (Prelude.NonEmpty ImportFilter),
    -- | The version of the bot to list imports for.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | If the response from the @ListImports@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Amazon Lex assigned to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of imports to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'filters', 'listImports_filters' - Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
--
-- 'botVersion', 'listImports_botVersion' - The version of the bot to list imports for.
--
-- 'nextToken', 'listImports_nextToken' - If the response from the @ListImports@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'botId', 'listImports_botId' - The unique identifier that Amazon Lex assigned to the bot.
--
-- 'maxResults', 'listImports_maxResults' - The maximum number of imports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'sortBy', 'listImports_sortBy' - Determines the field that the list of imports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
newListImports ::
  ListImports
newListImports =
  ListImports'
    { filters = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Provides the specification of a filter used to limit the bots in the
-- response to only those that match the filter specification. You can only
-- specify one filter and one string to filter on.
listImports_filters :: Lens.Lens' ListImports (Prelude.Maybe (Prelude.NonEmpty ImportFilter))
listImports_filters = Lens.lens (\ListImports' {filters} -> filters) (\s@ListImports' {} a -> s {filters = a} :: ListImports) Prelude.. Lens.mapping Lens.coerced

-- | The version of the bot to list imports for.
listImports_botVersion :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_botVersion = Lens.lens (\ListImports' {botVersion} -> botVersion) (\s@ListImports' {} a -> s {botVersion = a} :: ListImports)

-- | If the response from the @ListImports@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listImports_nextToken :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_nextToken = Lens.lens (\ListImports' {nextToken} -> nextToken) (\s@ListImports' {} a -> s {nextToken = a} :: ListImports)

-- | The unique identifier that Amazon Lex assigned to the bot.
listImports_botId :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Text)
listImports_botId = Lens.lens (\ListImports' {botId} -> botId) (\s@ListImports' {} a -> s {botId = a} :: ListImports)

-- | The maximum number of imports to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listImports_maxResults :: Lens.Lens' ListImports (Prelude.Maybe Prelude.Natural)
listImports_maxResults = Lens.lens (\ListImports' {maxResults} -> maxResults) (\s@ListImports' {} a -> s {maxResults = a} :: ListImports)

-- | Determines the field that the list of imports is sorted by. You can sort
-- by the @LastUpdatedDateTime@ field in ascending or descending order.
listImports_sortBy :: Lens.Lens' ListImports (Prelude.Maybe ImportSortBy)
listImports_sortBy = Lens.lens (\ListImports' {sortBy} -> sortBy) (\s@ListImports' {} a -> s {sortBy = a} :: ListImports)

instance Core.AWSRequest ListImports where
  type AWSResponse ListImports = ListImportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImportsResponse'
            Prelude.<$> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> ( x Core..?> "importSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImports

instance Prelude.NFData ListImports

instance Core.ToHeaders ListImports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImports where
  toJSON ListImports' {..} =
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

instance Core.ToPath ListImports where
  toPath = Prelude.const "/imports/"

instance Core.ToQuery ListImports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImportsResponse' smart constructor.
data ListImportsResponse = ListImportsResponse'
  { -- | The version of the bot that was imported. It will always be @DRAFT@.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListImports@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListImports@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned by Amazon Lex to the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the imports that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter. If there are more imports available, the
    -- @nextToken@ field contains a token to get the next page of results.
    importSummaries :: Prelude.Maybe [ImportSummary],
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
-- 'botVersion', 'listImportsResponse_botVersion' - The version of the bot that was imported. It will always be @DRAFT@.
--
-- 'nextToken', 'listImportsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListImports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListImports@ operation request to get the next page of results.
--
-- 'botId', 'listImportsResponse_botId' - The unique identifier assigned by Amazon Lex to the bot.
--
-- 'importSummaries', 'listImportsResponse_importSummaries' - Summary information for the imports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more imports available, the
-- @nextToken@ field contains a token to get the next page of results.
--
-- 'httpStatus', 'listImportsResponse_httpStatus' - The response's http status code.
newListImportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImportsResponse
newListImportsResponse pHttpStatus_ =
  ListImportsResponse'
    { botVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = Prelude.Nothing,
      importSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that was imported. It will always be @DRAFT@.
listImportsResponse_botVersion :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_botVersion = Lens.lens (\ListImportsResponse' {botVersion} -> botVersion) (\s@ListImportsResponse' {} a -> s {botVersion = a} :: ListImportsResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListImports@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListImports@ operation request to get the next page of results.
listImportsResponse_nextToken :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_nextToken = Lens.lens (\ListImportsResponse' {nextToken} -> nextToken) (\s@ListImportsResponse' {} a -> s {nextToken = a} :: ListImportsResponse)

-- | The unique identifier assigned by Amazon Lex to the bot.
listImportsResponse_botId :: Lens.Lens' ListImportsResponse (Prelude.Maybe Prelude.Text)
listImportsResponse_botId = Lens.lens (\ListImportsResponse' {botId} -> botId) (\s@ListImportsResponse' {} a -> s {botId = a} :: ListImportsResponse)

-- | Summary information for the imports that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter. If there are more imports available, the
-- @nextToken@ field contains a token to get the next page of results.
listImportsResponse_importSummaries :: Lens.Lens' ListImportsResponse (Prelude.Maybe [ImportSummary])
listImportsResponse_importSummaries = Lens.lens (\ListImportsResponse' {importSummaries} -> importSummaries) (\s@ListImportsResponse' {} a -> s {importSummaries = a} :: ListImportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImportsResponse_httpStatus :: Lens.Lens' ListImportsResponse Prelude.Int
listImportsResponse_httpStatus = Lens.lens (\ListImportsResponse' {httpStatus} -> httpStatus) (\s@ListImportsResponse' {} a -> s {httpStatus = a} :: ListImportsResponse)

instance Prelude.NFData ListImportsResponse

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
-- Module      : Amazonka.LexV2Models.ListBotVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of a bot.
--
-- The @ListBotVersions@ operation returns a summary of each version of a
-- bot. For example, if a bot has three numbered versions, the
-- @ListBotVersions@ operation returns for summaries, one for each numbered
-- version and one for the @DRAFT@ version.
--
-- The @ListBotVersions@ operation always returns at least one version, the
-- @DRAFT@ version.
module Amazonka.LexV2Models.ListBotVersions
  ( -- * Creating a Request
    ListBotVersions (..),
    newListBotVersions,

    -- * Request Lenses
    listBotVersions_maxResults,
    listBotVersions_nextToken,
    listBotVersions_sortBy,
    listBotVersions_botId,

    -- * Destructuring the Response
    ListBotVersionsResponse (..),
    newListBotVersionsResponse,

    -- * Response Lenses
    listBotVersionsResponse_botId,
    listBotVersionsResponse_botVersionSummaries,
    listBotVersionsResponse_nextToken,
    listBotVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBotVersions' smart constructor.
data ListBotVersions = ListBotVersions'
  { -- | The maximum number of versions to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response to the @ListBotVersion@ operation contains more results
    -- than specified in the @maxResults@ parameter, a token is returned in the
    -- response. Use that token in the @nextToken@ parameter to return the next
    -- page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies sorting parameters for the list of versions. You can specify
    -- that the list be sorted by version name in either ascending or
    -- descending order.
    sortBy :: Prelude.Maybe BotVersionSortBy,
    -- | The identifier of the bot to list versions for.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBotVersions_maxResults' - The maximum number of versions to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'nextToken', 'listBotVersions_nextToken' - If the response to the @ListBotVersion@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
--
-- 'sortBy', 'listBotVersions_sortBy' - Specifies sorting parameters for the list of versions. You can specify
-- that the list be sorted by version name in either ascending or
-- descending order.
--
-- 'botId', 'listBotVersions_botId' - The identifier of the bot to list versions for.
newListBotVersions ::
  -- | 'botId'
  Prelude.Text ->
  ListBotVersions
newListBotVersions pBotId_ =
  ListBotVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      botId = pBotId_
    }

-- | The maximum number of versions to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listBotVersions_maxResults :: Lens.Lens' ListBotVersions (Prelude.Maybe Prelude.Natural)
listBotVersions_maxResults = Lens.lens (\ListBotVersions' {maxResults} -> maxResults) (\s@ListBotVersions' {} a -> s {maxResults = a} :: ListBotVersions)

-- | If the response to the @ListBotVersion@ operation contains more results
-- than specified in the @maxResults@ parameter, a token is returned in the
-- response. Use that token in the @nextToken@ parameter to return the next
-- page of results.
listBotVersions_nextToken :: Lens.Lens' ListBotVersions (Prelude.Maybe Prelude.Text)
listBotVersions_nextToken = Lens.lens (\ListBotVersions' {nextToken} -> nextToken) (\s@ListBotVersions' {} a -> s {nextToken = a} :: ListBotVersions)

-- | Specifies sorting parameters for the list of versions. You can specify
-- that the list be sorted by version name in either ascending or
-- descending order.
listBotVersions_sortBy :: Lens.Lens' ListBotVersions (Prelude.Maybe BotVersionSortBy)
listBotVersions_sortBy = Lens.lens (\ListBotVersions' {sortBy} -> sortBy) (\s@ListBotVersions' {} a -> s {sortBy = a} :: ListBotVersions)

-- | The identifier of the bot to list versions for.
listBotVersions_botId :: Lens.Lens' ListBotVersions Prelude.Text
listBotVersions_botId = Lens.lens (\ListBotVersions' {botId} -> botId) (\s@ListBotVersions' {} a -> s {botId = a} :: ListBotVersions)

instance Core.AWSRequest ListBotVersions where
  type
    AWSResponse ListBotVersions =
      ListBotVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotVersionsResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> ( x Data..?> "botVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBotVersions where
  hashWithSalt _salt ListBotVersions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` botId

instance Prelude.NFData ListBotVersions where
  rnf ListBotVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders ListBotVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBotVersions where
  toJSON ListBotVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy
          ]
      )

instance Data.ToPath ListBotVersions where
  toPath ListBotVersions' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botId, "/botversions/"]

instance Data.ToQuery ListBotVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotVersionsResponse' smart constructor.
data ListBotVersionsResponse = ListBotVersionsResponse'
  { -- | The identifier of the bot to list versions for.
    botId :: Prelude.Maybe Prelude.Text,
    -- | Summary information for the bot versions that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more versions
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    botVersionSummaries :: Prelude.Maybe [BotVersionSummary],
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBotVersions@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListBotAliases@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'listBotVersionsResponse_botId' - The identifier of the bot to list versions for.
--
-- 'botVersionSummaries', 'listBotVersionsResponse_botVersionSummaries' - Summary information for the bot versions that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more versions
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'nextToken', 'listBotVersionsResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBotVersions@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotAliases@ operation request to get the next page of results.
--
-- 'httpStatus', 'listBotVersionsResponse_httpStatus' - The response's http status code.
newListBotVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotVersionsResponse
newListBotVersionsResponse pHttpStatus_ =
  ListBotVersionsResponse'
    { botId = Prelude.Nothing,
      botVersionSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot to list versions for.
listBotVersionsResponse_botId :: Lens.Lens' ListBotVersionsResponse (Prelude.Maybe Prelude.Text)
listBotVersionsResponse_botId = Lens.lens (\ListBotVersionsResponse' {botId} -> botId) (\s@ListBotVersionsResponse' {} a -> s {botId = a} :: ListBotVersionsResponse)

-- | Summary information for the bot versions that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more versions
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listBotVersionsResponse_botVersionSummaries :: Lens.Lens' ListBotVersionsResponse (Prelude.Maybe [BotVersionSummary])
listBotVersionsResponse_botVersionSummaries = Lens.lens (\ListBotVersionsResponse' {botVersionSummaries} -> botVersionSummaries) (\s@ListBotVersionsResponse' {} a -> s {botVersionSummaries = a} :: ListBotVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBotVersions@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotAliases@ operation request to get the next page of results.
listBotVersionsResponse_nextToken :: Lens.Lens' ListBotVersionsResponse (Prelude.Maybe Prelude.Text)
listBotVersionsResponse_nextToken = Lens.lens (\ListBotVersionsResponse' {nextToken} -> nextToken) (\s@ListBotVersionsResponse' {} a -> s {nextToken = a} :: ListBotVersionsResponse)

-- | The response's http status code.
listBotVersionsResponse_httpStatus :: Lens.Lens' ListBotVersionsResponse Prelude.Int
listBotVersionsResponse_httpStatus = Lens.lens (\ListBotVersionsResponse' {httpStatus} -> httpStatus) (\s@ListBotVersionsResponse' {} a -> s {httpStatus = a} :: ListBotVersionsResponse)

instance Prelude.NFData ListBotVersionsResponse where
  rnf ListBotVersionsResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

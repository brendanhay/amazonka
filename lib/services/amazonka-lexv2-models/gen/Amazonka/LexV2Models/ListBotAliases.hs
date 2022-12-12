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
-- Module      : Amazonka.LexV2Models.ListBotAliases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of aliases for the specified bot.
module Amazonka.LexV2Models.ListBotAliases
  ( -- * Creating a Request
    ListBotAliases (..),
    newListBotAliases,

    -- * Request Lenses
    listBotAliases_maxResults,
    listBotAliases_nextToken,
    listBotAliases_botId,

    -- * Destructuring the Response
    ListBotAliasesResponse (..),
    newListBotAliasesResponse,

    -- * Response Lenses
    listBotAliasesResponse_botAliasSummaries,
    listBotAliasesResponse_botId,
    listBotAliasesResponse_nextToken,
    listBotAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBotAliases' smart constructor.
data ListBotAliases = ListBotAliases'
  { -- | The maximum number of aliases to return in each page of results. If
    -- there are fewer results than the max page size, only the actual number
    -- of results are returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response from the @ListBotAliases@ operation contains more
    -- results than specified in the @maxResults@ parameter, a token is
    -- returned in the response. Use that token in the @nextToken@ parameter to
    -- return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot to list aliases for.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBotAliases_maxResults' - The maximum number of aliases to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
--
-- 'nextToken', 'listBotAliases_nextToken' - If the response from the @ListBotAliases@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token in the @nextToken@ parameter to
-- return the next page of results.
--
-- 'botId', 'listBotAliases_botId' - The identifier of the bot to list aliases for.
newListBotAliases ::
  -- | 'botId'
  Prelude.Text ->
  ListBotAliases
newListBotAliases pBotId_ =
  ListBotAliases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botId = pBotId_
    }

-- | The maximum number of aliases to return in each page of results. If
-- there are fewer results than the max page size, only the actual number
-- of results are returned.
listBotAliases_maxResults :: Lens.Lens' ListBotAliases (Prelude.Maybe Prelude.Natural)
listBotAliases_maxResults = Lens.lens (\ListBotAliases' {maxResults} -> maxResults) (\s@ListBotAliases' {} a -> s {maxResults = a} :: ListBotAliases)

-- | If the response from the @ListBotAliases@ operation contains more
-- results than specified in the @maxResults@ parameter, a token is
-- returned in the response. Use that token in the @nextToken@ parameter to
-- return the next page of results.
listBotAliases_nextToken :: Lens.Lens' ListBotAliases (Prelude.Maybe Prelude.Text)
listBotAliases_nextToken = Lens.lens (\ListBotAliases' {nextToken} -> nextToken) (\s@ListBotAliases' {} a -> s {nextToken = a} :: ListBotAliases)

-- | The identifier of the bot to list aliases for.
listBotAliases_botId :: Lens.Lens' ListBotAliases Prelude.Text
listBotAliases_botId = Lens.lens (\ListBotAliases' {botId} -> botId) (\s@ListBotAliases' {} a -> s {botId = a} :: ListBotAliases)

instance Core.AWSRequest ListBotAliases where
  type
    AWSResponse ListBotAliases =
      ListBotAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBotAliasesResponse'
            Prelude.<$> ( x Data..?> "botAliasSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBotAliases where
  hashWithSalt _salt ListBotAliases' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` botId

instance Prelude.NFData ListBotAliases where
  rnf ListBotAliases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders ListBotAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBotAliases where
  toJSON ListBotAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListBotAliases where
  toPath ListBotAliases' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botId, "/botaliases/"]

instance Data.ToQuery ListBotAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBotAliasesResponse' smart constructor.
data ListBotAliasesResponse = ListBotAliasesResponse'
  { -- | Summary information for the bot aliases that meet the filter criteria
    -- specified in the request. The length of the list is specified in the
    -- @maxResults@ parameter of the request. If there are more aliases
    -- available, the @nextToken@ field contains a token to get the next page
    -- of results.
    botAliasSummaries :: Prelude.Maybe [BotAliasSummary],
    -- | The identifier of the bot associated with the aliases.
    botId :: Prelude.Maybe Prelude.Text,
    -- | A token that indicates whether there are more results to return in a
    -- response to the @ListBotAliases@ operation. If the @nextToken@ field is
    -- present, you send the contents as the @nextToken@ parameter of a
    -- @ListBotAliases@ operation request to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBotAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasSummaries', 'listBotAliasesResponse_botAliasSummaries' - Summary information for the bot aliases that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more aliases
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
--
-- 'botId', 'listBotAliasesResponse_botId' - The identifier of the bot associated with the aliases.
--
-- 'nextToken', 'listBotAliasesResponse_nextToken' - A token that indicates whether there are more results to return in a
-- response to the @ListBotAliases@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotAliases@ operation request to get the next page of results.
--
-- 'httpStatus', 'listBotAliasesResponse_httpStatus' - The response's http status code.
newListBotAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBotAliasesResponse
newListBotAliasesResponse pHttpStatus_ =
  ListBotAliasesResponse'
    { botAliasSummaries =
        Prelude.Nothing,
      botId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for the bot aliases that meet the filter criteria
-- specified in the request. The length of the list is specified in the
-- @maxResults@ parameter of the request. If there are more aliases
-- available, the @nextToken@ field contains a token to get the next page
-- of results.
listBotAliasesResponse_botAliasSummaries :: Lens.Lens' ListBotAliasesResponse (Prelude.Maybe [BotAliasSummary])
listBotAliasesResponse_botAliasSummaries = Lens.lens (\ListBotAliasesResponse' {botAliasSummaries} -> botAliasSummaries) (\s@ListBotAliasesResponse' {} a -> s {botAliasSummaries = a} :: ListBotAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the bot associated with the aliases.
listBotAliasesResponse_botId :: Lens.Lens' ListBotAliasesResponse (Prelude.Maybe Prelude.Text)
listBotAliasesResponse_botId = Lens.lens (\ListBotAliasesResponse' {botId} -> botId) (\s@ListBotAliasesResponse' {} a -> s {botId = a} :: ListBotAliasesResponse)

-- | A token that indicates whether there are more results to return in a
-- response to the @ListBotAliases@ operation. If the @nextToken@ field is
-- present, you send the contents as the @nextToken@ parameter of a
-- @ListBotAliases@ operation request to get the next page of results.
listBotAliasesResponse_nextToken :: Lens.Lens' ListBotAliasesResponse (Prelude.Maybe Prelude.Text)
listBotAliasesResponse_nextToken = Lens.lens (\ListBotAliasesResponse' {nextToken} -> nextToken) (\s@ListBotAliasesResponse' {} a -> s {nextToken = a} :: ListBotAliasesResponse)

-- | The response's http status code.
listBotAliasesResponse_httpStatus :: Lens.Lens' ListBotAliasesResponse Prelude.Int
listBotAliasesResponse_httpStatus = Lens.lens (\ListBotAliasesResponse' {httpStatus} -> httpStatus) (\s@ListBotAliasesResponse' {} a -> s {httpStatus = a} :: ListBotAliasesResponse)

instance Prelude.NFData ListBotAliasesResponse where
  rnf ListBotAliasesResponse' {..} =
    Prelude.rnf botAliasSummaries
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

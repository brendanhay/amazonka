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
-- Module      : Amazonka.LexModels.GetBotAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of aliases for a specified Amazon Lex bot.
--
-- This operation requires permissions for the @lex:GetBotAliases@ action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetBotAliases
  ( -- * Creating a Request
    GetBotAliases (..),
    newGetBotAliases,

    -- * Request Lenses
    getBotAliases_maxResults,
    getBotAliases_nameContains,
    getBotAliases_nextToken,
    getBotAliases_botName,

    -- * Destructuring the Response
    GetBotAliasesResponse (..),
    newGetBotAliasesResponse,

    -- * Response Lenses
    getBotAliasesResponse_botAliases,
    getBotAliasesResponse_nextToken,
    getBotAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBotAliases' smart constructor.
data GetBotAliases = GetBotAliases'
  { -- | The maximum number of aliases to return in the response. The default is
    -- 50. .
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Substring to match in bot alias names. An alias will be returned if any
    -- part of its name matches the substring. For example, \"xyz\" matches
    -- both \"xyzabc\" and \"abcxyz.\"
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A pagination token for fetching the next page of aliases. If the
    -- response to this call is truncated, Amazon Lex returns a pagination
    -- token in the response. To fetch the next page of aliases, specify the
    -- pagination token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot.
    botName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getBotAliases_maxResults' - The maximum number of aliases to return in the response. The default is
-- 50. .
--
-- 'nameContains', 'getBotAliases_nameContains' - Substring to match in bot alias names. An alias will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
--
-- 'nextToken', 'getBotAliases_nextToken' - A pagination token for fetching the next page of aliases. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of aliases, specify the
-- pagination token in the next request.
--
-- 'botName', 'getBotAliases_botName' - The name of the bot.
newGetBotAliases ::
  -- | 'botName'
  Prelude.Text ->
  GetBotAliases
newGetBotAliases pBotName_ =
  GetBotAliases'
    { maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      botName = pBotName_
    }

-- | The maximum number of aliases to return in the response. The default is
-- 50. .
getBotAliases_maxResults :: Lens.Lens' GetBotAliases (Prelude.Maybe Prelude.Natural)
getBotAliases_maxResults = Lens.lens (\GetBotAliases' {maxResults} -> maxResults) (\s@GetBotAliases' {} a -> s {maxResults = a} :: GetBotAliases)

-- | Substring to match in bot alias names. An alias will be returned if any
-- part of its name matches the substring. For example, \"xyz\" matches
-- both \"xyzabc\" and \"abcxyz.\"
getBotAliases_nameContains :: Lens.Lens' GetBotAliases (Prelude.Maybe Prelude.Text)
getBotAliases_nameContains = Lens.lens (\GetBotAliases' {nameContains} -> nameContains) (\s@GetBotAliases' {} a -> s {nameContains = a} :: GetBotAliases)

-- | A pagination token for fetching the next page of aliases. If the
-- response to this call is truncated, Amazon Lex returns a pagination
-- token in the response. To fetch the next page of aliases, specify the
-- pagination token in the next request.
getBotAliases_nextToken :: Lens.Lens' GetBotAliases (Prelude.Maybe Prelude.Text)
getBotAliases_nextToken = Lens.lens (\GetBotAliases' {nextToken} -> nextToken) (\s@GetBotAliases' {} a -> s {nextToken = a} :: GetBotAliases)

-- | The name of the bot.
getBotAliases_botName :: Lens.Lens' GetBotAliases Prelude.Text
getBotAliases_botName = Lens.lens (\GetBotAliases' {botName} -> botName) (\s@GetBotAliases' {} a -> s {botName = a} :: GetBotAliases)

instance Core.AWSPager GetBotAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBotAliasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBotAliasesResponse_botAliases
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getBotAliases_nextToken
          Lens..~ rs
          Lens.^? getBotAliasesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetBotAliases where
  type
    AWSResponse GetBotAliases =
      GetBotAliasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotAliasesResponse'
            Prelude.<$> (x Data..?> "BotAliases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotAliases where
  hashWithSalt _salt GetBotAliases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` botName

instance Prelude.NFData GetBotAliases where
  rnf GetBotAliases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf botName

instance Data.ToHeaders GetBotAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBotAliases where
  toPath GetBotAliases' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botName, "/aliases/"]

instance Data.ToQuery GetBotAliases where
  toQuery GetBotAliases' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nameContains" Data.=: nameContains,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetBotAliasesResponse' smart constructor.
data GetBotAliasesResponse = GetBotAliasesResponse'
  { -- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
    botAliases :: Prelude.Maybe [BotAliasMetadata],
    -- | A pagination token for fetching next page of aliases. If the response to
    -- this call is truncated, Amazon Lex returns a pagination token in the
    -- response. To fetch the next page of aliases, specify the pagination
    -- token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliases', 'getBotAliasesResponse_botAliases' - An array of @BotAliasMetadata@ objects, each describing a bot alias.
--
-- 'nextToken', 'getBotAliasesResponse_nextToken' - A pagination token for fetching next page of aliases. If the response to
-- this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of aliases, specify the pagination
-- token in the next request.
--
-- 'httpStatus', 'getBotAliasesResponse_httpStatus' - The response's http status code.
newGetBotAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotAliasesResponse
newGetBotAliasesResponse pHttpStatus_ =
  GetBotAliasesResponse'
    { botAliases =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @BotAliasMetadata@ objects, each describing a bot alias.
getBotAliasesResponse_botAliases :: Lens.Lens' GetBotAliasesResponse (Prelude.Maybe [BotAliasMetadata])
getBotAliasesResponse_botAliases = Lens.lens (\GetBotAliasesResponse' {botAliases} -> botAliases) (\s@GetBotAliasesResponse' {} a -> s {botAliases = a} :: GetBotAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token for fetching next page of aliases. If the response to
-- this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of aliases, specify the pagination
-- token in the next request.
getBotAliasesResponse_nextToken :: Lens.Lens' GetBotAliasesResponse (Prelude.Maybe Prelude.Text)
getBotAliasesResponse_nextToken = Lens.lens (\GetBotAliasesResponse' {nextToken} -> nextToken) (\s@GetBotAliasesResponse' {} a -> s {nextToken = a} :: GetBotAliasesResponse)

-- | The response's http status code.
getBotAliasesResponse_httpStatus :: Lens.Lens' GetBotAliasesResponse Prelude.Int
getBotAliasesResponse_httpStatus = Lens.lens (\GetBotAliasesResponse' {httpStatus} -> httpStatus) (\s@GetBotAliasesResponse' {} a -> s {httpStatus = a} :: GetBotAliasesResponse)

instance Prelude.NFData GetBotAliasesResponse where
  rnf GetBotAliasesResponse' {..} =
    Prelude.rnf botAliases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

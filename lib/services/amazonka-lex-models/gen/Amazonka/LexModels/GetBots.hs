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
-- Module      : Amazonka.LexModels.GetBots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns bot information as follows:
--
-- -   If you provide the @nameContains@ field, the response includes
--     information for the @$LATEST@ version of all bots whose name
--     contains the specified string.
--
-- -   If you don\'t specify the @nameContains@ field, the operation
--     returns information about the @$LATEST@ version of all of your bots.
--
-- This operation requires permission for the @lex:GetBots@ action.
--
-- This operation returns paginated results.
module Amazonka.LexModels.GetBots
  ( -- * Creating a Request
    GetBots (..),
    newGetBots,

    -- * Request Lenses
    getBots_maxResults,
    getBots_nameContains,
    getBots_nextToken,

    -- * Destructuring the Response
    GetBotsResponse (..),
    newGetBotsResponse,

    -- * Response Lenses
    getBotsResponse_bots,
    getBotsResponse_nextToken,
    getBotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBots' smart constructor.
data GetBots = GetBots'
  { -- | The maximum number of bots to return in the response that the request
    -- will return. The default is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Substring to match in bot names. A bot will be returned if any part of
    -- its name matches the substring. For example, \"xyz\" matches both
    -- \"xyzabc\" and \"abcxyz.\"
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A pagination token that fetches the next page of bots. If the response
    -- to this call is truncated, Amazon Lex returns a pagination token in the
    -- response. To fetch the next page of bots, specify the pagination token
    -- in the next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getBots_maxResults' - The maximum number of bots to return in the response that the request
-- will return. The default is 10.
--
-- 'nameContains', 'getBots_nameContains' - Substring to match in bot names. A bot will be returned if any part of
-- its name matches the substring. For example, \"xyz\" matches both
-- \"xyzabc\" and \"abcxyz.\"
--
-- 'nextToken', 'getBots_nextToken' - A pagination token that fetches the next page of bots. If the response
-- to this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of bots, specify the pagination token
-- in the next request.
newGetBots ::
  GetBots
newGetBots =
  GetBots'
    { maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of bots to return in the response that the request
-- will return. The default is 10.
getBots_maxResults :: Lens.Lens' GetBots (Prelude.Maybe Prelude.Natural)
getBots_maxResults = Lens.lens (\GetBots' {maxResults} -> maxResults) (\s@GetBots' {} a -> s {maxResults = a} :: GetBots)

-- | Substring to match in bot names. A bot will be returned if any part of
-- its name matches the substring. For example, \"xyz\" matches both
-- \"xyzabc\" and \"abcxyz.\"
getBots_nameContains :: Lens.Lens' GetBots (Prelude.Maybe Prelude.Text)
getBots_nameContains = Lens.lens (\GetBots' {nameContains} -> nameContains) (\s@GetBots' {} a -> s {nameContains = a} :: GetBots)

-- | A pagination token that fetches the next page of bots. If the response
-- to this call is truncated, Amazon Lex returns a pagination token in the
-- response. To fetch the next page of bots, specify the pagination token
-- in the next request.
getBots_nextToken :: Lens.Lens' GetBots (Prelude.Maybe Prelude.Text)
getBots_nextToken = Lens.lens (\GetBots' {nextToken} -> nextToken) (\s@GetBots' {} a -> s {nextToken = a} :: GetBots)

instance Core.AWSPager GetBots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getBotsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getBotsResponse_bots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getBots_nextToken
              Lens..~ rs
              Lens.^? getBotsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetBots where
  type AWSResponse GetBots = GetBotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotsResponse'
            Prelude.<$> (x Data..?> "bots" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBots where
  hashWithSalt _salt GetBots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetBots where
  rnf GetBots' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nameContains `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders GetBots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBots where
  toPath = Prelude.const "/bots/"

instance Data.ToQuery GetBots where
  toQuery GetBots' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nameContains" Data.=: nameContains,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetBotsResponse' smart constructor.
data GetBotsResponse = GetBotsResponse'
  { -- | An array of @botMetadata@ objects, with one entry for each bot.
    bots :: Prelude.Maybe [BotMetadata],
    -- | If the response is truncated, it includes a pagination token that you
    -- can specify in your next request to fetch the next page of bots.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bots', 'getBotsResponse_bots' - An array of @botMetadata@ objects, with one entry for each bot.
--
-- 'nextToken', 'getBotsResponse_nextToken' - If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of bots.
--
-- 'httpStatus', 'getBotsResponse_httpStatus' - The response's http status code.
newGetBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotsResponse
newGetBotsResponse pHttpStatus_ =
  GetBotsResponse'
    { bots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @botMetadata@ objects, with one entry for each bot.
getBotsResponse_bots :: Lens.Lens' GetBotsResponse (Prelude.Maybe [BotMetadata])
getBotsResponse_bots = Lens.lens (\GetBotsResponse' {bots} -> bots) (\s@GetBotsResponse' {} a -> s {bots = a} :: GetBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, it includes a pagination token that you
-- can specify in your next request to fetch the next page of bots.
getBotsResponse_nextToken :: Lens.Lens' GetBotsResponse (Prelude.Maybe Prelude.Text)
getBotsResponse_nextToken = Lens.lens (\GetBotsResponse' {nextToken} -> nextToken) (\s@GetBotsResponse' {} a -> s {nextToken = a} :: GetBotsResponse)

-- | The response's http status code.
getBotsResponse_httpStatus :: Lens.Lens' GetBotsResponse Prelude.Int
getBotsResponse_httpStatus = Lens.lens (\GetBotsResponse' {httpStatus} -> httpStatus) (\s@GetBotsResponse' {} a -> s {httpStatus = a} :: GetBotsResponse)

instance Prelude.NFData GetBotsResponse where
  rnf GetBotsResponse' {..} =
    Prelude.rnf bots `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

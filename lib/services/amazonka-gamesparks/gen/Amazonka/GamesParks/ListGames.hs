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
-- Module      : Amazonka.GamesParks.ListGames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of games.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListGames
  ( -- * Creating a Request
    ListGames (..),
    newListGames,

    -- * Request Lenses
    listGames_maxResults,
    listGames_nextToken,

    -- * Destructuring the Response
    ListGamesResponse (..),
    newListGamesResponse,

    -- * Response Lenses
    listGamesResponse_games,
    listGamesResponse_nextToken,
    listGamesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGames' smart constructor.
data ListGames = ListGames'
  { -- | The maximum number of results to return.
    --
    -- Use this parameter with NextToken to get results as a set of sequential
    -- pages.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGames_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'nextToken', 'listGames_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
newListGames ::
  ListGames
newListGames =
  ListGames'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listGames_maxResults :: Lens.Lens' ListGames (Prelude.Maybe Prelude.Natural)
listGames_maxResults = Lens.lens (\ListGames' {maxResults} -> maxResults) (\s@ListGames' {} a -> s {maxResults = a} :: ListGames)

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listGames_nextToken :: Lens.Lens' ListGames (Prelude.Maybe Prelude.Text)
listGames_nextToken = Lens.lens (\ListGames' {nextToken} -> nextToken) (\s@ListGames' {} a -> s {nextToken = a} :: ListGames)

instance Core.AWSPager ListGames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGamesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGamesResponse_games
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listGames_nextToken
              Lens..~ rs
              Lens.^? listGamesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListGames where
  type AWSResponse ListGames = ListGamesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGamesResponse'
            Prelude.<$> (x Data..?> "Games" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGames where
  hashWithSalt _salt ListGames' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGames where
  rnf ListGames' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListGames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGames where
  toPath = Prelude.const "/game"

instance Data.ToQuery ListGames where
  toQuery ListGames' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGamesResponse' smart constructor.
data ListGamesResponse = ListGamesResponse'
  { -- | The list of games.
    games :: Prelude.Maybe [GameSummary],
    -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'games', 'listGamesResponse_games' - The list of games.
--
-- 'nextToken', 'listGamesResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'httpStatus', 'listGamesResponse_httpStatus' - The response's http status code.
newListGamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGamesResponse
newListGamesResponse pHttpStatus_ =
  ListGamesResponse'
    { games = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of games.
listGamesResponse_games :: Lens.Lens' ListGamesResponse (Prelude.Maybe [GameSummary])
listGamesResponse_games = Lens.lens (\ListGamesResponse' {games} -> games) (\s@ListGamesResponse' {} a -> s {games = a} :: ListGamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listGamesResponse_nextToken :: Lens.Lens' ListGamesResponse (Prelude.Maybe Prelude.Text)
listGamesResponse_nextToken = Lens.lens (\ListGamesResponse' {nextToken} -> nextToken) (\s@ListGamesResponse' {} a -> s {nextToken = a} :: ListGamesResponse)

-- | The response's http status code.
listGamesResponse_httpStatus :: Lens.Lens' ListGamesResponse Prelude.Int
listGamesResponse_httpStatus = Lens.lens (\ListGamesResponse' {httpStatus} -> httpStatus) (\s@ListGamesResponse' {} a -> s {httpStatus = a} :: ListGamesResponse)

instance Prelude.NFData ListGamesResponse where
  rnf ListGamesResponse' {..} =
    Prelude.rnf games `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

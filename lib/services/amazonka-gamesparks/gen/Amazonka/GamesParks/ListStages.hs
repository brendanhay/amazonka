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
-- Module      : Amazonka.GamesParks.ListStages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of stage summaries from the game.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListStages
  ( -- * Creating a Request
    ListStages (..),
    newListStages,

    -- * Request Lenses
    listStages_maxResults,
    listStages_nextToken,
    listStages_gameName,

    -- * Destructuring the Response
    ListStagesResponse (..),
    newListStagesResponse,

    -- * Response Lenses
    listStagesResponse_nextToken,
    listStagesResponse_stages,
    listStagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStages' smart constructor.
data ListStages = ListStages'
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStages_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'nextToken', 'listStages_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'gameName', 'listStages_gameName' - The name of the game.
newListStages ::
  -- | 'gameName'
  Prelude.Text ->
  ListStages
newListStages pGameName_ =
  ListStages'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      gameName = pGameName_
    }

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listStages_maxResults :: Lens.Lens' ListStages (Prelude.Maybe Prelude.Natural)
listStages_maxResults = Lens.lens (\ListStages' {maxResults} -> maxResults) (\s@ListStages' {} a -> s {maxResults = a} :: ListStages)

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listStages_nextToken :: Lens.Lens' ListStages (Prelude.Maybe Prelude.Text)
listStages_nextToken = Lens.lens (\ListStages' {nextToken} -> nextToken) (\s@ListStages' {} a -> s {nextToken = a} :: ListStages)

-- | The name of the game.
listStages_gameName :: Lens.Lens' ListStages Prelude.Text
listStages_gameName = Lens.lens (\ListStages' {gameName} -> gameName) (\s@ListStages' {} a -> s {gameName = a} :: ListStages)

instance Core.AWSPager ListStages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStagesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStagesResponse_stages Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStages_nextToken
          Lens..~ rs
          Lens.^? listStagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListStages where
  type AWSResponse ListStages = ListStagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStagesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Stages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStages where
  hashWithSalt _salt ListStages' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` gameName

instance Prelude.NFData ListStages where
  rnf ListStages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gameName

instance Data.ToHeaders ListStages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStages where
  toPath ListStages' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/stage"]

instance Data.ToQuery ListStages where
  toQuery ListStages' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListStagesResponse' smart constructor.
data ListStagesResponse = ListStagesResponse'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of stage summaries. You can use the stage names in the
    -- @UpdateStage@ and @GetStage@ actions.
    stages :: Prelude.Maybe [StageSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStagesResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'stages', 'listStagesResponse_stages' - A list of stage summaries. You can use the stage names in the
-- @UpdateStage@ and @GetStage@ actions.
--
-- 'httpStatus', 'listStagesResponse_httpStatus' - The response's http status code.
newListStagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStagesResponse
newListStagesResponse pHttpStatus_ =
  ListStagesResponse'
    { nextToken = Prelude.Nothing,
      stages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listStagesResponse_nextToken :: Lens.Lens' ListStagesResponse (Prelude.Maybe Prelude.Text)
listStagesResponse_nextToken = Lens.lens (\ListStagesResponse' {nextToken} -> nextToken) (\s@ListStagesResponse' {} a -> s {nextToken = a} :: ListStagesResponse)

-- | A list of stage summaries. You can use the stage names in the
-- @UpdateStage@ and @GetStage@ actions.
listStagesResponse_stages :: Lens.Lens' ListStagesResponse (Prelude.Maybe [StageSummary])
listStagesResponse_stages = Lens.lens (\ListStagesResponse' {stages} -> stages) (\s@ListStagesResponse' {} a -> s {stages = a} :: ListStagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStagesResponse_httpStatus :: Lens.Lens' ListStagesResponse Prelude.Int
listStagesResponse_httpStatus = Lens.lens (\ListStagesResponse' {httpStatus} -> httpStatus) (\s@ListStagesResponse' {} a -> s {httpStatus = a} :: ListStagesResponse)

instance Prelude.NFData ListStagesResponse where
  rnf ListStagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stages
      `Prelude.seq` Prelude.rnf httpStatus

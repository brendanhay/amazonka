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
-- Module      : Amazonka.GamesParks.ListStageDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of stage deployment summaries from the game.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListStageDeployments
  ( -- * Creating a Request
    ListStageDeployments (..),
    newListStageDeployments,

    -- * Request Lenses
    listStageDeployments_maxResults,
    listStageDeployments_nextToken,
    listStageDeployments_gameName,
    listStageDeployments_stageName,

    -- * Destructuring the Response
    ListStageDeploymentsResponse (..),
    newListStageDeploymentsResponse,

    -- * Response Lenses
    listStageDeploymentsResponse_nextToken,
    listStageDeploymentsResponse_stageDeployments,
    listStageDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStageDeployments' smart constructor.
data ListStageDeployments = ListStageDeployments'
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
    gameName :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStageDeployments_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'nextToken', 'listStageDeployments_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'gameName', 'listStageDeployments_gameName' - The name of the game.
--
-- 'stageName', 'listStageDeployments_stageName' - The name of the stage.
newListStageDeployments ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  ListStageDeployments
newListStageDeployments pGameName_ pStageName_ =
  ListStageDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      gameName = pGameName_,
      stageName = pStageName_
    }

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listStageDeployments_maxResults :: Lens.Lens' ListStageDeployments (Prelude.Maybe Prelude.Natural)
listStageDeployments_maxResults = Lens.lens (\ListStageDeployments' {maxResults} -> maxResults) (\s@ListStageDeployments' {} a -> s {maxResults = a} :: ListStageDeployments)

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listStageDeployments_nextToken :: Lens.Lens' ListStageDeployments (Prelude.Maybe Prelude.Text)
listStageDeployments_nextToken = Lens.lens (\ListStageDeployments' {nextToken} -> nextToken) (\s@ListStageDeployments' {} a -> s {nextToken = a} :: ListStageDeployments)

-- | The name of the game.
listStageDeployments_gameName :: Lens.Lens' ListStageDeployments Prelude.Text
listStageDeployments_gameName = Lens.lens (\ListStageDeployments' {gameName} -> gameName) (\s@ListStageDeployments' {} a -> s {gameName = a} :: ListStageDeployments)

-- | The name of the stage.
listStageDeployments_stageName :: Lens.Lens' ListStageDeployments Prelude.Text
listStageDeployments_stageName = Lens.lens (\ListStageDeployments' {stageName} -> stageName) (\s@ListStageDeployments' {} a -> s {stageName = a} :: ListStageDeployments)

instance Core.AWSPager ListStageDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStageDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStageDeploymentsResponse_stageDeployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStageDeployments_nextToken
          Lens..~ rs
          Lens.^? listStageDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStageDeployments where
  type
    AWSResponse ListStageDeployments =
      ListStageDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStageDeploymentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "StageDeployments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStageDeployments where
  hashWithSalt _salt ListStageDeployments' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData ListStageDeployments where
  rnf ListStageDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders ListStageDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStageDeployments where
  toPath ListStageDeployments' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName,
        "/deployments"
      ]

instance Data.ToQuery ListStageDeployments where
  toQuery ListStageDeployments' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListStageDeploymentsResponse' smart constructor.
data ListStageDeploymentsResponse = ListStageDeploymentsResponse'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of stage deployment summaries. You can use the deployment IDs in
    -- the @UpdateStageDeployment@ and @GetStageDeployment@ actions.
    stageDeployments :: Prelude.Maybe [StageDeploymentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStageDeploymentsResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'stageDeployments', 'listStageDeploymentsResponse_stageDeployments' - A list of stage deployment summaries. You can use the deployment IDs in
-- the @UpdateStageDeployment@ and @GetStageDeployment@ actions.
--
-- 'httpStatus', 'listStageDeploymentsResponse_httpStatus' - The response's http status code.
newListStageDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStageDeploymentsResponse
newListStageDeploymentsResponse pHttpStatus_ =
  ListStageDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      stageDeployments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listStageDeploymentsResponse_nextToken :: Lens.Lens' ListStageDeploymentsResponse (Prelude.Maybe Prelude.Text)
listStageDeploymentsResponse_nextToken = Lens.lens (\ListStageDeploymentsResponse' {nextToken} -> nextToken) (\s@ListStageDeploymentsResponse' {} a -> s {nextToken = a} :: ListStageDeploymentsResponse)

-- | A list of stage deployment summaries. You can use the deployment IDs in
-- the @UpdateStageDeployment@ and @GetStageDeployment@ actions.
listStageDeploymentsResponse_stageDeployments :: Lens.Lens' ListStageDeploymentsResponse (Prelude.Maybe [StageDeploymentSummary])
listStageDeploymentsResponse_stageDeployments = Lens.lens (\ListStageDeploymentsResponse' {stageDeployments} -> stageDeployments) (\s@ListStageDeploymentsResponse' {} a -> s {stageDeployments = a} :: ListStageDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStageDeploymentsResponse_httpStatus :: Lens.Lens' ListStageDeploymentsResponse Prelude.Int
listStageDeploymentsResponse_httpStatus = Lens.lens (\ListStageDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListStageDeploymentsResponse' {} a -> s {httpStatus = a} :: ListStageDeploymentsResponse)

instance Prelude.NFData ListStageDeploymentsResponse where
  rnf ListStageDeploymentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stageDeployments
      `Prelude.seq` Prelude.rnf httpStatus

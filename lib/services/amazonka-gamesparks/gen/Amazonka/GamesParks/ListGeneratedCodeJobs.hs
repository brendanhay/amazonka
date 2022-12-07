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
-- Module      : Amazonka.GamesParks.ListGeneratedCodeJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of code generation jobs for a snapshot.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListGeneratedCodeJobs
  ( -- * Creating a Request
    ListGeneratedCodeJobs (..),
    newListGeneratedCodeJobs,

    -- * Request Lenses
    listGeneratedCodeJobs_nextToken,
    listGeneratedCodeJobs_maxResults,
    listGeneratedCodeJobs_gameName,
    listGeneratedCodeJobs_snapshotId,

    -- * Destructuring the Response
    ListGeneratedCodeJobsResponse (..),
    newListGeneratedCodeJobsResponse,

    -- * Response Lenses
    listGeneratedCodeJobsResponse_nextToken,
    listGeneratedCodeJobsResponse_generatedCodeJobs,
    listGeneratedCodeJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGeneratedCodeJobs' smart constructor.
data ListGeneratedCodeJobs = ListGeneratedCodeJobs'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    --
    -- Use this parameter with NextToken to get results as a set of sequential
    -- pages.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the snapshot.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeneratedCodeJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGeneratedCodeJobs_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'maxResults', 'listGeneratedCodeJobs_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'gameName', 'listGeneratedCodeJobs_gameName' - The name of the game.
--
-- 'snapshotId', 'listGeneratedCodeJobs_snapshotId' - The identifier of the snapshot.
newListGeneratedCodeJobs ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  ListGeneratedCodeJobs
newListGeneratedCodeJobs pGameName_ pSnapshotId_ =
  ListGeneratedCodeJobs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      gameName = pGameName_,
      snapshotId = pSnapshotId_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listGeneratedCodeJobs_nextToken :: Lens.Lens' ListGeneratedCodeJobs (Prelude.Maybe Prelude.Text)
listGeneratedCodeJobs_nextToken = Lens.lens (\ListGeneratedCodeJobs' {nextToken} -> nextToken) (\s@ListGeneratedCodeJobs' {} a -> s {nextToken = a} :: ListGeneratedCodeJobs)

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listGeneratedCodeJobs_maxResults :: Lens.Lens' ListGeneratedCodeJobs (Prelude.Maybe Prelude.Natural)
listGeneratedCodeJobs_maxResults = Lens.lens (\ListGeneratedCodeJobs' {maxResults} -> maxResults) (\s@ListGeneratedCodeJobs' {} a -> s {maxResults = a} :: ListGeneratedCodeJobs)

-- | The name of the game.
listGeneratedCodeJobs_gameName :: Lens.Lens' ListGeneratedCodeJobs Prelude.Text
listGeneratedCodeJobs_gameName = Lens.lens (\ListGeneratedCodeJobs' {gameName} -> gameName) (\s@ListGeneratedCodeJobs' {} a -> s {gameName = a} :: ListGeneratedCodeJobs)

-- | The identifier of the snapshot.
listGeneratedCodeJobs_snapshotId :: Lens.Lens' ListGeneratedCodeJobs Prelude.Text
listGeneratedCodeJobs_snapshotId = Lens.lens (\ListGeneratedCodeJobs' {snapshotId} -> snapshotId) (\s@ListGeneratedCodeJobs' {} a -> s {snapshotId = a} :: ListGeneratedCodeJobs)

instance Core.AWSPager ListGeneratedCodeJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGeneratedCodeJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGeneratedCodeJobsResponse_generatedCodeJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGeneratedCodeJobs_nextToken
          Lens..~ rs
          Lens.^? listGeneratedCodeJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListGeneratedCodeJobs where
  type
    AWSResponse ListGeneratedCodeJobs =
      ListGeneratedCodeJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGeneratedCodeJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "GeneratedCodeJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGeneratedCodeJobs where
  hashWithSalt _salt ListGeneratedCodeJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData ListGeneratedCodeJobs where
  rnf ListGeneratedCodeJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToHeaders ListGeneratedCodeJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGeneratedCodeJobs where
  toPath ListGeneratedCodeJobs' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/snapshot/",
        Data.toBS snapshotId,
        "/generated-sdk-code-jobs"
      ]

instance Data.ToQuery ListGeneratedCodeJobs where
  toQuery ListGeneratedCodeJobs' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListGeneratedCodeJobsResponse' smart constructor.
data ListGeneratedCodeJobsResponse = ListGeneratedCodeJobsResponse'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of generated code jobs.
    generatedCodeJobs :: Prelude.Maybe [GeneratedCodeJobDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeneratedCodeJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGeneratedCodeJobsResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'generatedCodeJobs', 'listGeneratedCodeJobsResponse_generatedCodeJobs' - The list of generated code jobs.
--
-- 'httpStatus', 'listGeneratedCodeJobsResponse_httpStatus' - The response's http status code.
newListGeneratedCodeJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGeneratedCodeJobsResponse
newListGeneratedCodeJobsResponse pHttpStatus_ =
  ListGeneratedCodeJobsResponse'
    { nextToken =
        Prelude.Nothing,
      generatedCodeJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listGeneratedCodeJobsResponse_nextToken :: Lens.Lens' ListGeneratedCodeJobsResponse (Prelude.Maybe Prelude.Text)
listGeneratedCodeJobsResponse_nextToken = Lens.lens (\ListGeneratedCodeJobsResponse' {nextToken} -> nextToken) (\s@ListGeneratedCodeJobsResponse' {} a -> s {nextToken = a} :: ListGeneratedCodeJobsResponse)

-- | The list of generated code jobs.
listGeneratedCodeJobsResponse_generatedCodeJobs :: Lens.Lens' ListGeneratedCodeJobsResponse (Prelude.Maybe [GeneratedCodeJobDetails])
listGeneratedCodeJobsResponse_generatedCodeJobs = Lens.lens (\ListGeneratedCodeJobsResponse' {generatedCodeJobs} -> generatedCodeJobs) (\s@ListGeneratedCodeJobsResponse' {} a -> s {generatedCodeJobs = a} :: ListGeneratedCodeJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGeneratedCodeJobsResponse_httpStatus :: Lens.Lens' ListGeneratedCodeJobsResponse Prelude.Int
listGeneratedCodeJobsResponse_httpStatus = Lens.lens (\ListGeneratedCodeJobsResponse' {httpStatus} -> httpStatus) (\s@ListGeneratedCodeJobsResponse' {} a -> s {httpStatus = a} :: ListGeneratedCodeJobsResponse)

instance Prelude.NFData ListGeneratedCodeJobsResponse where
  rnf ListGeneratedCodeJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf generatedCodeJobs
      `Prelude.seq` Prelude.rnf httpStatus

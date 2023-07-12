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
-- Module      : Amazonka.GamesParks.ListSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of snapshot summaries from the game.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListSnapshots
  ( -- * Creating a Request
    ListSnapshots (..),
    newListSnapshots,

    -- * Request Lenses
    listSnapshots_maxResults,
    listSnapshots_nextToken,
    listSnapshots_gameName,

    -- * Destructuring the Response
    ListSnapshotsResponse (..),
    newListSnapshotsResponse,

    -- * Response Lenses
    listSnapshotsResponse_nextToken,
    listSnapshotsResponse_snapshots,
    listSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSnapshots' smart constructor.
data ListSnapshots = ListSnapshots'
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
-- Create a value of 'ListSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSnapshots_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'nextToken', 'listSnapshots_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'gameName', 'listSnapshots_gameName' - The name of the game.
newListSnapshots ::
  -- | 'gameName'
  Prelude.Text ->
  ListSnapshots
newListSnapshots pGameName_ =
  ListSnapshots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      gameName = pGameName_
    }

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listSnapshots_maxResults :: Lens.Lens' ListSnapshots (Prelude.Maybe Prelude.Natural)
listSnapshots_maxResults = Lens.lens (\ListSnapshots' {maxResults} -> maxResults) (\s@ListSnapshots' {} a -> s {maxResults = a} :: ListSnapshots)

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listSnapshots_nextToken :: Lens.Lens' ListSnapshots (Prelude.Maybe Prelude.Text)
listSnapshots_nextToken = Lens.lens (\ListSnapshots' {nextToken} -> nextToken) (\s@ListSnapshots' {} a -> s {nextToken = a} :: ListSnapshots)

-- | The name of the game.
listSnapshots_gameName :: Lens.Lens' ListSnapshots Prelude.Text
listSnapshots_gameName = Lens.lens (\ListSnapshots' {gameName} -> gameName) (\s@ListSnapshots' {} a -> s {gameName = a} :: ListSnapshots)

instance Core.AWSPager ListSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSnapshotsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSnapshotsResponse_snapshots
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSnapshots_nextToken
          Lens..~ rs
          Lens.^? listSnapshotsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSnapshots where
  type
    AWSResponse ListSnapshots =
      ListSnapshotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSnapshotsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Snapshots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSnapshots where
  hashWithSalt _salt ListSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` gameName

instance Prelude.NFData ListSnapshots where
  rnf ListSnapshots' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf gameName

instance Data.ToHeaders ListSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSnapshots where
  toPath ListSnapshots' {..} =
    Prelude.mconcat
      ["/game/", Data.toBS gameName, "/snapshot"]

instance Data.ToQuery ListSnapshots where
  toQuery ListSnapshots' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSnapshotsResponse' smart constructor.
data ListSnapshotsResponse = ListSnapshotsResponse'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of snapshot summaries. You can use the returned snapshot IDs in
    -- the @UpdateSnapshot@ and @GetSnapshot@ operations.
    snapshots :: Prelude.Maybe [SnapshotSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSnapshotsResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'snapshots', 'listSnapshotsResponse_snapshots' - A list of snapshot summaries. You can use the returned snapshot IDs in
-- the @UpdateSnapshot@ and @GetSnapshot@ operations.
--
-- 'httpStatus', 'listSnapshotsResponse_httpStatus' - The response's http status code.
newListSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSnapshotsResponse
newListSnapshotsResponse pHttpStatus_ =
  ListSnapshotsResponse'
    { nextToken = Prelude.Nothing,
      snapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listSnapshotsResponse_nextToken :: Lens.Lens' ListSnapshotsResponse (Prelude.Maybe Prelude.Text)
listSnapshotsResponse_nextToken = Lens.lens (\ListSnapshotsResponse' {nextToken} -> nextToken) (\s@ListSnapshotsResponse' {} a -> s {nextToken = a} :: ListSnapshotsResponse)

-- | A list of snapshot summaries. You can use the returned snapshot IDs in
-- the @UpdateSnapshot@ and @GetSnapshot@ operations.
listSnapshotsResponse_snapshots :: Lens.Lens' ListSnapshotsResponse (Prelude.Maybe [SnapshotSummary])
listSnapshotsResponse_snapshots = Lens.lens (\ListSnapshotsResponse' {snapshots} -> snapshots) (\s@ListSnapshotsResponse' {} a -> s {snapshots = a} :: ListSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSnapshotsResponse_httpStatus :: Lens.Lens' ListSnapshotsResponse Prelude.Int
listSnapshotsResponse_httpStatus = Lens.lens (\ListSnapshotsResponse' {httpStatus} -> httpStatus) (\s@ListSnapshotsResponse' {} a -> s {httpStatus = a} :: ListSnapshotsResponse)

instance Prelude.NFData ListSnapshotsResponse where
  rnf ListSnapshotsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshots
      `Prelude.seq` Prelude.rnf httpStatus

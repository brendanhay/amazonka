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
-- Module      : Amazonka.KinesisAnalyticsV2.ListApplicationSnapshots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the current application snapshots.
--
-- This operation returns paginated results.
module Amazonka.KinesisAnalyticsV2.ListApplicationSnapshots
  ( -- * Creating a Request
    ListApplicationSnapshots (..),
    newListApplicationSnapshots,

    -- * Request Lenses
    listApplicationSnapshots_nextToken,
    listApplicationSnapshots_limit,
    listApplicationSnapshots_applicationName,

    -- * Destructuring the Response
    ListApplicationSnapshotsResponse (..),
    newListApplicationSnapshotsResponse,

    -- * Response Lenses
    listApplicationSnapshotsResponse_nextToken,
    listApplicationSnapshotsResponse_snapshotSummaries,
    listApplicationSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationSnapshots' smart constructor.
data ListApplicationSnapshots = ListApplicationSnapshots'
  { -- | Use this parameter if you receive a @NextToken@ response in a previous
    -- request that indicates that there is more output available. Set it to
    -- the value of the previous call\'s @NextToken@ response to indicate where
    -- the output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of application snapshots to list.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of an existing application.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationSnapshots_nextToken' - Use this parameter if you receive a @NextToken@ response in a previous
-- request that indicates that there is more output available. Set it to
-- the value of the previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
--
-- 'limit', 'listApplicationSnapshots_limit' - The maximum number of application snapshots to list.
--
-- 'applicationName', 'listApplicationSnapshots_applicationName' - The name of an existing application.
newListApplicationSnapshots ::
  -- | 'applicationName'
  Prelude.Text ->
  ListApplicationSnapshots
newListApplicationSnapshots pApplicationName_ =
  ListApplicationSnapshots'
    { nextToken =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Use this parameter if you receive a @NextToken@ response in a previous
-- request that indicates that there is more output available. Set it to
-- the value of the previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
listApplicationSnapshots_nextToken :: Lens.Lens' ListApplicationSnapshots (Prelude.Maybe Prelude.Text)
listApplicationSnapshots_nextToken = Lens.lens (\ListApplicationSnapshots' {nextToken} -> nextToken) (\s@ListApplicationSnapshots' {} a -> s {nextToken = a} :: ListApplicationSnapshots)

-- | The maximum number of application snapshots to list.
listApplicationSnapshots_limit :: Lens.Lens' ListApplicationSnapshots (Prelude.Maybe Prelude.Natural)
listApplicationSnapshots_limit = Lens.lens (\ListApplicationSnapshots' {limit} -> limit) (\s@ListApplicationSnapshots' {} a -> s {limit = a} :: ListApplicationSnapshots)

-- | The name of an existing application.
listApplicationSnapshots_applicationName :: Lens.Lens' ListApplicationSnapshots Prelude.Text
listApplicationSnapshots_applicationName = Lens.lens (\ListApplicationSnapshots' {applicationName} -> applicationName) (\s@ListApplicationSnapshots' {} a -> s {applicationName = a} :: ListApplicationSnapshots)

instance Core.AWSPager ListApplicationSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationSnapshotsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationSnapshotsResponse_snapshotSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApplicationSnapshots_nextToken
          Lens..~ rs
          Lens.^? listApplicationSnapshotsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationSnapshots where
  type
    AWSResponse ListApplicationSnapshots =
      ListApplicationSnapshotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationSnapshotsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "SnapshotSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationSnapshots where
  hashWithSalt _salt ListApplicationSnapshots' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData ListApplicationSnapshots where
  rnf ListApplicationSnapshots' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders ListApplicationSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.ListApplicationSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplicationSnapshots where
  toJSON ListApplicationSnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Limit" Data..=) Prelude.<$> limit,
            Prelude.Just
              ("ApplicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath ListApplicationSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApplicationSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationSnapshotsResponse' smart constructor.
data ListApplicationSnapshotsResponse = ListApplicationSnapshotsResponse'
  { -- | The token for the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A collection of objects containing information about the application
    -- snapshots.
    snapshotSummaries :: Prelude.Maybe [SnapshotDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationSnapshotsResponse_nextToken' - The token for the next set of results, or @null@ if there are no
-- additional results.
--
-- 'snapshotSummaries', 'listApplicationSnapshotsResponse_snapshotSummaries' - A collection of objects containing information about the application
-- snapshots.
--
-- 'httpStatus', 'listApplicationSnapshotsResponse_httpStatus' - The response's http status code.
newListApplicationSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationSnapshotsResponse
newListApplicationSnapshotsResponse pHttpStatus_ =
  ListApplicationSnapshotsResponse'
    { nextToken =
        Prelude.Nothing,
      snapshotSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or @null@ if there are no
-- additional results.
listApplicationSnapshotsResponse_nextToken :: Lens.Lens' ListApplicationSnapshotsResponse (Prelude.Maybe Prelude.Text)
listApplicationSnapshotsResponse_nextToken = Lens.lens (\ListApplicationSnapshotsResponse' {nextToken} -> nextToken) (\s@ListApplicationSnapshotsResponse' {} a -> s {nextToken = a} :: ListApplicationSnapshotsResponse)

-- | A collection of objects containing information about the application
-- snapshots.
listApplicationSnapshotsResponse_snapshotSummaries :: Lens.Lens' ListApplicationSnapshotsResponse (Prelude.Maybe [SnapshotDetails])
listApplicationSnapshotsResponse_snapshotSummaries = Lens.lens (\ListApplicationSnapshotsResponse' {snapshotSummaries} -> snapshotSummaries) (\s@ListApplicationSnapshotsResponse' {} a -> s {snapshotSummaries = a} :: ListApplicationSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApplicationSnapshotsResponse_httpStatus :: Lens.Lens' ListApplicationSnapshotsResponse Prelude.Int
listApplicationSnapshotsResponse_httpStatus = Lens.lens (\ListApplicationSnapshotsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationSnapshotsResponse' {} a -> s {httpStatus = a} :: ListApplicationSnapshotsResponse)

instance
  Prelude.NFData
    ListApplicationSnapshotsResponse
  where
  rnf ListApplicationSnapshotsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshotSummaries
      `Prelude.seq` Prelude.rnf httpStatus

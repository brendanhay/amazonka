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
-- Module      : Network.AWS.KinesisAnalyticsV2.ListApplicationSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the current application snapshots.
--
-- This operation returns paginated results.
module Network.AWS.KinesisAnalyticsV2.ListApplicationSnapshots
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
    listApplicationSnapshotsResponse_snapshotSummaries,
    listApplicationSnapshotsResponse_nextToken,
    listApplicationSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalyticsV2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationSnapshotsResponse'
            Prelude.<$> ( x Core..?> "SnapshotSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationSnapshots

instance Prelude.NFData ListApplicationSnapshots

instance Core.ToHeaders ListApplicationSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.ListApplicationSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListApplicationSnapshots where
  toJSON ListApplicationSnapshots' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just
              ("ApplicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath ListApplicationSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery ListApplicationSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationSnapshotsResponse' smart constructor.
data ListApplicationSnapshotsResponse = ListApplicationSnapshotsResponse'
  { -- | A collection of objects containing information about the application
    -- snapshots.
    snapshotSummaries :: Prelude.Maybe [SnapshotDetails],
    -- | The token for the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'snapshotSummaries', 'listApplicationSnapshotsResponse_snapshotSummaries' - A collection of objects containing information about the application
-- snapshots.
--
-- 'nextToken', 'listApplicationSnapshotsResponse_nextToken' - The token for the next set of results, or @null@ if there are no
-- additional results.
--
-- 'httpStatus', 'listApplicationSnapshotsResponse_httpStatus' - The response's http status code.
newListApplicationSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationSnapshotsResponse
newListApplicationSnapshotsResponse pHttpStatus_ =
  ListApplicationSnapshotsResponse'
    { snapshotSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of objects containing information about the application
-- snapshots.
listApplicationSnapshotsResponse_snapshotSummaries :: Lens.Lens' ListApplicationSnapshotsResponse (Prelude.Maybe [SnapshotDetails])
listApplicationSnapshotsResponse_snapshotSummaries = Lens.lens (\ListApplicationSnapshotsResponse' {snapshotSummaries} -> snapshotSummaries) (\s@ListApplicationSnapshotsResponse' {} a -> s {snapshotSummaries = a} :: ListApplicationSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or @null@ if there are no
-- additional results.
listApplicationSnapshotsResponse_nextToken :: Lens.Lens' ListApplicationSnapshotsResponse (Prelude.Maybe Prelude.Text)
listApplicationSnapshotsResponse_nextToken = Lens.lens (\ListApplicationSnapshotsResponse' {nextToken} -> nextToken) (\s@ListApplicationSnapshotsResponse' {} a -> s {nextToken = a} :: ListApplicationSnapshotsResponse)

-- | The response's http status code.
listApplicationSnapshotsResponse_httpStatus :: Lens.Lens' ListApplicationSnapshotsResponse Prelude.Int
listApplicationSnapshotsResponse_httpStatus = Lens.lens (\ListApplicationSnapshotsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationSnapshotsResponse' {} a -> s {httpStatus = a} :: ListApplicationSnapshotsResponse)

instance
  Prelude.NFData
    ListApplicationSnapshotsResponse

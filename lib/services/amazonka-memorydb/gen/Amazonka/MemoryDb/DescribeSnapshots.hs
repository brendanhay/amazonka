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
-- Module      : Amazonka.MemoryDb.DescribeSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster snapshots. By default,
-- DescribeSnapshots lists all of your snapshots; it can optionally
-- describe a single snapshot, or just the snapshots associated with a
-- particular cluster.
--
-- This operation returns paginated results.
module Amazonka.MemoryDb.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_clusterName,
    describeSnapshots_maxResults,
    describeSnapshots_nextToken,
    describeSnapshots_showDetail,
    describeSnapshots_snapshotName,
    describeSnapshots_source,

    -- * Destructuring the Response
    DescribeSnapshotsResponse (..),
    newDescribeSnapshotsResponse,

    -- * Response Lenses
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | A user-supplied cluster identifier. If this parameter is specified, only
    -- snapshots associated with that specific cluster are described.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value which if true, the shard configuration is included in
    -- the snapshot description.
    showDetail :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name of the snapshot. If this parameter is specified,
    -- only this named snapshot is described.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | If set to system, the output shows snapshots that were automatically
    -- created by MemoryDB. If set to user the output shows snapshots that were
    -- manually created. If omitted, the output shows both automatically and
    -- manually created snapshots.
    source :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeSnapshots_clusterName' - A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
--
-- 'maxResults', 'describeSnapshots_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'describeSnapshots_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'showDetail', 'describeSnapshots_showDetail' - A Boolean value which if true, the shard configuration is included in
-- the snapshot description.
--
-- 'snapshotName', 'describeSnapshots_snapshotName' - A user-supplied name of the snapshot. If this parameter is specified,
-- only this named snapshot is described.
--
-- 'source', 'describeSnapshots_source' - If set to system, the output shows snapshots that were automatically
-- created by MemoryDB. If set to user the output shows snapshots that were
-- manually created. If omitted, the output shows both automatically and
-- manually created snapshots.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { clusterName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      showDetail = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | A user-supplied cluster identifier. If this parameter is specified, only
-- snapshots associated with that specific cluster are described.
describeSnapshots_clusterName :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_clusterName = Lens.lens (\DescribeSnapshots' {clusterName} -> clusterName) (\s@DescribeSnapshots' {} a -> s {clusterName = a} :: DescribeSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeSnapshots_maxResults :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Int)
describeSnapshots_maxResults = Lens.lens (\DescribeSnapshots' {maxResults} -> maxResults) (\s@DescribeSnapshots' {} a -> s {maxResults = a} :: DescribeSnapshots)

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeSnapshots_nextToken :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_nextToken = Lens.lens (\DescribeSnapshots' {nextToken} -> nextToken) (\s@DescribeSnapshots' {} a -> s {nextToken = a} :: DescribeSnapshots)

-- | A Boolean value which if true, the shard configuration is included in
-- the snapshot description.
describeSnapshots_showDetail :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Bool)
describeSnapshots_showDetail = Lens.lens (\DescribeSnapshots' {showDetail} -> showDetail) (\s@DescribeSnapshots' {} a -> s {showDetail = a} :: DescribeSnapshots)

-- | A user-supplied name of the snapshot. If this parameter is specified,
-- only this named snapshot is described.
describeSnapshots_snapshotName :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_snapshotName = Lens.lens (\DescribeSnapshots' {snapshotName} -> snapshotName) (\s@DescribeSnapshots' {} a -> s {snapshotName = a} :: DescribeSnapshots)

-- | If set to system, the output shows snapshots that were automatically
-- created by MemoryDB. If set to user the output shows snapshots that were
-- manually created. If omitted, the output shows both automatically and
-- manually created snapshots.
describeSnapshots_source :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_source = Lens.lens (\DescribeSnapshots' {source} -> source) (\s@DescribeSnapshots' {} a -> s {source = a} :: DescribeSnapshots)

instance Core.AWSPager DescribeSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotsResponse_snapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSnapshots_nextToken
          Lens..~ rs
          Lens.^? describeSnapshotsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSnapshots where
  type
    AWSResponse DescribeSnapshots =
      DescribeSnapshotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSnapshotsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Snapshots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshots where
  hashWithSalt _salt DescribeSnapshots' {..} =
    _salt `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` showDetail
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` source

instance Prelude.NFData DescribeSnapshots where
  rnf DescribeSnapshots' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf showDetail
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf source

instance Data.ToHeaders DescribeSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSnapshots where
  toJSON DescribeSnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterName" Data..=) Prelude.<$> clusterName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ShowDetail" Data..=) Prelude.<$> showDetail,
            ("SnapshotName" Data..=) Prelude.<$> snapshotName,
            ("Source" Data..=) Prelude.<$> source
          ]
      )

instance Data.ToPath DescribeSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of snapshots. Each item in the list contains detailed information
    -- about one snapshot.
    snapshots :: Prelude.Maybe [Snapshot],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSnapshotsResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
--
-- 'httpStatus', 'describeSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotsResponse
newDescribeSnapshotsResponse pHttpStatus_ =
  DescribeSnapshotsResponse'
    { nextToken =
        Prelude.Nothing,
      snapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeSnapshotsResponse_nextToken :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotsResponse_nextToken = Lens.lens (\DescribeSnapshotsResponse' {nextToken} -> nextToken) (\s@DescribeSnapshotsResponse' {} a -> s {nextToken = a} :: DescribeSnapshotsResponse)

-- | A list of snapshots. Each item in the list contains detailed information
-- about one snapshot.
describeSnapshotsResponse_snapshots :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe [Snapshot])
describeSnapshotsResponse_snapshots = Lens.lens (\DescribeSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotsResponse_httpStatus :: Lens.Lens' DescribeSnapshotsResponse Prelude.Int
describeSnapshotsResponse_httpStatus = Lens.lens (\DescribeSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotsResponse)

instance Prelude.NFData DescribeSnapshotsResponse where
  rnf DescribeSnapshotsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshots
      `Prelude.seq` Prelude.rnf httpStatus

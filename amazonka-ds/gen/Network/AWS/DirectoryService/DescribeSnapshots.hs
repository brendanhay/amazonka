{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directory snapshots that belong to this
-- account.
--
-- This operation supports pagination with the use of the /NextToken/
-- request and response parameters. If more results are available, the
-- /DescribeSnapshots.NextToken/ member contains a token that you pass in
-- the next call to DescribeSnapshots to retrieve the next set of items.
--
-- You can also specify a maximum number of return results with the /Limit/
-- parameter.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshots_directoryId,
    describeSnapshots_limit,

    -- * Destructuring the Response
    DescribeSnapshotsResponse (..),
    newDescribeSnapshotsResponse,

    -- * Response Lenses
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DescribeSnapshots operation.
--
-- /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to
    -- DescribeSnapshots. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers of the snapshots to obtain the information for. If
    -- this member is null or empty, all snapshots are returned using the
    -- /Limit/ and /NextToken/ members.
    snapshotIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the directory for which to retrieve snapshot
    -- information.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSnapshots_nextToken' - The /DescribeSnapshotsResult.NextToken/ value from a previous call to
-- DescribeSnapshots. Pass null if this is the first call.
--
-- 'snapshotIds', 'describeSnapshots_snapshotIds' - A list of identifiers of the snapshots to obtain the information for. If
-- this member is null or empty, all snapshots are returned using the
-- /Limit/ and /NextToken/ members.
--
-- 'directoryId', 'describeSnapshots_directoryId' - The identifier of the directory for which to retrieve snapshot
-- information.
--
-- 'limit', 'describeSnapshots_limit' - The maximum number of objects to return.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { nextToken = Prelude.Nothing,
      snapshotIds = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to
-- DescribeSnapshots. Pass null if this is the first call.
describeSnapshots_nextToken :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_nextToken = Lens.lens (\DescribeSnapshots' {nextToken} -> nextToken) (\s@DescribeSnapshots' {} a -> s {nextToken = a} :: DescribeSnapshots)

-- | A list of identifiers of the snapshots to obtain the information for. If
-- this member is null or empty, all snapshots are returned using the
-- /Limit/ and /NextToken/ members.
describeSnapshots_snapshotIds :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Prelude.Text])
describeSnapshots_snapshotIds = Lens.lens (\DescribeSnapshots' {snapshotIds} -> snapshotIds) (\s@DescribeSnapshots' {} a -> s {snapshotIds = a} :: DescribeSnapshots) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the directory for which to retrieve snapshot
-- information.
describeSnapshots_directoryId :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_directoryId = Lens.lens (\DescribeSnapshots' {directoryId} -> directoryId) (\s@DescribeSnapshots' {} a -> s {directoryId = a} :: DescribeSnapshots)

-- | The maximum number of objects to return.
describeSnapshots_limit :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Natural)
describeSnapshots_limit = Lens.lens (\DescribeSnapshots' {limit} -> limit) (\s@DescribeSnapshots' {} a -> s {limit = a} :: DescribeSnapshots)

instance Pager.AWSPager DescribeSnapshots where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeSnapshotsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeSnapshotsResponse_snapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSnapshots_nextToken
          Lens..~ rs
          Lens.^? describeSnapshotsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSnapshotsResponse'
            Prelude.<$> ( x Prelude..?> "Snapshots"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshots

instance Prelude.NFData DescribeSnapshots

instance Prelude.ToHeaders DescribeSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DescribeSnapshots" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeSnapshots where
  toJSON DescribeSnapshots' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("SnapshotIds" Prelude..=) Prelude.<$> snapshotIds,
            ("DirectoryId" Prelude..=) Prelude.<$> directoryId,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath DescribeSnapshots where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DescribeSnapshots operation.
--
-- /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | The list of Snapshot objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the /Limit/ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
    snapshots :: Prelude.Maybe [Snapshot],
    -- | If not null, more results are available. Pass this value in the
    -- /NextToken/ member of a subsequent call to DescribeSnapshots.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
--
-- 'nextToken', 'describeSnapshotsResponse_nextToken' - If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
--
-- 'httpStatus', 'describeSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotsResponse
newDescribeSnapshotsResponse pHttpStatus_ =
  DescribeSnapshotsResponse'
    { snapshots =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
describeSnapshotsResponse_snapshots :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe [Snapshot])
describeSnapshotsResponse_snapshots = Lens.lens (\DescribeSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeSnapshotsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
describeSnapshotsResponse_nextToken :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotsResponse_nextToken = Lens.lens (\DescribeSnapshotsResponse' {nextToken} -> nextToken) (\s@DescribeSnapshotsResponse' {} a -> s {nextToken = a} :: DescribeSnapshotsResponse)

-- | The response's http status code.
describeSnapshotsResponse_httpStatus :: Lens.Lens' DescribeSnapshotsResponse Prelude.Int
describeSnapshotsResponse_httpStatus = Lens.lens (\DescribeSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotsResponse)

instance Prelude.NFData DescribeSnapshotsResponse

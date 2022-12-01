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
-- Module      : Amazonka.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DirectoryService.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_directoryId,
    describeSnapshots_nextToken,
    describeSnapshots_snapshotIds,
    describeSnapshots_limit,

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
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the DescribeSnapshots operation.
--
-- /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | The identifier of the directory for which to retrieve snapshot
    -- information.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to
    -- DescribeSnapshots. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of identifiers of the snapshots to obtain the information for. If
    -- this member is null or empty, all snapshots are returned using the
    -- /Limit/ and /NextToken/ members.
    snapshotIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of objects to return.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'directoryId', 'describeSnapshots_directoryId' - The identifier of the directory for which to retrieve snapshot
-- information.
--
-- 'nextToken', 'describeSnapshots_nextToken' - The /DescribeSnapshotsResult.NextToken/ value from a previous call to
-- DescribeSnapshots. Pass null if this is the first call.
--
-- 'snapshotIds', 'describeSnapshots_snapshotIds' - A list of identifiers of the snapshots to obtain the information for. If
-- this member is null or empty, all snapshots are returned using the
-- /Limit/ and /NextToken/ members.
--
-- 'limit', 'describeSnapshots_limit' - The maximum number of objects to return.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { directoryId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      snapshotIds = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The identifier of the directory for which to retrieve snapshot
-- information.
describeSnapshots_directoryId :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_directoryId = Lens.lens (\DescribeSnapshots' {directoryId} -> directoryId) (\s@DescribeSnapshots' {} a -> s {directoryId = a} :: DescribeSnapshots)

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to
-- DescribeSnapshots. Pass null if this is the first call.
describeSnapshots_nextToken :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_nextToken = Lens.lens (\DescribeSnapshots' {nextToken} -> nextToken) (\s@DescribeSnapshots' {} a -> s {nextToken = a} :: DescribeSnapshots)

-- | A list of identifiers of the snapshots to obtain the information for. If
-- this member is null or empty, all snapshots are returned using the
-- /Limit/ and /NextToken/ members.
describeSnapshots_snapshotIds :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Prelude.Text])
describeSnapshots_snapshotIds = Lens.lens (\DescribeSnapshots' {snapshotIds} -> snapshotIds) (\s@DescribeSnapshots' {} a -> s {snapshotIds = a} :: DescribeSnapshots) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of objects to return.
describeSnapshots_limit :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Natural)
describeSnapshots_limit = Lens.lens (\DescribeSnapshots' {limit} -> limit) (\s@DescribeSnapshots' {} a -> s {limit = a} :: DescribeSnapshots)

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Snapshots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshots where
  hashWithSalt _salt DescribeSnapshots' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` snapshotIds
      `Prelude.hashWithSalt` limit

instance Prelude.NFData DescribeSnapshots where
  rnf DescribeSnapshots' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshotIds
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders DescribeSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSnapshots where
  toJSON DescribeSnapshots' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Core..=) Prelude.<$> directoryId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SnapshotIds" Core..=) Prelude.<$> snapshotIds,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DescribeSnapshots operation.
--
-- /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | If not null, more results are available. Pass this value in the
    -- /NextToken/ member of a subsequent call to DescribeSnapshots.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of Snapshot objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the /Limit/ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
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
-- 'nextToken', 'describeSnapshotsResponse_nextToken' - If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
--
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
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

-- | If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
describeSnapshotsResponse_nextToken :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotsResponse_nextToken = Lens.lens (\DescribeSnapshotsResponse' {nextToken} -> nextToken) (\s@DescribeSnapshotsResponse' {} a -> s {nextToken = a} :: DescribeSnapshotsResponse)

-- | The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
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

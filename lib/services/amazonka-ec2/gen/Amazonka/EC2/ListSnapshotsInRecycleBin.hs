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
-- Module      : Amazonka.EC2.ListSnapshotsInRecycleBin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more snapshots that are currently in the Recycle Bin.
--
-- This operation returns paginated results.
module Amazonka.EC2.ListSnapshotsInRecycleBin
  ( -- * Creating a Request
    ListSnapshotsInRecycleBin (..),
    newListSnapshotsInRecycleBin,

    -- * Request Lenses
    listSnapshotsInRecycleBin_nextToken,
    listSnapshotsInRecycleBin_snapshotIds,
    listSnapshotsInRecycleBin_dryRun,
    listSnapshotsInRecycleBin_maxResults,

    -- * Destructuring the Response
    ListSnapshotsInRecycleBinResponse (..),
    newListSnapshotsInRecycleBinResponse,

    -- * Response Lenses
    listSnapshotsInRecycleBinResponse_nextToken,
    listSnapshotsInRecycleBinResponse_snapshots,
    listSnapshotsInRecycleBinResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSnapshotsInRecycleBin' smart constructor.
data ListSnapshotsInRecycleBin = ListSnapshotsInRecycleBin'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the snapshots to list. Omit this parameter to list all of the
    -- snapshots that are in the Recycle Bin.
    snapshotIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSnapshotsInRecycleBin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSnapshotsInRecycleBin_nextToken' - The token for the next page of results.
--
-- 'snapshotIds', 'listSnapshotsInRecycleBin_snapshotIds' - The IDs of the snapshots to list. Omit this parameter to list all of the
-- snapshots that are in the Recycle Bin.
--
-- 'dryRun', 'listSnapshotsInRecycleBin_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'listSnapshotsInRecycleBin_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListSnapshotsInRecycleBin ::
  ListSnapshotsInRecycleBin
newListSnapshotsInRecycleBin =
  ListSnapshotsInRecycleBin'
    { nextToken =
        Prelude.Nothing,
      snapshotIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listSnapshotsInRecycleBin_nextToken :: Lens.Lens' ListSnapshotsInRecycleBin (Prelude.Maybe Prelude.Text)
listSnapshotsInRecycleBin_nextToken = Lens.lens (\ListSnapshotsInRecycleBin' {nextToken} -> nextToken) (\s@ListSnapshotsInRecycleBin' {} a -> s {nextToken = a} :: ListSnapshotsInRecycleBin)

-- | The IDs of the snapshots to list. Omit this parameter to list all of the
-- snapshots that are in the Recycle Bin.
listSnapshotsInRecycleBin_snapshotIds :: Lens.Lens' ListSnapshotsInRecycleBin (Prelude.Maybe [Prelude.Text])
listSnapshotsInRecycleBin_snapshotIds = Lens.lens (\ListSnapshotsInRecycleBin' {snapshotIds} -> snapshotIds) (\s@ListSnapshotsInRecycleBin' {} a -> s {snapshotIds = a} :: ListSnapshotsInRecycleBin) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
listSnapshotsInRecycleBin_dryRun :: Lens.Lens' ListSnapshotsInRecycleBin (Prelude.Maybe Prelude.Bool)
listSnapshotsInRecycleBin_dryRun = Lens.lens (\ListSnapshotsInRecycleBin' {dryRun} -> dryRun) (\s@ListSnapshotsInRecycleBin' {} a -> s {dryRun = a} :: ListSnapshotsInRecycleBin)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listSnapshotsInRecycleBin_maxResults :: Lens.Lens' ListSnapshotsInRecycleBin (Prelude.Maybe Prelude.Natural)
listSnapshotsInRecycleBin_maxResults = Lens.lens (\ListSnapshotsInRecycleBin' {maxResults} -> maxResults) (\s@ListSnapshotsInRecycleBin' {} a -> s {maxResults = a} :: ListSnapshotsInRecycleBin)

instance Core.AWSPager ListSnapshotsInRecycleBin where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSnapshotsInRecycleBinResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSnapshotsInRecycleBinResponse_snapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSnapshotsInRecycleBin_nextToken
          Lens..~ rs
          Lens.^? listSnapshotsInRecycleBinResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSnapshotsInRecycleBin where
  type
    AWSResponse ListSnapshotsInRecycleBin =
      ListSnapshotsInRecycleBinResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListSnapshotsInRecycleBinResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "snapshotSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSnapshotsInRecycleBin where
  hashWithSalt _salt ListSnapshotsInRecycleBin' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` snapshotIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSnapshotsInRecycleBin where
  rnf ListSnapshotsInRecycleBin' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshotIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSnapshotsInRecycleBin where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListSnapshotsInRecycleBin where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSnapshotsInRecycleBin where
  toQuery ListSnapshotsInRecycleBin' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListSnapshotsInRecycleBin" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "SnapshotId"
              Prelude.<$> snapshotIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSnapshotsInRecycleBinResponse' smart constructor.
data ListSnapshotsInRecycleBinResponse = ListSnapshotsInRecycleBinResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the snapshots.
    snapshots :: Prelude.Maybe [SnapshotRecycleBinInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSnapshotsInRecycleBinResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSnapshotsInRecycleBinResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'snapshots', 'listSnapshotsInRecycleBinResponse_snapshots' - Information about the snapshots.
--
-- 'httpStatus', 'listSnapshotsInRecycleBinResponse_httpStatus' - The response's http status code.
newListSnapshotsInRecycleBinResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSnapshotsInRecycleBinResponse
newListSnapshotsInRecycleBinResponse pHttpStatus_ =
  ListSnapshotsInRecycleBinResponse'
    { nextToken =
        Prelude.Nothing,
      snapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listSnapshotsInRecycleBinResponse_nextToken :: Lens.Lens' ListSnapshotsInRecycleBinResponse (Prelude.Maybe Prelude.Text)
listSnapshotsInRecycleBinResponse_nextToken = Lens.lens (\ListSnapshotsInRecycleBinResponse' {nextToken} -> nextToken) (\s@ListSnapshotsInRecycleBinResponse' {} a -> s {nextToken = a} :: ListSnapshotsInRecycleBinResponse)

-- | Information about the snapshots.
listSnapshotsInRecycleBinResponse_snapshots :: Lens.Lens' ListSnapshotsInRecycleBinResponse (Prelude.Maybe [SnapshotRecycleBinInfo])
listSnapshotsInRecycleBinResponse_snapshots = Lens.lens (\ListSnapshotsInRecycleBinResponse' {snapshots} -> snapshots) (\s@ListSnapshotsInRecycleBinResponse' {} a -> s {snapshots = a} :: ListSnapshotsInRecycleBinResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSnapshotsInRecycleBinResponse_httpStatus :: Lens.Lens' ListSnapshotsInRecycleBinResponse Prelude.Int
listSnapshotsInRecycleBinResponse_httpStatus = Lens.lens (\ListSnapshotsInRecycleBinResponse' {httpStatus} -> httpStatus) (\s@ListSnapshotsInRecycleBinResponse' {} a -> s {httpStatus = a} :: ListSnapshotsInRecycleBinResponse)

instance
  Prelude.NFData
    ListSnapshotsInRecycleBinResponse
  where
  rnf ListSnapshotsInRecycleBinResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshots
      `Prelude.seq` Prelude.rnf httpStatus

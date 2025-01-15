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
-- Module      : Amazonka.DrS.DescribeRecoverySnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Recovery Snapshots for a single Source Server.
--
-- This operation returns paginated results.
module Amazonka.DrS.DescribeRecoverySnapshots
  ( -- * Creating a Request
    DescribeRecoverySnapshots (..),
    newDescribeRecoverySnapshots,

    -- * Request Lenses
    describeRecoverySnapshots_filters,
    describeRecoverySnapshots_maxResults,
    describeRecoverySnapshots_nextToken,
    describeRecoverySnapshots_order,
    describeRecoverySnapshots_sourceServerID,

    -- * Destructuring the Response
    DescribeRecoverySnapshotsResponse (..),
    newDescribeRecoverySnapshotsResponse,

    -- * Response Lenses
    describeRecoverySnapshotsResponse_items,
    describeRecoverySnapshotsResponse_nextToken,
    describeRecoverySnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecoverySnapshots' smart constructor.
data DescribeRecoverySnapshots = DescribeRecoverySnapshots'
  { -- | A set of filters by which to return Recovery Snapshots.
    filters :: Prelude.Maybe DescribeRecoverySnapshotsRequestFilters,
    -- | Maximum number of Recovery Snapshots to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token of the next Recovery Snapshot to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sorted ordering by which to return Recovery Snapshots.
    order :: Prelude.Maybe RecoverySnapshotsOrder,
    -- | Filter Recovery Snapshots by Source Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoverySnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeRecoverySnapshots_filters' - A set of filters by which to return Recovery Snapshots.
--
-- 'maxResults', 'describeRecoverySnapshots_maxResults' - Maximum number of Recovery Snapshots to retrieve.
--
-- 'nextToken', 'describeRecoverySnapshots_nextToken' - The token of the next Recovery Snapshot to retrieve.
--
-- 'order', 'describeRecoverySnapshots_order' - The sorted ordering by which to return Recovery Snapshots.
--
-- 'sourceServerID', 'describeRecoverySnapshots_sourceServerID' - Filter Recovery Snapshots by Source Server ID.
newDescribeRecoverySnapshots ::
  -- | 'sourceServerID'
  Prelude.Text ->
  DescribeRecoverySnapshots
newDescribeRecoverySnapshots pSourceServerID_ =
  DescribeRecoverySnapshots'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      order = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | A set of filters by which to return Recovery Snapshots.
describeRecoverySnapshots_filters :: Lens.Lens' DescribeRecoverySnapshots (Prelude.Maybe DescribeRecoverySnapshotsRequestFilters)
describeRecoverySnapshots_filters = Lens.lens (\DescribeRecoverySnapshots' {filters} -> filters) (\s@DescribeRecoverySnapshots' {} a -> s {filters = a} :: DescribeRecoverySnapshots)

-- | Maximum number of Recovery Snapshots to retrieve.
describeRecoverySnapshots_maxResults :: Lens.Lens' DescribeRecoverySnapshots (Prelude.Maybe Prelude.Natural)
describeRecoverySnapshots_maxResults = Lens.lens (\DescribeRecoverySnapshots' {maxResults} -> maxResults) (\s@DescribeRecoverySnapshots' {} a -> s {maxResults = a} :: DescribeRecoverySnapshots)

-- | The token of the next Recovery Snapshot to retrieve.
describeRecoverySnapshots_nextToken :: Lens.Lens' DescribeRecoverySnapshots (Prelude.Maybe Prelude.Text)
describeRecoverySnapshots_nextToken = Lens.lens (\DescribeRecoverySnapshots' {nextToken} -> nextToken) (\s@DescribeRecoverySnapshots' {} a -> s {nextToken = a} :: DescribeRecoverySnapshots)

-- | The sorted ordering by which to return Recovery Snapshots.
describeRecoverySnapshots_order :: Lens.Lens' DescribeRecoverySnapshots (Prelude.Maybe RecoverySnapshotsOrder)
describeRecoverySnapshots_order = Lens.lens (\DescribeRecoverySnapshots' {order} -> order) (\s@DescribeRecoverySnapshots' {} a -> s {order = a} :: DescribeRecoverySnapshots)

-- | Filter Recovery Snapshots by Source Server ID.
describeRecoverySnapshots_sourceServerID :: Lens.Lens' DescribeRecoverySnapshots Prelude.Text
describeRecoverySnapshots_sourceServerID = Lens.lens (\DescribeRecoverySnapshots' {sourceServerID} -> sourceServerID) (\s@DescribeRecoverySnapshots' {} a -> s {sourceServerID = a} :: DescribeRecoverySnapshots)

instance Core.AWSPager DescribeRecoverySnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRecoverySnapshotsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRecoverySnapshotsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeRecoverySnapshots_nextToken
              Lens..~ rs
              Lens.^? describeRecoverySnapshotsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeRecoverySnapshots where
  type
    AWSResponse DescribeRecoverySnapshots =
      DescribeRecoverySnapshotsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecoverySnapshotsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecoverySnapshots where
  hashWithSalt _salt DescribeRecoverySnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData DescribeRecoverySnapshots where
  rnf DescribeRecoverySnapshots' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf order `Prelude.seq`
            Prelude.rnf sourceServerID

instance Data.ToHeaders DescribeRecoverySnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecoverySnapshots where
  toJSON DescribeRecoverySnapshots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("order" Data..=) Prelude.<$> order,
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath DescribeRecoverySnapshots where
  toPath = Prelude.const "/DescribeRecoverySnapshots"

instance Data.ToQuery DescribeRecoverySnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecoverySnapshotsResponse' smart constructor.
data DescribeRecoverySnapshotsResponse = DescribeRecoverySnapshotsResponse'
  { -- | An array of Recovery Snapshots.
    items :: Prelude.Maybe [RecoverySnapshot],
    -- | The token of the next Recovery Snapshot to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoverySnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeRecoverySnapshotsResponse_items' - An array of Recovery Snapshots.
--
-- 'nextToken', 'describeRecoverySnapshotsResponse_nextToken' - The token of the next Recovery Snapshot to retrieve.
--
-- 'httpStatus', 'describeRecoverySnapshotsResponse_httpStatus' - The response's http status code.
newDescribeRecoverySnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecoverySnapshotsResponse
newDescribeRecoverySnapshotsResponse pHttpStatus_ =
  DescribeRecoverySnapshotsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of Recovery Snapshots.
describeRecoverySnapshotsResponse_items :: Lens.Lens' DescribeRecoverySnapshotsResponse (Prelude.Maybe [RecoverySnapshot])
describeRecoverySnapshotsResponse_items = Lens.lens (\DescribeRecoverySnapshotsResponse' {items} -> items) (\s@DescribeRecoverySnapshotsResponse' {} a -> s {items = a} :: DescribeRecoverySnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token of the next Recovery Snapshot to retrieve.
describeRecoverySnapshotsResponse_nextToken :: Lens.Lens' DescribeRecoverySnapshotsResponse (Prelude.Maybe Prelude.Text)
describeRecoverySnapshotsResponse_nextToken = Lens.lens (\DescribeRecoverySnapshotsResponse' {nextToken} -> nextToken) (\s@DescribeRecoverySnapshotsResponse' {} a -> s {nextToken = a} :: DescribeRecoverySnapshotsResponse)

-- | The response's http status code.
describeRecoverySnapshotsResponse_httpStatus :: Lens.Lens' DescribeRecoverySnapshotsResponse Prelude.Int
describeRecoverySnapshotsResponse_httpStatus = Lens.lens (\DescribeRecoverySnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeRecoverySnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeRecoverySnapshotsResponse)

instance
  Prelude.NFData
    DescribeRecoverySnapshotsResponse
  where
  rnf DescribeRecoverySnapshotsResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus

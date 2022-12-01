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
-- Module      : Amazonka.EC2.DescribeSnapshotTierStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the storage tier status of one or more Amazon EBS snapshots.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSnapshotTierStatus
  ( -- * Creating a Request
    DescribeSnapshotTierStatus (..),
    newDescribeSnapshotTierStatus,

    -- * Request Lenses
    describeSnapshotTierStatus_nextToken,
    describeSnapshotTierStatus_filters,
    describeSnapshotTierStatus_dryRun,
    describeSnapshotTierStatus_maxResults,

    -- * Destructuring the Response
    DescribeSnapshotTierStatusResponse (..),
    newDescribeSnapshotTierStatusResponse,

    -- * Response Lenses
    describeSnapshotTierStatusResponse_nextToken,
    describeSnapshotTierStatusResponse_snapshotTierStatuses,
    describeSnapshotTierStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSnapshotTierStatus' smart constructor.
data DescribeSnapshotTierStatus = DescribeSnapshotTierStatus'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filters.
    --
    -- -   @snapshot-id@ - The snapshot ID.
    --
    -- -   @volume-id@ - The ID of the volume the snapshot is for.
    --
    -- -   @last-tiering-operation@ - The state of the last archive or restore
    --     action. (@archival-in-progress@ | @archival-completed@ |
    --     @archival-failed@ | @permanent-restore-in-progress@ |
    --     @permanent-restore-completed@ | @permanent-restore-failed@ |
    --     @temporary-restore-in-progress@ | @temporary-restore-completed@ |
    --     @temporary-restore-failed@)
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotTierStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSnapshotTierStatus_nextToken' - The token for the next page of results.
--
-- 'filters', 'describeSnapshotTierStatus_filters' - The filters.
--
-- -   @snapshot-id@ - The snapshot ID.
--
-- -   @volume-id@ - The ID of the volume the snapshot is for.
--
-- -   @last-tiering-operation@ - The state of the last archive or restore
--     action. (@archival-in-progress@ | @archival-completed@ |
--     @archival-failed@ | @permanent-restore-in-progress@ |
--     @permanent-restore-completed@ | @permanent-restore-failed@ |
--     @temporary-restore-in-progress@ | @temporary-restore-completed@ |
--     @temporary-restore-failed@)
--
-- 'dryRun', 'describeSnapshotTierStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSnapshotTierStatus_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeSnapshotTierStatus ::
  DescribeSnapshotTierStatus
newDescribeSnapshotTierStatus =
  DescribeSnapshotTierStatus'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeSnapshotTierStatus_nextToken :: Lens.Lens' DescribeSnapshotTierStatus (Prelude.Maybe Prelude.Text)
describeSnapshotTierStatus_nextToken = Lens.lens (\DescribeSnapshotTierStatus' {nextToken} -> nextToken) (\s@DescribeSnapshotTierStatus' {} a -> s {nextToken = a} :: DescribeSnapshotTierStatus)

-- | The filters.
--
-- -   @snapshot-id@ - The snapshot ID.
--
-- -   @volume-id@ - The ID of the volume the snapshot is for.
--
-- -   @last-tiering-operation@ - The state of the last archive or restore
--     action. (@archival-in-progress@ | @archival-completed@ |
--     @archival-failed@ | @permanent-restore-in-progress@ |
--     @permanent-restore-completed@ | @permanent-restore-failed@ |
--     @temporary-restore-in-progress@ | @temporary-restore-completed@ |
--     @temporary-restore-failed@)
describeSnapshotTierStatus_filters :: Lens.Lens' DescribeSnapshotTierStatus (Prelude.Maybe [Filter])
describeSnapshotTierStatus_filters = Lens.lens (\DescribeSnapshotTierStatus' {filters} -> filters) (\s@DescribeSnapshotTierStatus' {} a -> s {filters = a} :: DescribeSnapshotTierStatus) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSnapshotTierStatus_dryRun :: Lens.Lens' DescribeSnapshotTierStatus (Prelude.Maybe Prelude.Bool)
describeSnapshotTierStatus_dryRun = Lens.lens (\DescribeSnapshotTierStatus' {dryRun} -> dryRun) (\s@DescribeSnapshotTierStatus' {} a -> s {dryRun = a} :: DescribeSnapshotTierStatus)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeSnapshotTierStatus_maxResults :: Lens.Lens' DescribeSnapshotTierStatus (Prelude.Maybe Prelude.Int)
describeSnapshotTierStatus_maxResults = Lens.lens (\DescribeSnapshotTierStatus' {maxResults} -> maxResults) (\s@DescribeSnapshotTierStatus' {} a -> s {maxResults = a} :: DescribeSnapshotTierStatus)

instance Core.AWSPager DescribeSnapshotTierStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotTierStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotTierStatusResponse_snapshotTierStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSnapshotTierStatus_nextToken
          Lens..~ rs
          Lens.^? describeSnapshotTierStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSnapshotTierStatus where
  type
    AWSResponse DescribeSnapshotTierStatus =
      DescribeSnapshotTierStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotTierStatusResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "snapshotTierStatusSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotTierStatus where
  hashWithSalt _salt DescribeSnapshotTierStatus' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeSnapshotTierStatus where
  rnf DescribeSnapshotTierStatus' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeSnapshotTierStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSnapshotTierStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSnapshotTierStatus where
  toQuery DescribeSnapshotTierStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshotTierStatus" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeSnapshotTierStatusResponse' smart constructor.
data DescribeSnapshotTierStatusResponse = DescribeSnapshotTierStatusResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the snapshot\'s storage tier.
    snapshotTierStatuses :: Prelude.Maybe [SnapshotTierStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotTierStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSnapshotTierStatusResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'snapshotTierStatuses', 'describeSnapshotTierStatusResponse_snapshotTierStatuses' - Information about the snapshot\'s storage tier.
--
-- 'httpStatus', 'describeSnapshotTierStatusResponse_httpStatus' - The response's http status code.
newDescribeSnapshotTierStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotTierStatusResponse
newDescribeSnapshotTierStatusResponse pHttpStatus_ =
  DescribeSnapshotTierStatusResponse'
    { nextToken =
        Prelude.Nothing,
      snapshotTierStatuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeSnapshotTierStatusResponse_nextToken :: Lens.Lens' DescribeSnapshotTierStatusResponse (Prelude.Maybe Prelude.Text)
describeSnapshotTierStatusResponse_nextToken = Lens.lens (\DescribeSnapshotTierStatusResponse' {nextToken} -> nextToken) (\s@DescribeSnapshotTierStatusResponse' {} a -> s {nextToken = a} :: DescribeSnapshotTierStatusResponse)

-- | Information about the snapshot\'s storage tier.
describeSnapshotTierStatusResponse_snapshotTierStatuses :: Lens.Lens' DescribeSnapshotTierStatusResponse (Prelude.Maybe [SnapshotTierStatus])
describeSnapshotTierStatusResponse_snapshotTierStatuses = Lens.lens (\DescribeSnapshotTierStatusResponse' {snapshotTierStatuses} -> snapshotTierStatuses) (\s@DescribeSnapshotTierStatusResponse' {} a -> s {snapshotTierStatuses = a} :: DescribeSnapshotTierStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotTierStatusResponse_httpStatus :: Lens.Lens' DescribeSnapshotTierStatusResponse Prelude.Int
describeSnapshotTierStatusResponse_httpStatus = Lens.lens (\DescribeSnapshotTierStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotTierStatusResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotTierStatusResponse)

instance
  Prelude.NFData
    DescribeSnapshotTierStatusResponse
  where
  rnf DescribeSnapshotTierStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf snapshotTierStatuses
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS snapshots available to you or all of the EBS
-- snapshots available to you.
--
-- The snapshots available to you include public snapshots, private
-- snapshots that you own, and private snapshots owned by other AWS
-- accounts for which you have explicit create volume permissions.
--
-- The create volume permissions fall into the following categories:
--
-- -   /public/: The owner of the snapshot granted create volume
--     permissions for the snapshot to the @all@ group. All AWS accounts
--     have create volume permissions for these snapshots.
--
-- -   /explicit/: The owner of the snapshot granted create volume
--     permissions to a specific AWS account.
--
-- -   /implicit/: An AWS account has implicit create volume permissions
--     for all snapshots it owns.
--
-- The list of snapshots returned can be filtered by specifying snapshot
-- IDs, snapshot owners, or AWS accounts with create volume permissions. If
-- no options are specified, Amazon EC2 returns all snapshots for which you
-- have create volume permissions.
--
-- If you specify one or more snapshot IDs, only snapshots that have the
-- specified IDs are returned. If you specify an invalid snapshot ID, an
-- error is returned. If you specify a snapshot ID for which you do not
-- have access, it is not included in the returned results.
--
-- If you specify one or more snapshot owners using the @OwnerIds@ option,
-- only snapshots from the specified owners and for which you have access
-- are returned. The results can include the AWS account IDs of the
-- specified owners, @amazon@ for snapshots owned by Amazon, or @self@ for
-- snapshots that you own.
--
-- If you specify a list of restorable users, only snapshots with create
-- snapshot permissions for those users are returned. You can specify AWS
-- account IDs (if you own the snapshots), @self@ for snapshots for which
-- you own or have explicit permissions, or @all@ for public snapshots.
--
-- If you are describing a long list of snapshots, we recommend that you
-- paginate the output to make the list more manageable. The @MaxResults@
-- parameter sets the maximum number of results returned in a single page.
-- If the list of results exceeds your @MaxResults@ value, then that number
-- of results is returned along with a @NextToken@ value that can be passed
-- to a subsequent @DescribeSnapshots@ request to retrieve the remaining
-- results.
--
-- To get the state of fast snapshot restores for a snapshot, use
-- DescribeFastSnapshotRestores.
--
-- For more information about EBS snapshots, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSSnapshots.html Amazon EBS snapshots>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSnapshots
  ( -- * Creating a Request
    DescribeSnapshots (..),
    newDescribeSnapshots,

    -- * Request Lenses
    describeSnapshots_ownerIds,
    describeSnapshots_nextToken,
    describeSnapshots_dryRun,
    describeSnapshots_maxResults,
    describeSnapshots_restorableByUserIds,
    describeSnapshots_snapshotIds,
    describeSnapshots_filters,

    -- * Destructuring the Response
    DescribeSnapshotsResponse (..),
    newDescribeSnapshotsResponse,

    -- * Response Lenses
    describeSnapshotsResponse_snapshots,
    describeSnapshotsResponse_nextToken,
    describeSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | Scopes the results to snapshots with the specified owners. You can
    -- specify a combination of AWS account IDs, @self@, and @amazon@.
    ownerIds :: Prelude.Maybe [Prelude.Text],
    -- | The @NextToken@ value returned from a previous paginated
    -- @DescribeSnapshots@ request where @MaxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @NextToken@ value. This value
    -- is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of snapshot results returned by @DescribeSnapshots@
    -- in paginated output. When this parameter is used, @DescribeSnapshots@
    -- only returns @MaxResults@ results in a single page along with a
    -- @NextToken@ response element. The remaining results of the initial
    -- request can be seen by sending another @DescribeSnapshots@ request with
    -- the returned @NextToken@ value. This value can be between 5 and 1,000;
    -- if @MaxResults@ is given a value larger than 1,000, only 1,000 results
    -- are returned. If this parameter is not used, then @DescribeSnapshots@
    -- returns all results. You cannot specify this parameter and the snapshot
    -- IDs parameter in the same request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The IDs of the AWS accounts that can create volumes from the snapshot.
    restorableByUserIds :: Prelude.Maybe [Prelude.Text],
    -- | The snapshot IDs.
    --
    -- Default: Describes the snapshots for which you have create volume
    -- permissions.
    snapshotIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @description@ - A description of the snapshot.
    --
    -- -   @encrypted@ - Indicates whether the snapshot is encrypted (@true@ |
    --     @false@)
    --
    -- -   @owner-alias@ - The owner alias, from an Amazon-maintained list
    --     (@amazon@). This is not the user-configured AWS account alias set
    --     using the IAM console. We recommend that you use the related
    --     parameter instead of this filter.
    --
    -- -   @owner-id@ - The AWS account ID of the owner. We recommend that you
    --     use the related parameter instead of this filter.
    --
    -- -   @progress@ - The progress of the snapshot, as a percentage (for
    --     example, 80%).
    --
    -- -   @snapshot-id@ - The snapshot ID.
    --
    -- -   @start-time@ - The time stamp when the snapshot was initiated.
    --
    -- -   @status@ - The status of the snapshot (@pending@ | @completed@ |
    --     @error@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @volume-id@ - The ID of the volume the snapshot is for.
    --
    -- -   @volume-size@ - The size of the volume, in GiB.
    filters :: Prelude.Maybe [Filter]
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
-- 'ownerIds', 'describeSnapshots_ownerIds' - Scopes the results to snapshots with the specified owners. You can
-- specify a combination of AWS account IDs, @self@, and @amazon@.
--
-- 'nextToken', 'describeSnapshots_nextToken' - The @NextToken@ value returned from a previous paginated
-- @DescribeSnapshots@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- 'dryRun', 'describeSnapshots_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSnapshots_maxResults' - The maximum number of snapshot results returned by @DescribeSnapshots@
-- in paginated output. When this parameter is used, @DescribeSnapshots@
-- only returns @MaxResults@ results in a single page along with a
-- @NextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeSnapshots@ request with
-- the returned @NextToken@ value. This value can be between 5 and 1,000;
-- if @MaxResults@ is given a value larger than 1,000, only 1,000 results
-- are returned. If this parameter is not used, then @DescribeSnapshots@
-- returns all results. You cannot specify this parameter and the snapshot
-- IDs parameter in the same request.
--
-- 'restorableByUserIds', 'describeSnapshots_restorableByUserIds' - The IDs of the AWS accounts that can create volumes from the snapshot.
--
-- 'snapshotIds', 'describeSnapshots_snapshotIds' - The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume
-- permissions.
--
-- 'filters', 'describeSnapshots_filters' - The filters.
--
-- -   @description@ - A description of the snapshot.
--
-- -   @encrypted@ - Indicates whether the snapshot is encrypted (@true@ |
--     @false@)
--
-- -   @owner-alias@ - The owner alias, from an Amazon-maintained list
--     (@amazon@). This is not the user-configured AWS account alias set
--     using the IAM console. We recommend that you use the related
--     parameter instead of this filter.
--
-- -   @owner-id@ - The AWS account ID of the owner. We recommend that you
--     use the related parameter instead of this filter.
--
-- -   @progress@ - The progress of the snapshot, as a percentage (for
--     example, 80%).
--
-- -   @snapshot-id@ - The snapshot ID.
--
-- -   @start-time@ - The time stamp when the snapshot was initiated.
--
-- -   @status@ - The status of the snapshot (@pending@ | @completed@ |
--     @error@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @volume-id@ - The ID of the volume the snapshot is for.
--
-- -   @volume-size@ - The size of the volume, in GiB.
newDescribeSnapshots ::
  DescribeSnapshots
newDescribeSnapshots =
  DescribeSnapshots'
    { ownerIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      restorableByUserIds = Prelude.Nothing,
      snapshotIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | Scopes the results to snapshots with the specified owners. You can
-- specify a combination of AWS account IDs, @self@, and @amazon@.
describeSnapshots_ownerIds :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Prelude.Text])
describeSnapshots_ownerIds = Lens.lens (\DescribeSnapshots' {ownerIds} -> ownerIds) (\s@DescribeSnapshots' {} a -> s {ownerIds = a} :: DescribeSnapshots) Prelude.. Lens.mapping Lens._Coerce

-- | The @NextToken@ value returned from a previous paginated
-- @DescribeSnapshots@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
describeSnapshots_nextToken :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Text)
describeSnapshots_nextToken = Lens.lens (\DescribeSnapshots' {nextToken} -> nextToken) (\s@DescribeSnapshots' {} a -> s {nextToken = a} :: DescribeSnapshots)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSnapshots_dryRun :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Bool)
describeSnapshots_dryRun = Lens.lens (\DescribeSnapshots' {dryRun} -> dryRun) (\s@DescribeSnapshots' {} a -> s {dryRun = a} :: DescribeSnapshots)

-- | The maximum number of snapshot results returned by @DescribeSnapshots@
-- in paginated output. When this parameter is used, @DescribeSnapshots@
-- only returns @MaxResults@ results in a single page along with a
-- @NextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeSnapshots@ request with
-- the returned @NextToken@ value. This value can be between 5 and 1,000;
-- if @MaxResults@ is given a value larger than 1,000, only 1,000 results
-- are returned. If this parameter is not used, then @DescribeSnapshots@
-- returns all results. You cannot specify this parameter and the snapshot
-- IDs parameter in the same request.
describeSnapshots_maxResults :: Lens.Lens' DescribeSnapshots (Prelude.Maybe Prelude.Int)
describeSnapshots_maxResults = Lens.lens (\DescribeSnapshots' {maxResults} -> maxResults) (\s@DescribeSnapshots' {} a -> s {maxResults = a} :: DescribeSnapshots)

-- | The IDs of the AWS accounts that can create volumes from the snapshot.
describeSnapshots_restorableByUserIds :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Prelude.Text])
describeSnapshots_restorableByUserIds = Lens.lens (\DescribeSnapshots' {restorableByUserIds} -> restorableByUserIds) (\s@DescribeSnapshots' {} a -> s {restorableByUserIds = a} :: DescribeSnapshots) Prelude.. Lens.mapping Lens._Coerce

-- | The snapshot IDs.
--
-- Default: Describes the snapshots for which you have create volume
-- permissions.
describeSnapshots_snapshotIds :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Prelude.Text])
describeSnapshots_snapshotIds = Lens.lens (\DescribeSnapshots' {snapshotIds} -> snapshotIds) (\s@DescribeSnapshots' {} a -> s {snapshotIds = a} :: DescribeSnapshots) Prelude.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @description@ - A description of the snapshot.
--
-- -   @encrypted@ - Indicates whether the snapshot is encrypted (@true@ |
--     @false@)
--
-- -   @owner-alias@ - The owner alias, from an Amazon-maintained list
--     (@amazon@). This is not the user-configured AWS account alias set
--     using the IAM console. We recommend that you use the related
--     parameter instead of this filter.
--
-- -   @owner-id@ - The AWS account ID of the owner. We recommend that you
--     use the related parameter instead of this filter.
--
-- -   @progress@ - The progress of the snapshot, as a percentage (for
--     example, 80%).
--
-- -   @snapshot-id@ - The snapshot ID.
--
-- -   @start-time@ - The time stamp when the snapshot was initiated.
--
-- -   @status@ - The status of the snapshot (@pending@ | @completed@ |
--     @error@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @volume-id@ - The ID of the volume the snapshot is for.
--
-- -   @volume-size@ - The size of the volume, in GiB.
describeSnapshots_filters :: Lens.Lens' DescribeSnapshots (Prelude.Maybe [Filter])
describeSnapshots_filters = Lens.lens (\DescribeSnapshots' {filters} -> filters) (\s@DescribeSnapshots' {} a -> s {filters = a} :: DescribeSnapshots) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSnapshotsResponse'
            Prelude.<$> ( x Core..@? "snapshotSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshots

instance Prelude.NFData DescribeSnapshots

instance Core.ToHeaders DescribeSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSnapshots where
  toQuery DescribeSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeSnapshots" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Owner" Prelude.<$> ownerIds),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "RestorableBy"
              Prelude.<$> restorableByUserIds
          ),
        Core.toQuery
          ( Core.toQueryList "SnapshotId"
              Prelude.<$> snapshotIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | Information about the snapshots.
    snapshots :: Prelude.Maybe [Snapshot],
    -- | The @NextToken@ value to include in a future @DescribeSnapshots@
    -- request. When the results of a @DescribeSnapshots@ request exceed
    -- @MaxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'snapshots', 'describeSnapshotsResponse_snapshots' - Information about the snapshots.
--
-- 'nextToken', 'describeSnapshotsResponse_nextToken' - The @NextToken@ value to include in a future @DescribeSnapshots@
-- request. When the results of a @DescribeSnapshots@ request exceed
-- @MaxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
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

-- | Information about the snapshots.
describeSnapshotsResponse_snapshots :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe [Snapshot])
describeSnapshotsResponse_snapshots = Lens.lens (\DescribeSnapshotsResponse' {snapshots} -> snapshots) (\s@DescribeSnapshotsResponse' {} a -> s {snapshots = a} :: DescribeSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The @NextToken@ value to include in a future @DescribeSnapshots@
-- request. When the results of a @DescribeSnapshots@ request exceed
-- @MaxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeSnapshotsResponse_nextToken :: Lens.Lens' DescribeSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotsResponse_nextToken = Lens.lens (\DescribeSnapshotsResponse' {nextToken} -> nextToken) (\s@DescribeSnapshotsResponse' {} a -> s {nextToken = a} :: DescribeSnapshotsResponse)

-- | The response's http status code.
describeSnapshotsResponse_httpStatus :: Lens.Lens' DescribeSnapshotsResponse Prelude.Int
describeSnapshotsResponse_httpStatus = Lens.lens (\DescribeSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotsResponse)

instance Prelude.NFData DescribeSnapshotsResponse

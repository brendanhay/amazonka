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
-- Module      : Network.AWS.EC2.DescribeVolumesModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the most recent volume modification request for the specified
-- EBS volumes.
--
-- If a volume has never been modified, some information in the output will
-- be null. If a volume has been modified more than once, the output
-- includes only the most recent modification request.
--
-- You can also use CloudWatch Events to check the status of a modification
-- to an EBS volume. For information about CloudWatch Events, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring volume modifications>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumesModifications
  ( -- * Creating a Request
    DescribeVolumesModifications (..),
    newDescribeVolumesModifications,

    -- * Request Lenses
    describeVolumesModifications_nextToken,
    describeVolumesModifications_dryRun,
    describeVolumesModifications_volumeIds,
    describeVolumesModifications_maxResults,
    describeVolumesModifications_filters,

    -- * Destructuring the Response
    DescribeVolumesModificationsResponse (..),
    newDescribeVolumesModificationsResponse,

    -- * Response Lenses
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { -- | The @nextToken@ value returned by a previous paginated request.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the volumes.
    volumeIds :: Core.Maybe [Core.Text],
    -- | The maximum number of results (up to a limit of 500) to be returned in a
    -- paginated request.
    maxResults :: Core.Maybe Core.Int,
    -- | The filters.
    --
    -- -   @modification-state@ - The current modification state (modifying |
    --     optimizing | completed | failed).
    --
    -- -   @original-iops@ - The original IOPS rate of the volume.
    --
    -- -   @original-size@ - The original size of the volume, in GiB.
    --
    -- -   @original-volume-type@ - The original volume type of the volume
    --     (standard | io1 | io2 | gp2 | sc1 | st1).
    --
    -- -   @originalMultiAttachEnabled@ - Indicates whether Multi-Attach
    --     support was enabled (true | false).
    --
    -- -   @start-time@ - The modification start time.
    --
    -- -   @target-iops@ - The target IOPS rate of the volume.
    --
    -- -   @target-size@ - The target size of the volume, in GiB.
    --
    -- -   @target-volume-type@ - The target volume type of the volume
    --     (standard | io1 | io2 | gp2 | sc1 | st1).
    --
    -- -   @targetMultiAttachEnabled@ - Indicates whether Multi-Attach support
    --     is to be enabled (true | false).
    --
    -- -   @volume-id@ - The ID of the volume.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumesModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVolumesModifications_nextToken' - The @nextToken@ value returned by a previous paginated request.
--
-- 'dryRun', 'describeVolumesModifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'volumeIds', 'describeVolumesModifications_volumeIds' - The IDs of the volumes.
--
-- 'maxResults', 'describeVolumesModifications_maxResults' - The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
--
-- 'filters', 'describeVolumesModifications_filters' - The filters.
--
-- -   @modification-state@ - The current modification state (modifying |
--     optimizing | completed | failed).
--
-- -   @original-iops@ - The original IOPS rate of the volume.
--
-- -   @original-size@ - The original size of the volume, in GiB.
--
-- -   @original-volume-type@ - The original volume type of the volume
--     (standard | io1 | io2 | gp2 | sc1 | st1).
--
-- -   @originalMultiAttachEnabled@ - Indicates whether Multi-Attach
--     support was enabled (true | false).
--
-- -   @start-time@ - The modification start time.
--
-- -   @target-iops@ - The target IOPS rate of the volume.
--
-- -   @target-size@ - The target size of the volume, in GiB.
--
-- -   @target-volume-type@ - The target volume type of the volume
--     (standard | io1 | io2 | gp2 | sc1 | st1).
--
-- -   @targetMultiAttachEnabled@ - Indicates whether Multi-Attach support
--     is to be enabled (true | false).
--
-- -   @volume-id@ - The ID of the volume.
newDescribeVolumesModifications ::
  DescribeVolumesModifications
newDescribeVolumesModifications =
  DescribeVolumesModifications'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      volumeIds = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The @nextToken@ value returned by a previous paginated request.
describeVolumesModifications_nextToken :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Core.Text)
describeVolumesModifications_nextToken = Lens.lens (\DescribeVolumesModifications' {nextToken} -> nextToken) (\s@DescribeVolumesModifications' {} a -> s {nextToken = a} :: DescribeVolumesModifications)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumesModifications_dryRun :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Core.Bool)
describeVolumesModifications_dryRun = Lens.lens (\DescribeVolumesModifications' {dryRun} -> dryRun) (\s@DescribeVolumesModifications' {} a -> s {dryRun = a} :: DescribeVolumesModifications)

-- | The IDs of the volumes.
describeVolumesModifications_volumeIds :: Lens.Lens' DescribeVolumesModifications (Core.Maybe [Core.Text])
describeVolumesModifications_volumeIds = Lens.lens (\DescribeVolumesModifications' {volumeIds} -> volumeIds) (\s@DescribeVolumesModifications' {} a -> s {volumeIds = a} :: DescribeVolumesModifications) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
describeVolumesModifications_maxResults :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Core.Int)
describeVolumesModifications_maxResults = Lens.lens (\DescribeVolumesModifications' {maxResults} -> maxResults) (\s@DescribeVolumesModifications' {} a -> s {maxResults = a} :: DescribeVolumesModifications)

-- | The filters.
--
-- -   @modification-state@ - The current modification state (modifying |
--     optimizing | completed | failed).
--
-- -   @original-iops@ - The original IOPS rate of the volume.
--
-- -   @original-size@ - The original size of the volume, in GiB.
--
-- -   @original-volume-type@ - The original volume type of the volume
--     (standard | io1 | io2 | gp2 | sc1 | st1).
--
-- -   @originalMultiAttachEnabled@ - Indicates whether Multi-Attach
--     support was enabled (true | false).
--
-- -   @start-time@ - The modification start time.
--
-- -   @target-iops@ - The target IOPS rate of the volume.
--
-- -   @target-size@ - The target size of the volume, in GiB.
--
-- -   @target-volume-type@ - The target volume type of the volume
--     (standard | io1 | io2 | gp2 | sc1 | st1).
--
-- -   @targetMultiAttachEnabled@ - Indicates whether Multi-Attach support
--     is to be enabled (true | false).
--
-- -   @volume-id@ - The ID of the volume.
describeVolumesModifications_filters :: Lens.Lens' DescribeVolumesModifications (Core.Maybe [Filter])
describeVolumesModifications_filters = Lens.lens (\DescribeVolumesModifications' {filters} -> filters) (\s@DescribeVolumesModifications' {} a -> s {filters = a} :: DescribeVolumesModifications) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeVolumesModifications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVolumesModificationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVolumesModificationsResponse_volumesModifications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVolumesModifications_nextToken
          Lens..~ rs
          Lens.^? describeVolumesModificationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeVolumesModifications where
  type
    AWSResponse DescribeVolumesModifications =
      DescribeVolumesModificationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesModificationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "volumeModificationSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVolumesModifications

instance Core.NFData DescribeVolumesModifications

instance Core.ToHeaders DescribeVolumesModifications where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVolumesModifications where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVolumesModifications where
  toQuery DescribeVolumesModifications' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVolumesModifications" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "VolumeId" Core.<$> volumeIds),
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { -- | Token for pagination, null if there are no more results
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the volume modifications.
    volumesModifications :: Core.Maybe [VolumeModification],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVolumesModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVolumesModificationsResponse_nextToken' - Token for pagination, null if there are no more results
--
-- 'volumesModifications', 'describeVolumesModificationsResponse_volumesModifications' - Information about the volume modifications.
--
-- 'httpStatus', 'describeVolumesModificationsResponse_httpStatus' - The response's http status code.
newDescribeVolumesModificationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVolumesModificationsResponse
newDescribeVolumesModificationsResponse pHttpStatus_ =
  DescribeVolumesModificationsResponse'
    { nextToken =
        Core.Nothing,
      volumesModifications = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for pagination, null if there are no more results
describeVolumesModificationsResponse_nextToken :: Lens.Lens' DescribeVolumesModificationsResponse (Core.Maybe Core.Text)
describeVolumesModificationsResponse_nextToken = Lens.lens (\DescribeVolumesModificationsResponse' {nextToken} -> nextToken) (\s@DescribeVolumesModificationsResponse' {} a -> s {nextToken = a} :: DescribeVolumesModificationsResponse)

-- | Information about the volume modifications.
describeVolumesModificationsResponse_volumesModifications :: Lens.Lens' DescribeVolumesModificationsResponse (Core.Maybe [VolumeModification])
describeVolumesModificationsResponse_volumesModifications = Lens.lens (\DescribeVolumesModificationsResponse' {volumesModifications} -> volumesModifications) (\s@DescribeVolumesModificationsResponse' {} a -> s {volumesModifications = a} :: DescribeVolumesModificationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVolumesModificationsResponse_httpStatus :: Lens.Lens' DescribeVolumesModificationsResponse Core.Int
describeVolumesModificationsResponse_httpStatus = Lens.lens (\DescribeVolumesModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesModificationsResponse' {} a -> s {httpStatus = a} :: DescribeVolumesModificationsResponse)

instance
  Core.NFData
    DescribeVolumesModificationsResponse

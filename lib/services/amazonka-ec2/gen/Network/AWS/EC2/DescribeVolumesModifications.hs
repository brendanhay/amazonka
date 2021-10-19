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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-modifications.html Monitor the progress of volume modifications>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumesModifications
  ( -- * Creating a Request
    DescribeVolumesModifications (..),
    newDescribeVolumesModifications,

    -- * Request Lenses
    describeVolumesModifications_filters,
    describeVolumesModifications_volumeIds,
    describeVolumesModifications_nextToken,
    describeVolumesModifications_dryRun,
    describeVolumesModifications_maxResults,

    -- * Destructuring the Response
    DescribeVolumesModificationsResponse (..),
    newDescribeVolumesModificationsResponse,

    -- * Response Lenses
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { -- | The filters.
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
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the volumes.
    volumeIds :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ value returned by a previous paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results (up to a limit of 500) to be returned in a
    -- paginated request.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumesModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'volumeIds', 'describeVolumesModifications_volumeIds' - The IDs of the volumes.
--
-- 'nextToken', 'describeVolumesModifications_nextToken' - The @nextToken@ value returned by a previous paginated request.
--
-- 'dryRun', 'describeVolumesModifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVolumesModifications_maxResults' - The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
newDescribeVolumesModifications ::
  DescribeVolumesModifications
newDescribeVolumesModifications =
  DescribeVolumesModifications'
    { filters =
        Prelude.Nothing,
      volumeIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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
describeVolumesModifications_filters :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe [Filter])
describeVolumesModifications_filters = Lens.lens (\DescribeVolumesModifications' {filters} -> filters) (\s@DescribeVolumesModifications' {} a -> s {filters = a} :: DescribeVolumesModifications) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the volumes.
describeVolumesModifications_volumeIds :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe [Prelude.Text])
describeVolumesModifications_volumeIds = Lens.lens (\DescribeVolumesModifications' {volumeIds} -> volumeIds) (\s@DescribeVolumesModifications' {} a -> s {volumeIds = a} :: DescribeVolumesModifications) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned by a previous paginated request.
describeVolumesModifications_nextToken :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Text)
describeVolumesModifications_nextToken = Lens.lens (\DescribeVolumesModifications' {nextToken} -> nextToken) (\s@DescribeVolumesModifications' {} a -> s {nextToken = a} :: DescribeVolumesModifications)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumesModifications_dryRun :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Bool)
describeVolumesModifications_dryRun = Lens.lens (\DescribeVolumesModifications' {dryRun} -> dryRun) (\s@DescribeVolumesModifications' {} a -> s {dryRun = a} :: DescribeVolumesModifications)

-- | The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
describeVolumesModifications_maxResults :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Int)
describeVolumesModifications_maxResults = Lens.lens (\DescribeVolumesModifications' {maxResults} -> maxResults) (\s@DescribeVolumesModifications' {} a -> s {maxResults = a} :: DescribeVolumesModifications)

instance Core.AWSPager DescribeVolumesModifications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVolumesModificationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVolumesModificationsResponse_volumesModifications
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVolumesModifications_nextToken
          Lens..~ rs
          Lens.^? describeVolumesModificationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeVolumesModifications where
  type
    AWSResponse DescribeVolumesModifications =
      DescribeVolumesModificationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesModificationsResponse'
            Prelude.<$> ( x Core..@? "volumeModificationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVolumesModifications

instance Prelude.NFData DescribeVolumesModifications

instance Core.ToHeaders DescribeVolumesModifications where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeVolumesModifications where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeVolumesModifications where
  toQuery DescribeVolumesModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeVolumesModifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        Core.toQuery
          (Core.toQueryList "VolumeId" Prelude.<$> volumeIds),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { -- | Information about the volume modifications.
    volumesModifications :: Prelude.Maybe [VolumeModification],
    -- | Token for pagination, null if there are no more results
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumesModificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumesModifications', 'describeVolumesModificationsResponse_volumesModifications' - Information about the volume modifications.
--
-- 'nextToken', 'describeVolumesModificationsResponse_nextToken' - Token for pagination, null if there are no more results
--
-- 'httpStatus', 'describeVolumesModificationsResponse_httpStatus' - The response's http status code.
newDescribeVolumesModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVolumesModificationsResponse
newDescribeVolumesModificationsResponse pHttpStatus_ =
  DescribeVolumesModificationsResponse'
    { volumesModifications =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the volume modifications.
describeVolumesModificationsResponse_volumesModifications :: Lens.Lens' DescribeVolumesModificationsResponse (Prelude.Maybe [VolumeModification])
describeVolumesModificationsResponse_volumesModifications = Lens.lens (\DescribeVolumesModificationsResponse' {volumesModifications} -> volumesModifications) (\s@DescribeVolumesModificationsResponse' {} a -> s {volumesModifications = a} :: DescribeVolumesModificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for pagination, null if there are no more results
describeVolumesModificationsResponse_nextToken :: Lens.Lens' DescribeVolumesModificationsResponse (Prelude.Maybe Prelude.Text)
describeVolumesModificationsResponse_nextToken = Lens.lens (\DescribeVolumesModificationsResponse' {nextToken} -> nextToken) (\s@DescribeVolumesModificationsResponse' {} a -> s {nextToken = a} :: DescribeVolumesModificationsResponse)

-- | The response's http status code.
describeVolumesModificationsResponse_httpStatus :: Lens.Lens' DescribeVolumesModificationsResponse Prelude.Int
describeVolumesModificationsResponse_httpStatus = Lens.lens (\DescribeVolumesModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesModificationsResponse' {} a -> s {httpStatus = a} :: DescribeVolumesModificationsResponse)

instance
  Prelude.NFData
    DescribeVolumesModificationsResponse

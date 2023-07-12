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
-- Module      : Amazonka.EC2.DescribeVolumesModifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.DescribeVolumesModifications
  ( -- * Creating a Request
    DescribeVolumesModifications (..),
    newDescribeVolumesModifications,

    -- * Request Lenses
    describeVolumesModifications_dryRun,
    describeVolumesModifications_filters,
    describeVolumesModifications_maxResults,
    describeVolumesModifications_nextToken,
    describeVolumesModifications_volumeIds,

    -- * Destructuring the Response
    DescribeVolumesModificationsResponse (..),
    newDescribeVolumesModificationsResponse,

    -- * Response Lenses
    describeVolumesModificationsResponse_nextToken,
    describeVolumesModificationsResponse_volumesModifications,
    describeVolumesModificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results (up to a limit of 500) to be returned in a
    -- paginated request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @nextToken@ value returned by a previous paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the volumes.
    volumeIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeVolumesModifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'maxResults', 'describeVolumesModifications_maxResults' - The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
--
-- 'nextToken', 'describeVolumesModifications_nextToken' - The @nextToken@ value returned by a previous paginated request.
--
-- 'volumeIds', 'describeVolumesModifications_volumeIds' - The IDs of the volumes.
newDescribeVolumesModifications ::
  DescribeVolumesModifications
newDescribeVolumesModifications =
  DescribeVolumesModifications'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      volumeIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumesModifications_dryRun :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Bool)
describeVolumesModifications_dryRun = Lens.lens (\DescribeVolumesModifications' {dryRun} -> dryRun) (\s@DescribeVolumesModifications' {} a -> s {dryRun = a} :: DescribeVolumesModifications)

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

-- | The maximum number of results (up to a limit of 500) to be returned in a
-- paginated request.
describeVolumesModifications_maxResults :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Int)
describeVolumesModifications_maxResults = Lens.lens (\DescribeVolumesModifications' {maxResults} -> maxResults) (\s@DescribeVolumesModifications' {} a -> s {maxResults = a} :: DescribeVolumesModifications)

-- | The @nextToken@ value returned by a previous paginated request.
describeVolumesModifications_nextToken :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe Prelude.Text)
describeVolumesModifications_nextToken = Lens.lens (\DescribeVolumesModifications' {nextToken} -> nextToken) (\s@DescribeVolumesModifications' {} a -> s {nextToken = a} :: DescribeVolumesModifications)

-- | The IDs of the volumes.
describeVolumesModifications_volumeIds :: Lens.Lens' DescribeVolumesModifications (Prelude.Maybe [Prelude.Text])
describeVolumesModifications_volumeIds = Lens.lens (\DescribeVolumesModifications' {volumeIds} -> volumeIds) (\s@DescribeVolumesModifications' {} a -> s {volumeIds = a} :: DescribeVolumesModifications) Prelude.. Lens.mapping Lens.coerced

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVolumesModifications_nextToken
          Lens..~ rs
          Lens.^? describeVolumesModificationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeVolumesModifications where
  type
    AWSResponse DescribeVolumesModifications =
      DescribeVolumesModificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesModificationsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "volumeModificationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVolumesModifications
  where
  hashWithSalt _salt DescribeVolumesModifications' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` volumeIds

instance Prelude.NFData DescribeVolumesModifications where
  rnf DescribeVolumesModifications' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf volumeIds

instance Data.ToHeaders DescribeVolumesModifications where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVolumesModifications where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVolumesModifications where
  toQuery DescribeVolumesModifications' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVolumesModifications" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "VolumeId" Prelude.<$> volumeIds)
      ]

-- | /See:/ 'newDescribeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { -- | Token for pagination, null if there are no more results
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the volume modifications.
    volumesModifications :: Prelude.Maybe [VolumeModification],
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
-- 'nextToken', 'describeVolumesModificationsResponse_nextToken' - Token for pagination, null if there are no more results
--
-- 'volumesModifications', 'describeVolumesModificationsResponse_volumesModifications' - Information about the volume modifications.
--
-- 'httpStatus', 'describeVolumesModificationsResponse_httpStatus' - The response's http status code.
newDescribeVolumesModificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVolumesModificationsResponse
newDescribeVolumesModificationsResponse pHttpStatus_ =
  DescribeVolumesModificationsResponse'
    { nextToken =
        Prelude.Nothing,
      volumesModifications =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for pagination, null if there are no more results
describeVolumesModificationsResponse_nextToken :: Lens.Lens' DescribeVolumesModificationsResponse (Prelude.Maybe Prelude.Text)
describeVolumesModificationsResponse_nextToken = Lens.lens (\DescribeVolumesModificationsResponse' {nextToken} -> nextToken) (\s@DescribeVolumesModificationsResponse' {} a -> s {nextToken = a} :: DescribeVolumesModificationsResponse)

-- | Information about the volume modifications.
describeVolumesModificationsResponse_volumesModifications :: Lens.Lens' DescribeVolumesModificationsResponse (Prelude.Maybe [VolumeModification])
describeVolumesModificationsResponse_volumesModifications = Lens.lens (\DescribeVolumesModificationsResponse' {volumesModifications} -> volumesModifications) (\s@DescribeVolumesModificationsResponse' {} a -> s {volumesModifications = a} :: DescribeVolumesModificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVolumesModificationsResponse_httpStatus :: Lens.Lens' DescribeVolumesModificationsResponse Prelude.Int
describeVolumesModificationsResponse_httpStatus = Lens.lens (\DescribeVolumesModificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesModificationsResponse' {} a -> s {httpStatus = a} :: DescribeVolumesModificationsResponse)

instance
  Prelude.NFData
    DescribeVolumesModificationsResponse
  where
  rnf DescribeVolumesModificationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf volumesModifications
      `Prelude.seq` Prelude.rnf httpStatus

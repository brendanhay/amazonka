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
-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS volumes or all of your EBS volumes.
--
-- If you are describing a long list of volumes, we recommend that you
-- paginate the output to make the list more manageable. The @MaxResults@
-- parameter sets the maximum number of results returned in a single page.
-- If the list of results exceeds your @MaxResults@ value, then that number
-- of results is returned along with a @NextToken@ value that can be passed
-- to a subsequent @DescribeVolumes@ request to retrieve the remaining
-- results.
--
-- For more information about EBS volumes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumes
  ( -- * Creating a Request
    DescribeVolumes (..),
    newDescribeVolumes,

    -- * Request Lenses
    describeVolumes_nextToken,
    describeVolumes_dryRun,
    describeVolumes_volumeIds,
    describeVolumes_maxResults,
    describeVolumes_filters,

    -- * Destructuring the Response
    DescribeVolumesResponse (..),
    newDescribeVolumesResponse,

    -- * Response Lenses
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The @NextToken@ value returned from a previous paginated
    -- @DescribeVolumes@ request where @MaxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @NextToken@ value. This value
    -- is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The volume IDs.
    volumeIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of volume results returned by @DescribeVolumes@ in
    -- paginated output. When this parameter is used, @DescribeVolumes@ only
    -- returns @MaxResults@ results in a single page along with a @NextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @DescribeVolumes@ request with the returned
    -- @NextToken@ value. This value can be between 5 and 500; if @MaxResults@
    -- is given a value larger than 500, only 500 results are returned. If this
    -- parameter is not used, then @DescribeVolumes@ returns all results. You
    -- cannot specify this parameter and the volume IDs parameter in the same
    -- request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The filters.
    --
    -- -   @attachment.attach-time@ - The time stamp when the attachment
    --     initiated.
    --
    -- -   @attachment.delete-on-termination@ - Whether the volume is deleted
    --     on instance termination.
    --
    -- -   @attachment.device@ - The device name specified in the block device
    --     mapping (for example, @\/dev\/sda1@).
    --
    -- -   @attachment.instance-id@ - The ID of the instance the volume is
    --     attached to.
    --
    -- -   @attachment.status@ - The attachment state (@attaching@ | @attached@
    --     | @detaching@).
    --
    -- -   @availability-zone@ - The Availability Zone in which the volume was
    --     created.
    --
    -- -   @create-time@ - The time stamp when the volume was created.
    --
    -- -   @encrypted@ - Indicates whether the volume is encrypted (@true@ |
    --     @false@)
    --
    -- -   @multi-attach-enabled@ - Indicates whether the volume is enabled for
    --     Multi-Attach (@true@ | @false@)
    --
    -- -   @fast-restored@ - Indicates whether the volume was created from a
    --     snapshot that is enabled for fast snapshot restore (@true@ |
    --     @false@).
    --
    -- -   @size@ - The size of the volume, in GiB.
    --
    -- -   @snapshot-id@ - The snapshot from which the volume was created.
    --
    -- -   @status@ - The state of the volume (@creating@ | @available@ |
    --     @in-use@ | @deleting@ | @deleted@ | @error@).
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
    -- -   @volume-id@ - The volume ID.
    --
    -- -   @volume-type@ - The Amazon EBS volume type (@gp2@ | @gp3@ | @io1@ |
    --     @io2@ | @st1@ | @sc1@| @standard@)
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVolumes_nextToken' - The @NextToken@ value returned from a previous paginated
-- @DescribeVolumes@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
--
-- 'dryRun', 'describeVolumes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'volumeIds', 'describeVolumes_volumeIds' - The volume IDs.
--
-- 'maxResults', 'describeVolumes_maxResults' - The maximum number of volume results returned by @DescribeVolumes@ in
-- paginated output. When this parameter is used, @DescribeVolumes@ only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeVolumes@ request with the returned
-- @NextToken@ value. This value can be between 5 and 500; if @MaxResults@
-- is given a value larger than 500, only 500 results are returned. If this
-- parameter is not used, then @DescribeVolumes@ returns all results. You
-- cannot specify this parameter and the volume IDs parameter in the same
-- request.
--
-- 'filters', 'describeVolumes_filters' - The filters.
--
-- -   @attachment.attach-time@ - The time stamp when the attachment
--     initiated.
--
-- -   @attachment.delete-on-termination@ - Whether the volume is deleted
--     on instance termination.
--
-- -   @attachment.device@ - The device name specified in the block device
--     mapping (for example, @\/dev\/sda1@).
--
-- -   @attachment.instance-id@ - The ID of the instance the volume is
--     attached to.
--
-- -   @attachment.status@ - The attachment state (@attaching@ | @attached@
--     | @detaching@).
--
-- -   @availability-zone@ - The Availability Zone in which the volume was
--     created.
--
-- -   @create-time@ - The time stamp when the volume was created.
--
-- -   @encrypted@ - Indicates whether the volume is encrypted (@true@ |
--     @false@)
--
-- -   @multi-attach-enabled@ - Indicates whether the volume is enabled for
--     Multi-Attach (@true@ | @false@)
--
-- -   @fast-restored@ - Indicates whether the volume was created from a
--     snapshot that is enabled for fast snapshot restore (@true@ |
--     @false@).
--
-- -   @size@ - The size of the volume, in GiB.
--
-- -   @snapshot-id@ - The snapshot from which the volume was created.
--
-- -   @status@ - The state of the volume (@creating@ | @available@ |
--     @in-use@ | @deleting@ | @deleted@ | @error@).
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
-- -   @volume-id@ - The volume ID.
--
-- -   @volume-type@ - The Amazon EBS volume type (@gp2@ | @gp3@ | @io1@ |
--     @io2@ | @st1@ | @sc1@| @standard@)
newDescribeVolumes ::
  DescribeVolumes
newDescribeVolumes =
  DescribeVolumes'
    { nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      volumeIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The @NextToken@ value returned from a previous paginated
-- @DescribeVolumes@ request where @MaxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @NextToken@ value. This value
-- is @null@ when there are no more results to return.
describeVolumes_nextToken :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Text)
describeVolumes_nextToken = Lens.lens (\DescribeVolumes' {nextToken} -> nextToken) (\s@DescribeVolumes' {} a -> s {nextToken = a} :: DescribeVolumes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumes_dryRun :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Bool)
describeVolumes_dryRun = Lens.lens (\DescribeVolumes' {dryRun} -> dryRun) (\s@DescribeVolumes' {} a -> s {dryRun = a} :: DescribeVolumes)

-- | The volume IDs.
describeVolumes_volumeIds :: Lens.Lens' DescribeVolumes (Prelude.Maybe [Prelude.Text])
describeVolumes_volumeIds = Lens.lens (\DescribeVolumes' {volumeIds} -> volumeIds) (\s@DescribeVolumes' {} a -> s {volumeIds = a} :: DescribeVolumes) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of volume results returned by @DescribeVolumes@ in
-- paginated output. When this parameter is used, @DescribeVolumes@ only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeVolumes@ request with the returned
-- @NextToken@ value. This value can be between 5 and 500; if @MaxResults@
-- is given a value larger than 500, only 500 results are returned. If this
-- parameter is not used, then @DescribeVolumes@ returns all results. You
-- cannot specify this parameter and the volume IDs parameter in the same
-- request.
describeVolumes_maxResults :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Int)
describeVolumes_maxResults = Lens.lens (\DescribeVolumes' {maxResults} -> maxResults) (\s@DescribeVolumes' {} a -> s {maxResults = a} :: DescribeVolumes)

-- | The filters.
--
-- -   @attachment.attach-time@ - The time stamp when the attachment
--     initiated.
--
-- -   @attachment.delete-on-termination@ - Whether the volume is deleted
--     on instance termination.
--
-- -   @attachment.device@ - The device name specified in the block device
--     mapping (for example, @\/dev\/sda1@).
--
-- -   @attachment.instance-id@ - The ID of the instance the volume is
--     attached to.
--
-- -   @attachment.status@ - The attachment state (@attaching@ | @attached@
--     | @detaching@).
--
-- -   @availability-zone@ - The Availability Zone in which the volume was
--     created.
--
-- -   @create-time@ - The time stamp when the volume was created.
--
-- -   @encrypted@ - Indicates whether the volume is encrypted (@true@ |
--     @false@)
--
-- -   @multi-attach-enabled@ - Indicates whether the volume is enabled for
--     Multi-Attach (@true@ | @false@)
--
-- -   @fast-restored@ - Indicates whether the volume was created from a
--     snapshot that is enabled for fast snapshot restore (@true@ |
--     @false@).
--
-- -   @size@ - The size of the volume, in GiB.
--
-- -   @snapshot-id@ - The snapshot from which the volume was created.
--
-- -   @status@ - The state of the volume (@creating@ | @available@ |
--     @in-use@ | @deleting@ | @deleted@ | @error@).
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
-- -   @volume-id@ - The volume ID.
--
-- -   @volume-type@ - The Amazon EBS volume type (@gp2@ | @gp3@ | @io1@ |
--     @io2@ | @st1@ | @sc1@| @standard@)
describeVolumes_filters :: Lens.Lens' DescribeVolumes (Prelude.Maybe [Filter])
describeVolumes_filters = Lens.lens (\DescribeVolumes' {filters} -> filters) (\s@DescribeVolumes' {} a -> s {filters = a} :: DescribeVolumes) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeVolumes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVolumesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVolumesResponse_volumes Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVolumes_nextToken
          Lens..~ rs
          Lens.^? describeVolumesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeVolumes where
  type
    AWSResponse DescribeVolumes =
      DescribeVolumesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "volumeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVolumes

instance Prelude.NFData DescribeVolumes

instance Core.ToHeaders DescribeVolumes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeVolumes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeVolumes where
  toQuery DescribeVolumes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeVolumes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "VolumeId" Prelude.<$> volumeIds),
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | The @NextToken@ value to include in a future @DescribeVolumes@ request.
    -- When the results of a @DescribeVolumes@ request exceed @MaxResults@,
    -- this value can be used to retrieve the next page of results. This value
    -- is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the volumes.
    volumes :: Prelude.Maybe [Volume],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVolumesResponse_nextToken' - The @NextToken@ value to include in a future @DescribeVolumes@ request.
-- When the results of a @DescribeVolumes@ request exceed @MaxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
--
-- 'volumes', 'describeVolumesResponse_volumes' - Information about the volumes.
--
-- 'httpStatus', 'describeVolumesResponse_httpStatus' - The response's http status code.
newDescribeVolumesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVolumesResponse
newDescribeVolumesResponse pHttpStatus_ =
  DescribeVolumesResponse'
    { nextToken =
        Prelude.Nothing,
      volumes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @NextToken@ value to include in a future @DescribeVolumes@ request.
-- When the results of a @DescribeVolumes@ request exceed @MaxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
describeVolumesResponse_nextToken :: Lens.Lens' DescribeVolumesResponse (Prelude.Maybe Prelude.Text)
describeVolumesResponse_nextToken = Lens.lens (\DescribeVolumesResponse' {nextToken} -> nextToken) (\s@DescribeVolumesResponse' {} a -> s {nextToken = a} :: DescribeVolumesResponse)

-- | Information about the volumes.
describeVolumesResponse_volumes :: Lens.Lens' DescribeVolumesResponse (Prelude.Maybe [Volume])
describeVolumesResponse_volumes = Lens.lens (\DescribeVolumesResponse' {volumes} -> volumes) (\s@DescribeVolumesResponse' {} a -> s {volumes = a} :: DescribeVolumesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVolumesResponse_httpStatus :: Lens.Lens' DescribeVolumesResponse Prelude.Int
describeVolumesResponse_httpStatus = Lens.lens (\DescribeVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesResponse' {} a -> s {httpStatus = a} :: DescribeVolumesResponse)

instance Prelude.NFData DescribeVolumesResponse

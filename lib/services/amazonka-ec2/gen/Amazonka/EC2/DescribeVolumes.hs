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
-- Module      : Amazonka.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS volumes or all of your EBS volumes.
--
-- If you are describing a long list of volumes, we recommend that you
-- paginate the output to make the list more manageable. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- For more information about EBS volumes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVolumes
  ( -- * Creating a Request
    DescribeVolumes (..),
    newDescribeVolumes,

    -- * Request Lenses
    describeVolumes_dryRun,
    describeVolumes_filters,
    describeVolumes_maxResults,
    describeVolumes_nextToken,
    describeVolumes_volumeIds,

    -- * Destructuring the Response
    DescribeVolumesResponse (..),
    newDescribeVolumesResponse,

    -- * Response Lenses
    describeVolumesResponse_nextToken,
    describeVolumesResponse_volumes,
    describeVolumesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of volumes to return for this request. This value can
    -- be between 5 and 500; if you specify a value larger than 500, only 500
    -- items are returned. If this parameter is not used, then all items are
    -- returned. You cannot specify this parameter and the volume IDs parameter
    -- in the same request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned from the previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The volume IDs.
    volumeIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeVolumes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
--
-- 'maxResults', 'describeVolumes_maxResults' - The maximum number of volumes to return for this request. This value can
-- be between 5 and 500; if you specify a value larger than 500, only 500
-- items are returned. If this parameter is not used, then all items are
-- returned. You cannot specify this parameter and the volume IDs parameter
-- in the same request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeVolumes_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned from the previous request.
--
-- 'volumeIds', 'describeVolumes_volumeIds' - The volume IDs.
newDescribeVolumes ::
  DescribeVolumes
newDescribeVolumes =
  DescribeVolumes'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      volumeIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumes_dryRun :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Bool)
describeVolumes_dryRun = Lens.lens (\DescribeVolumes' {dryRun} -> dryRun) (\s@DescribeVolumes' {} a -> s {dryRun = a} :: DescribeVolumes)

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
describeVolumes_filters = Lens.lens (\DescribeVolumes' {filters} -> filters) (\s@DescribeVolumes' {} a -> s {filters = a} :: DescribeVolumes) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of volumes to return for this request. This value can
-- be between 5 and 500; if you specify a value larger than 500, only 500
-- items are returned. If this parameter is not used, then all items are
-- returned. You cannot specify this parameter and the volume IDs parameter
-- in the same request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeVolumes_maxResults :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Int)
describeVolumes_maxResults = Lens.lens (\DescribeVolumes' {maxResults} -> maxResults) (\s@DescribeVolumes' {} a -> s {maxResults = a} :: DescribeVolumes)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned from the previous request.
describeVolumes_nextToken :: Lens.Lens' DescribeVolumes (Prelude.Maybe Prelude.Text)
describeVolumes_nextToken = Lens.lens (\DescribeVolumes' {nextToken} -> nextToken) (\s@DescribeVolumes' {} a -> s {nextToken = a} :: DescribeVolumes)

-- | The volume IDs.
describeVolumes_volumeIds :: Lens.Lens' DescribeVolumes (Prelude.Maybe [Prelude.Text])
describeVolumes_volumeIds = Lens.lens (\DescribeVolumes' {volumeIds} -> volumeIds) (\s@DescribeVolumes' {} a -> s {volumeIds = a} :: DescribeVolumes) Prelude.. Lens.mapping Lens.coerced

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
            Lens.^? describeVolumesResponse_volumes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVolumes_nextToken
          Lens..~ rs
          Lens.^? describeVolumesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeVolumes where
  type
    AWSResponse DescribeVolumes =
      DescribeVolumesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "volumeSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVolumes where
  hashWithSalt _salt DescribeVolumes' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` volumeIds

instance Prelude.NFData DescribeVolumes where
  rnf DescribeVolumes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf volumeIds

instance Data.ToHeaders DescribeVolumes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVolumes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVolumes where
  toQuery DescribeVolumes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVolumes" :: Prelude.ByteString),
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

-- | /See:/ 'newDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
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
-- 'nextToken', 'describeVolumesResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
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

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeVolumesResponse_nextToken :: Lens.Lens' DescribeVolumesResponse (Prelude.Maybe Prelude.Text)
describeVolumesResponse_nextToken = Lens.lens (\DescribeVolumesResponse' {nextToken} -> nextToken) (\s@DescribeVolumesResponse' {} a -> s {nextToken = a} :: DescribeVolumesResponse)

-- | Information about the volumes.
describeVolumesResponse_volumes :: Lens.Lens' DescribeVolumesResponse (Prelude.Maybe [Volume])
describeVolumesResponse_volumes = Lens.lens (\DescribeVolumesResponse' {volumes} -> volumes) (\s@DescribeVolumesResponse' {} a -> s {volumes = a} :: DescribeVolumesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVolumesResponse_httpStatus :: Lens.Lens' DescribeVolumesResponse Prelude.Int
describeVolumesResponse_httpStatus = Lens.lens (\DescribeVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumesResponse' {} a -> s {httpStatus = a} :: DescribeVolumesResponse)

instance Prelude.NFData DescribeVolumesResponse where
  rnf DescribeVolumesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf httpStatus

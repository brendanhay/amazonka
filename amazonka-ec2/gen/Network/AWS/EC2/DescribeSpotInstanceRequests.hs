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
-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Spot Instance requests.
--
-- You can use @DescribeSpotInstanceRequests@ to find a running Spot
-- Instance by examining the response. If the status of the Spot Instance
-- is @fulfilled@, the instance ID appears in the response and contains the
-- identifier of the instance. Alternatively, you can use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances DescribeInstances>
-- with a filter to look for instances where the instance lifecycle is
-- @spot@.
--
-- We recommend that you set @MaxResults@ to a value between 5 and 1000 to
-- limit the number of results returned. This paginates the output, which
-- makes the list more manageable and returns the results faster. If the
-- list of results exceeds your @MaxResults@ value, then that number of
-- results is returned along with a @NextToken@ value that can be passed to
-- a subsequent @DescribeSpotInstanceRequests@ request to retrieve the
-- remaining results.
--
-- Spot Instance requests are deleted four hours after they are canceled
-- and their instances are terminated.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotInstanceRequests
  ( -- * Creating a Request
    DescribeSpotInstanceRequests (..),
    newDescribeSpotInstanceRequests,

    -- * Request Lenses
    describeSpotInstanceRequests_nextToken,
    describeSpotInstanceRequests_dryRun,
    describeSpotInstanceRequests_maxResults,
    describeSpotInstanceRequests_spotInstanceRequestIds,
    describeSpotInstanceRequests_filters,

    -- * Destructuring the Response
    DescribeSpotInstanceRequestsResponse (..),
    newDescribeSpotInstanceRequestsResponse,

    -- * Response Lenses
    describeSpotInstanceRequestsResponse_nextToken,
    describeSpotInstanceRequestsResponse_spotInstanceRequests,
    describeSpotInstanceRequestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotInstanceRequests.
--
-- /See:/ 'newDescribeSpotInstanceRequests' smart constructor.
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests'
  { -- | The token to request the next set of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 5 and 1000. To retrieve the remaining results, make
    -- another call with the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | One or more Spot Instance request IDs.
    spotInstanceRequestIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
    --
    -- -   @availability-zone-group@ - The Availability Zone group.
    --
    -- -   @create-time@ - The time stamp when the Spot Instance request was
    --     created.
    --
    -- -   @fault-code@ - The fault code related to the request.
    --
    -- -   @fault-message@ - The fault message related to the request.
    --
    -- -   @instance-id@ - The ID of the instance that fulfilled the request.
    --
    -- -   @launch-group@ - The Spot Instance launch group.
    --
    -- -   @launch.block-device-mapping.delete-on-termination@ - Indicates
    --     whether the EBS volume is deleted on instance termination.
    --
    -- -   @launch.block-device-mapping.device-name@ - The device name for the
    --     volume in the block device mapping (for example, @\/dev\/sdh@ or
    --     @xvdh@).
    --
    -- -   @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot
    --     for the EBS volume.
    --
    -- -   @launch.block-device-mapping.volume-size@ - The size of the EBS
    --     volume, in GiB.
    --
    -- -   @launch.block-device-mapping.volume-type@ - The type of EBS volume:
    --     @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS
    --     SSD, @st1@ for Throughput Optimized HDD, @sc1@for Cold HDD, or
    --     @standard@ for Magnetic.
    --
    -- -   @launch.group-id@ - The ID of the security group for the instance.
    --
    -- -   @launch.group-name@ - The name of the security group for the
    --     instance.
    --
    -- -   @launch.image-id@ - The ID of the AMI.
    --
    -- -   @launch.instance-type@ - The type of instance (for example,
    --     @m3.medium@).
    --
    -- -   @launch.kernel-id@ - The kernel ID.
    --
    -- -   @launch.key-name@ - The name of the key pair the instance launched
    --     with.
    --
    -- -   @launch.monitoring-enabled@ - Whether detailed monitoring is enabled
    --     for the Spot Instance.
    --
    -- -   @launch.ramdisk-id@ - The RAM disk ID.
    --
    -- -   @launched-availability-zone@ - The Availability Zone in which the
    --     request is launched.
    --
    -- -   @network-interface.addresses.primary@ - Indicates whether the IP
    --     address is the primary private IP address.
    --
    -- -   @network-interface.delete-on-termination@ - Indicates whether the
    --     network interface is deleted when the instance is terminated.
    --
    -- -   @network-interface.description@ - A description of the network
    --     interface.
    --
    -- -   @network-interface.device-index@ - The index of the device for the
    --     network interface attachment on the instance.
    --
    -- -   @network-interface.group-id@ - The ID of the security group
    --     associated with the network interface.
    --
    -- -   @network-interface.network-interface-id@ - The ID of the network
    --     interface.
    --
    -- -   @network-interface.private-ip-address@ - The primary private IP
    --     address of the network interface.
    --
    -- -   @network-interface.subnet-id@ - The ID of the subnet for the
    --     instance.
    --
    -- -   @product-description@ - The product description associated with the
    --     instance (@Linux\/UNIX@ | @Windows@).
    --
    -- -   @spot-instance-request-id@ - The Spot Instance request ID.
    --
    -- -   @spot-price@ - The maximum hourly price for any Spot Instance
    --     launched to fulfill the request.
    --
    -- -   @state@ - The state of the Spot Instance request (@open@ | @active@
    --     | @closed@ | @cancelled@ | @failed@). Spot request status
    --     information can help you track your Amazon EC2 Spot Instance
    --     requests. For more information, see
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status>
    --     in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- -   @status-code@ - The short code describing the most recent evaluation
    --     of your Spot Instance request.
    --
    -- -   @status-message@ - The message explaining the status of the Spot
    --     Instance request.
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
    -- -   @type@ - The type of Spot Instance request (@one-time@ |
    --     @persistent@).
    --
    -- -   @valid-from@ - The start date of the request.
    --
    -- -   @valid-until@ - The end date of the request.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotInstanceRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotInstanceRequests_nextToken' - The token to request the next set of results. This value is @null@ when
-- there are no more results to return.
--
-- 'dryRun', 'describeSpotInstanceRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotInstanceRequests_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 5 and 1000. To retrieve the remaining results, make
-- another call with the returned @NextToken@ value.
--
-- 'spotInstanceRequestIds', 'describeSpotInstanceRequests_spotInstanceRequestIds' - One or more Spot Instance request IDs.
--
-- 'filters', 'describeSpotInstanceRequests_filters' - One or more filters.
--
-- -   @availability-zone-group@ - The Availability Zone group.
--
-- -   @create-time@ - The time stamp when the Spot Instance request was
--     created.
--
-- -   @fault-code@ - The fault code related to the request.
--
-- -   @fault-message@ - The fault message related to the request.
--
-- -   @instance-id@ - The ID of the instance that fulfilled the request.
--
-- -   @launch-group@ - The Spot Instance launch group.
--
-- -   @launch.block-device-mapping.delete-on-termination@ - Indicates
--     whether the EBS volume is deleted on instance termination.
--
-- -   @launch.block-device-mapping.device-name@ - The device name for the
--     volume in the block device mapping (for example, @\/dev\/sdh@ or
--     @xvdh@).
--
-- -   @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot
--     for the EBS volume.
--
-- -   @launch.block-device-mapping.volume-size@ - The size of the EBS
--     volume, in GiB.
--
-- -   @launch.block-device-mapping.volume-type@ - The type of EBS volume:
--     @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS
--     SSD, @st1@ for Throughput Optimized HDD, @sc1@for Cold HDD, or
--     @standard@ for Magnetic.
--
-- -   @launch.group-id@ - The ID of the security group for the instance.
--
-- -   @launch.group-name@ - The name of the security group for the
--     instance.
--
-- -   @launch.image-id@ - The ID of the AMI.
--
-- -   @launch.instance-type@ - The type of instance (for example,
--     @m3.medium@).
--
-- -   @launch.kernel-id@ - The kernel ID.
--
-- -   @launch.key-name@ - The name of the key pair the instance launched
--     with.
--
-- -   @launch.monitoring-enabled@ - Whether detailed monitoring is enabled
--     for the Spot Instance.
--
-- -   @launch.ramdisk-id@ - The RAM disk ID.
--
-- -   @launched-availability-zone@ - The Availability Zone in which the
--     request is launched.
--
-- -   @network-interface.addresses.primary@ - Indicates whether the IP
--     address is the primary private IP address.
--
-- -   @network-interface.delete-on-termination@ - Indicates whether the
--     network interface is deleted when the instance is terminated.
--
-- -   @network-interface.description@ - A description of the network
--     interface.
--
-- -   @network-interface.device-index@ - The index of the device for the
--     network interface attachment on the instance.
--
-- -   @network-interface.group-id@ - The ID of the security group
--     associated with the network interface.
--
-- -   @network-interface.network-interface-id@ - The ID of the network
--     interface.
--
-- -   @network-interface.private-ip-address@ - The primary private IP
--     address of the network interface.
--
-- -   @network-interface.subnet-id@ - The ID of the subnet for the
--     instance.
--
-- -   @product-description@ - The product description associated with the
--     instance (@Linux\/UNIX@ | @Windows@).
--
-- -   @spot-instance-request-id@ - The Spot Instance request ID.
--
-- -   @spot-price@ - The maximum hourly price for any Spot Instance
--     launched to fulfill the request.
--
-- -   @state@ - The state of the Spot Instance request (@open@ | @active@
--     | @closed@ | @cancelled@ | @failed@). Spot request status
--     information can help you track your Amazon EC2 Spot Instance
--     requests. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status>
--     in the /Amazon EC2 User Guide for Linux Instances/.
--
-- -   @status-code@ - The short code describing the most recent evaluation
--     of your Spot Instance request.
--
-- -   @status-message@ - The message explaining the status of the Spot
--     Instance request.
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
-- -   @type@ - The type of Spot Instance request (@one-time@ |
--     @persistent@).
--
-- -   @valid-from@ - The start date of the request.
--
-- -   @valid-until@ - The end date of the request.
newDescribeSpotInstanceRequests ::
  DescribeSpotInstanceRequests
newDescribeSpotInstanceRequests =
  DescribeSpotInstanceRequests'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      spotInstanceRequestIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to request the next set of results. This value is @null@ when
-- there are no more results to return.
describeSpotInstanceRequests_nextToken :: Lens.Lens' DescribeSpotInstanceRequests (Prelude.Maybe Prelude.Text)
describeSpotInstanceRequests_nextToken = Lens.lens (\DescribeSpotInstanceRequests' {nextToken} -> nextToken) (\s@DescribeSpotInstanceRequests' {} a -> s {nextToken = a} :: DescribeSpotInstanceRequests)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotInstanceRequests_dryRun :: Lens.Lens' DescribeSpotInstanceRequests (Prelude.Maybe Prelude.Bool)
describeSpotInstanceRequests_dryRun = Lens.lens (\DescribeSpotInstanceRequests' {dryRun} -> dryRun) (\s@DescribeSpotInstanceRequests' {} a -> s {dryRun = a} :: DescribeSpotInstanceRequests)

-- | The maximum number of results to return in a single call. Specify a
-- value between 5 and 1000. To retrieve the remaining results, make
-- another call with the returned @NextToken@ value.
describeSpotInstanceRequests_maxResults :: Lens.Lens' DescribeSpotInstanceRequests (Prelude.Maybe Prelude.Int)
describeSpotInstanceRequests_maxResults = Lens.lens (\DescribeSpotInstanceRequests' {maxResults} -> maxResults) (\s@DescribeSpotInstanceRequests' {} a -> s {maxResults = a} :: DescribeSpotInstanceRequests)

-- | One or more Spot Instance request IDs.
describeSpotInstanceRequests_spotInstanceRequestIds :: Lens.Lens' DescribeSpotInstanceRequests (Prelude.Maybe [Prelude.Text])
describeSpotInstanceRequests_spotInstanceRequestIds = Lens.lens (\DescribeSpotInstanceRequests' {spotInstanceRequestIds} -> spotInstanceRequestIds) (\s@DescribeSpotInstanceRequests' {} a -> s {spotInstanceRequestIds = a} :: DescribeSpotInstanceRequests) Prelude.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @availability-zone-group@ - The Availability Zone group.
--
-- -   @create-time@ - The time stamp when the Spot Instance request was
--     created.
--
-- -   @fault-code@ - The fault code related to the request.
--
-- -   @fault-message@ - The fault message related to the request.
--
-- -   @instance-id@ - The ID of the instance that fulfilled the request.
--
-- -   @launch-group@ - The Spot Instance launch group.
--
-- -   @launch.block-device-mapping.delete-on-termination@ - Indicates
--     whether the EBS volume is deleted on instance termination.
--
-- -   @launch.block-device-mapping.device-name@ - The device name for the
--     volume in the block device mapping (for example, @\/dev\/sdh@ or
--     @xvdh@).
--
-- -   @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot
--     for the EBS volume.
--
-- -   @launch.block-device-mapping.volume-size@ - The size of the EBS
--     volume, in GiB.
--
-- -   @launch.block-device-mapping.volume-type@ - The type of EBS volume:
--     @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS
--     SSD, @st1@ for Throughput Optimized HDD, @sc1@for Cold HDD, or
--     @standard@ for Magnetic.
--
-- -   @launch.group-id@ - The ID of the security group for the instance.
--
-- -   @launch.group-name@ - The name of the security group for the
--     instance.
--
-- -   @launch.image-id@ - The ID of the AMI.
--
-- -   @launch.instance-type@ - The type of instance (for example,
--     @m3.medium@).
--
-- -   @launch.kernel-id@ - The kernel ID.
--
-- -   @launch.key-name@ - The name of the key pair the instance launched
--     with.
--
-- -   @launch.monitoring-enabled@ - Whether detailed monitoring is enabled
--     for the Spot Instance.
--
-- -   @launch.ramdisk-id@ - The RAM disk ID.
--
-- -   @launched-availability-zone@ - The Availability Zone in which the
--     request is launched.
--
-- -   @network-interface.addresses.primary@ - Indicates whether the IP
--     address is the primary private IP address.
--
-- -   @network-interface.delete-on-termination@ - Indicates whether the
--     network interface is deleted when the instance is terminated.
--
-- -   @network-interface.description@ - A description of the network
--     interface.
--
-- -   @network-interface.device-index@ - The index of the device for the
--     network interface attachment on the instance.
--
-- -   @network-interface.group-id@ - The ID of the security group
--     associated with the network interface.
--
-- -   @network-interface.network-interface-id@ - The ID of the network
--     interface.
--
-- -   @network-interface.private-ip-address@ - The primary private IP
--     address of the network interface.
--
-- -   @network-interface.subnet-id@ - The ID of the subnet for the
--     instance.
--
-- -   @product-description@ - The product description associated with the
--     instance (@Linux\/UNIX@ | @Windows@).
--
-- -   @spot-instance-request-id@ - The Spot Instance request ID.
--
-- -   @spot-price@ - The maximum hourly price for any Spot Instance
--     launched to fulfill the request.
--
-- -   @state@ - The state of the Spot Instance request (@open@ | @active@
--     | @closed@ | @cancelled@ | @failed@). Spot request status
--     information can help you track your Amazon EC2 Spot Instance
--     requests. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status>
--     in the /Amazon EC2 User Guide for Linux Instances/.
--
-- -   @status-code@ - The short code describing the most recent evaluation
--     of your Spot Instance request.
--
-- -   @status-message@ - The message explaining the status of the Spot
--     Instance request.
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
-- -   @type@ - The type of Spot Instance request (@one-time@ |
--     @persistent@).
--
-- -   @valid-from@ - The start date of the request.
--
-- -   @valid-until@ - The end date of the request.
describeSpotInstanceRequests_filters :: Lens.Lens' DescribeSpotInstanceRequests (Prelude.Maybe [Filter])
describeSpotInstanceRequests_filters = Lens.lens (\DescribeSpotInstanceRequests' {filters} -> filters) (\s@DescribeSpotInstanceRequests' {} a -> s {filters = a} :: DescribeSpotInstanceRequests) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeSpotInstanceRequests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSpotInstanceRequestsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSpotInstanceRequestsResponse_spotInstanceRequests
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeSpotInstanceRequests_nextToken
          Lens..~ rs
          Lens.^? describeSpotInstanceRequestsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeSpotInstanceRequests where
  type
    AWSResponse DescribeSpotInstanceRequests =
      DescribeSpotInstanceRequestsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotInstanceRequestsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "spotInstanceRequestSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSpotInstanceRequests

instance Prelude.NFData DescribeSpotInstanceRequests

instance Core.ToHeaders DescribeSpotInstanceRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeSpotInstanceRequests where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSpotInstanceRequests where
  toQuery DescribeSpotInstanceRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeSpotInstanceRequests" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "SpotInstanceRequestId"
              Prelude.<$> spotInstanceRequestIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | Contains the output of DescribeSpotInstanceRequests.
--
-- /See:/ 'newDescribeSpotInstanceRequestsResponse' smart constructor.
data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse'
  { -- | The token to use to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more Spot Instance requests.
    spotInstanceRequests :: Prelude.Maybe [SpotInstanceRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotInstanceRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotInstanceRequestsResponse_nextToken' - The token to use to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
--
-- 'spotInstanceRequests', 'describeSpotInstanceRequestsResponse_spotInstanceRequests' - One or more Spot Instance requests.
--
-- 'httpStatus', 'describeSpotInstanceRequestsResponse_httpStatus' - The response's http status code.
newDescribeSpotInstanceRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpotInstanceRequestsResponse
newDescribeSpotInstanceRequestsResponse pHttpStatus_ =
  DescribeSpotInstanceRequestsResponse'
    { nextToken =
        Prelude.Nothing,
      spotInstanceRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeSpotInstanceRequestsResponse_nextToken :: Lens.Lens' DescribeSpotInstanceRequestsResponse (Prelude.Maybe Prelude.Text)
describeSpotInstanceRequestsResponse_nextToken = Lens.lens (\DescribeSpotInstanceRequestsResponse' {nextToken} -> nextToken) (\s@DescribeSpotInstanceRequestsResponse' {} a -> s {nextToken = a} :: DescribeSpotInstanceRequestsResponse)

-- | One or more Spot Instance requests.
describeSpotInstanceRequestsResponse_spotInstanceRequests :: Lens.Lens' DescribeSpotInstanceRequestsResponse (Prelude.Maybe [SpotInstanceRequest])
describeSpotInstanceRequestsResponse_spotInstanceRequests = Lens.lens (\DescribeSpotInstanceRequestsResponse' {spotInstanceRequests} -> spotInstanceRequests) (\s@DescribeSpotInstanceRequestsResponse' {} a -> s {spotInstanceRequests = a} :: DescribeSpotInstanceRequestsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSpotInstanceRequestsResponse_httpStatus :: Lens.Lens' DescribeSpotInstanceRequestsResponse Prelude.Int
describeSpotInstanceRequestsResponse_httpStatus = Lens.lens (\DescribeSpotInstanceRequestsResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotInstanceRequestsResponse' {} a -> s {httpStatus = a} :: DescribeSpotInstanceRequestsResponse)

instance
  Prelude.NFData
    DescribeSpotInstanceRequestsResponse

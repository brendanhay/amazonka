{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotInstanceRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Spot Instance requests.
--
-- You can use @DescribeSpotInstanceRequests@ to find a running Spot Instance by examining the response. If the status of the Spot Instance is @fulfilled@ , the instance ID appears in the response and contains the identifier of the instance. Alternatively, you can use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances DescribeInstances> with a filter to look for instances where the instance lifecycle is @spot@ .
-- We recommend that you set @MaxResults@ to a value between 5 and 1000 to limit the number of results returned. This paginates the output, which makes the list more manageable and returns the results faster. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeSpotInstanceRequests@ request to retrieve the remaining results.
-- Spot Instance requests are deleted four hours after they are canceled and their instances are terminated.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotInstanceRequests
  ( -- * Creating a request
    DescribeSpotInstanceRequests (..),
    mkDescribeSpotInstanceRequests,

    -- ** Request lenses
    dsirFilters,
    dsirSpotInstanceRequestIds,
    dsirNextToken,
    dsirDryRun,
    dsirMaxResults,

    -- * Destructuring the response
    DescribeSpotInstanceRequestsResponse (..),
    mkDescribeSpotInstanceRequestsResponse,

    -- ** Response lenses
    dsirrsNextToken,
    dsirrsSpotInstanceRequests,
    dsirrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeSpotInstanceRequests.
--
-- /See:/ 'mkDescribeSpotInstanceRequests' smart constructor.
data DescribeSpotInstanceRequests = DescribeSpotInstanceRequests'
  { filters ::
      Lude.Maybe [Filter],
    spotInstanceRequestIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotInstanceRequests' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @availability-zone-group@ - The Availability Zone group.
--
--
--     * @create-time@ - The time stamp when the Spot Instance request was created.
--
--
--     * @fault-code@ - The fault code related to the request.
--
--
--     * @fault-message@ - The fault message related to the request.
--
--
--     * @instance-id@ - The ID of the instance that fulfilled the request.
--
--
--     * @launch-group@ - The Spot Instance launch group.
--
--
--     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.
--
--
--     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).
--
--
--     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.
--
--
--     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.
--
--
--     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.
--
--
--     * @launch.group-id@ - The ID of the security group for the instance.
--
--
--     * @launch.group-name@ - The name of the security group for the instance.
--
--
--     * @launch.image-id@ - The ID of the AMI.
--
--
--     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).
--
--
--     * @launch.kernel-id@ - The kernel ID.
--
--
--     * @launch.key-name@ - The name of the key pair the instance launched with.
--
--
--     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.
--
--
--     * @launch.ramdisk-id@ - The RAM disk ID.
--
--
--     * @launched-availability-zone@ - The Availability Zone in which the request is launched.
--
--
--     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.
--
--
--     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.
--
--
--     * @network-interface.description@ - A description of the network interface.
--
--
--     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.
--
--
--     * @network-interface.group-id@ - The ID of the security group associated with the network interface.
--
--
--     * @network-interface.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.
--
--
--     * @network-interface.subnet-id@ - The ID of the subnet for the instance.
--
--
--     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).
--
--
--     * @spot-instance-request-id@ - The Spot Instance request ID.
--
--
--     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.
--
--
--     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
--     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.
--
--
--     * @status-message@ - The message explaining the status of the Spot Instance request.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).
--
--
--     * @valid-from@ - The start date of the request.
--
--
--     * @valid-until@ - The end date of the request.
--
--
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token to request the next set of results. This value is @null@ when there are no more results to return.
-- * 'spotInstanceRequestIds' - One or more Spot Instance request IDs.
mkDescribeSpotInstanceRequests ::
  DescribeSpotInstanceRequests
mkDescribeSpotInstanceRequests =
  DescribeSpotInstanceRequests'
    { filters = Lude.Nothing,
      spotInstanceRequestIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @availability-zone-group@ - The Availability Zone group.
--
--
--     * @create-time@ - The time stamp when the Spot Instance request was created.
--
--
--     * @fault-code@ - The fault code related to the request.
--
--
--     * @fault-message@ - The fault message related to the request.
--
--
--     * @instance-id@ - The ID of the instance that fulfilled the request.
--
--
--     * @launch-group@ - The Spot Instance launch group.
--
--
--     * @launch.block-device-mapping.delete-on-termination@ - Indicates whether the EBS volume is deleted on instance termination.
--
--
--     * @launch.block-device-mapping.device-name@ - The device name for the volume in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).
--
--
--     * @launch.block-device-mapping.snapshot-id@ - The ID of the snapshot for the EBS volume.
--
--
--     * @launch.block-device-mapping.volume-size@ - The size of the EBS volume, in GiB.
--
--
--     * @launch.block-device-mapping.volume-type@ - The type of EBS volume: @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic.
--
--
--     * @launch.group-id@ - The ID of the security group for the instance.
--
--
--     * @launch.group-name@ - The name of the security group for the instance.
--
--
--     * @launch.image-id@ - The ID of the AMI.
--
--
--     * @launch.instance-type@ - The type of instance (for example, @m3.medium@ ).
--
--
--     * @launch.kernel-id@ - The kernel ID.
--
--
--     * @launch.key-name@ - The name of the key pair the instance launched with.
--
--
--     * @launch.monitoring-enabled@ - Whether detailed monitoring is enabled for the Spot Instance.
--
--
--     * @launch.ramdisk-id@ - The RAM disk ID.
--
--
--     * @launched-availability-zone@ - The Availability Zone in which the request is launched.
--
--
--     * @network-interface.addresses.primary@ - Indicates whether the IP address is the primary private IP address.
--
--
--     * @network-interface.delete-on-termination@ - Indicates whether the network interface is deleted when the instance is terminated.
--
--
--     * @network-interface.description@ - A description of the network interface.
--
--
--     * @network-interface.device-index@ - The index of the device for the network interface attachment on the instance.
--
--
--     * @network-interface.group-id@ - The ID of the security group associated with the network interface.
--
--
--     * @network-interface.network-interface-id@ - The ID of the network interface.
--
--
--     * @network-interface.private-ip-address@ - The primary private IP address of the network interface.
--
--
--     * @network-interface.subnet-id@ - The ID of the subnet for the instance.
--
--
--     * @product-description@ - The product description associated with the instance (@Linux/UNIX@ | @Windows@ ).
--
--
--     * @spot-instance-request-id@ - The Spot Instance request ID.
--
--
--     * @spot-price@ - The maximum hourly price for any Spot Instance launched to fulfill the request.
--
--
--     * @state@ - The state of the Spot Instance request (@open@ | @active@ | @closed@ | @cancelled@ | @failed@ ). Spot request status information can help you track your Amazon EC2 Spot Instance requests. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot request status> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
--     * @status-code@ - The short code describing the most recent evaluation of your Spot Instance request.
--
--
--     * @status-message@ - The message explaining the status of the Spot Instance request.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @type@ - The type of Spot Instance request (@one-time@ | @persistent@ ).
--
--
--     * @valid-from@ - The start date of the request.
--
--
--     * @valid-until@ - The end date of the request.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirFilters :: Lens.Lens' DescribeSpotInstanceRequests (Lude.Maybe [Filter])
dsirFilters = Lens.lens (filters :: DescribeSpotInstanceRequests -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeSpotInstanceRequests)
{-# DEPRECATED dsirFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more Spot Instance request IDs.
--
-- /Note:/ Consider using 'spotInstanceRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirSpotInstanceRequestIds :: Lens.Lens' DescribeSpotInstanceRequests (Lude.Maybe [Lude.Text])
dsirSpotInstanceRequestIds = Lens.lens (spotInstanceRequestIds :: DescribeSpotInstanceRequests -> Lude.Maybe [Lude.Text]) (\s a -> s {spotInstanceRequestIds = a} :: DescribeSpotInstanceRequests)
{-# DEPRECATED dsirSpotInstanceRequestIds "Use generic-lens or generic-optics with 'spotInstanceRequestIds' instead." #-}

-- | The token to request the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirNextToken :: Lens.Lens' DescribeSpotInstanceRequests (Lude.Maybe Lude.Text)
dsirNextToken = Lens.lens (nextToken :: DescribeSpotInstanceRequests -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotInstanceRequests)
{-# DEPRECATED dsirNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirDryRun :: Lens.Lens' DescribeSpotInstanceRequests (Lude.Maybe Lude.Bool)
dsirDryRun = Lens.lens (dryRun :: DescribeSpotInstanceRequests -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeSpotInstanceRequests)
{-# DEPRECATED dsirDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 5 and 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirMaxResults :: Lens.Lens' DescribeSpotInstanceRequests (Lude.Maybe Lude.Int)
dsirMaxResults = Lens.lens (maxResults :: DescribeSpotInstanceRequests -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeSpotInstanceRequests)
{-# DEPRECATED dsirMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeSpotInstanceRequests where
  page rq rs
    | Page.stop (rs Lens.^. dsirrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsirrsSpotInstanceRequests) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsirNextToken Lens..~ rs Lens.^. dsirrsNextToken

instance Lude.AWSRequest DescribeSpotInstanceRequests where
  type
    Rs DescribeSpotInstanceRequests =
      DescribeSpotInstanceRequestsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeSpotInstanceRequestsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "spotInstanceRequestSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSpotInstanceRequests where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSpotInstanceRequests where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSpotInstanceRequests where
  toQuery DescribeSpotInstanceRequests' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeSpotInstanceRequests" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          ( Lude.toQueryList "SpotInstanceRequestId"
              Lude.<$> spotInstanceRequestIds
          ),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeSpotInstanceRequests.
--
-- /See:/ 'mkDescribeSpotInstanceRequestsResponse' smart constructor.
data DescribeSpotInstanceRequestsResponse = DescribeSpotInstanceRequestsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    spotInstanceRequests ::
      Lude.Maybe
        [SpotInstanceRequest],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSpotInstanceRequestsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next set of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'spotInstanceRequests' - One or more Spot Instance requests.
mkDescribeSpotInstanceRequestsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSpotInstanceRequestsResponse
mkDescribeSpotInstanceRequestsResponse pResponseStatus_ =
  DescribeSpotInstanceRequestsResponse'
    { nextToken = Lude.Nothing,
      spotInstanceRequests = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsNextToken :: Lens.Lens' DescribeSpotInstanceRequestsResponse (Lude.Maybe Lude.Text)
dsirrsNextToken = Lens.lens (nextToken :: DescribeSpotInstanceRequestsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSpotInstanceRequestsResponse)
{-# DEPRECATED dsirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more Spot Instance requests.
--
-- /Note:/ Consider using 'spotInstanceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsSpotInstanceRequests :: Lens.Lens' DescribeSpotInstanceRequestsResponse (Lude.Maybe [SpotInstanceRequest])
dsirrsSpotInstanceRequests = Lens.lens (spotInstanceRequests :: DescribeSpotInstanceRequestsResponse -> Lude.Maybe [SpotInstanceRequest]) (\s a -> s {spotInstanceRequests = a} :: DescribeSpotInstanceRequestsResponse)
{-# DEPRECATED dsirrsSpotInstanceRequests "Use generic-lens or generic-optics with 'spotInstanceRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DescribeSpotInstanceRequestsResponse Lude.Int
dsirrsResponseStatus = Lens.lens (responseStatus :: DescribeSpotInstanceRequestsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSpotInstanceRequestsResponse)
{-# DEPRECATED dsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

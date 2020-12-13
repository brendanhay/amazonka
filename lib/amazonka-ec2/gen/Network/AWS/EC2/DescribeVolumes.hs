{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EBS volumes or all of your EBS volumes.
--
-- If you are describing a long list of volumes, we recommend that you paginate the output to make the list more manageable. The @MaxResults@ parameter sets the maximum number of results returned in a single page. If the list of results exceeds your @MaxResults@ value, then that number of results is returned along with a @NextToken@ value that can be passed to a subsequent @DescribeVolumes@ request to retrieve the remaining results.
-- For more information about EBS volumes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumes
  ( -- * Creating a request
    DescribeVolumes (..),
    mkDescribeVolumes,

    -- ** Request lenses
    dvFilters,
    dvVolumeIds,
    dvNextToken,
    dvDryRun,
    dvMaxResults,

    -- * Destructuring the response
    DescribeVolumesResponse (..),
    mkDescribeVolumesResponse,

    -- ** Response lenses
    dvsrsNextToken,
    dvsrsVolumes,
    dvsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | The filters.
    --
    --
    --     * @attachment.attach-time@ - The time stamp when the attachment initiated.
    --
    --
    --     * @attachment.delete-on-termination@ - Whether the volume is deleted on instance termination.
    --
    --
    --     * @attachment.device@ - The device name specified in the block device mapping (for example, @/dev/sda1@ ).
    --
    --
    --     * @attachment.instance-id@ - The ID of the instance the volume is attached to.
    --
    --
    --     * @attachment.status@ - The attachment state (@attaching@ | @attached@ | @detaching@ ).
    --
    --
    --     * @availability-zone@ - The Availability Zone in which the volume was created.
    --
    --
    --     * @create-time@ - The time stamp when the volume was created.
    --
    --
    --     * @encrypted@ - Indicates whether the volume is encrypted (@true@ | @false@ )
    --
    --
    --     * @multi-attach-enabled@ - Indicates whether the volume is enabled for Multi-Attach (@true@ | @false@ )
    --
    --
    --     * @fast-restored@ - Indicates whether the volume was created from a snapshot that is enabled for fast snapshot restore (@true@ | @false@ ).
    --
    --
    --     * @size@ - The size of the volume, in GiB.
    --
    --
    --     * @snapshot-id@ - The snapshot from which the volume was created.
    --
    --
    --     * @status@ - The state of the volume (@creating@ | @available@ | @in-use@ | @deleting@ | @deleted@ | @error@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @volume-id@ - The volume ID.
    --
    --
    --     * @volume-type@ - The Amazon EBS volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
    filters :: Lude.Maybe [Filter],
    -- | The volume IDs.
    volumeIds :: Lude.Maybe [Lude.Text],
    -- | The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumes' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @attachment.attach-time@ - The time stamp when the attachment initiated.
--
--
--     * @attachment.delete-on-termination@ - Whether the volume is deleted on instance termination.
--
--
--     * @attachment.device@ - The device name specified in the block device mapping (for example, @/dev/sda1@ ).
--
--
--     * @attachment.instance-id@ - The ID of the instance the volume is attached to.
--
--
--     * @attachment.status@ - The attachment state (@attaching@ | @attached@ | @detaching@ ).
--
--
--     * @availability-zone@ - The Availability Zone in which the volume was created.
--
--
--     * @create-time@ - The time stamp when the volume was created.
--
--
--     * @encrypted@ - Indicates whether the volume is encrypted (@true@ | @false@ )
--
--
--     * @multi-attach-enabled@ - Indicates whether the volume is enabled for Multi-Attach (@true@ | @false@ )
--
--
--     * @fast-restored@ - Indicates whether the volume was created from a snapshot that is enabled for fast snapshot restore (@true@ | @false@ ).
--
--
--     * @size@ - The size of the volume, in GiB.
--
--
--     * @snapshot-id@ - The snapshot from which the volume was created.
--
--
--     * @status@ - The state of the volume (@creating@ | @available@ | @in-use@ | @deleting@ | @deleted@ | @error@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @volume-id@ - The volume ID.
--
--
--     * @volume-type@ - The Amazon EBS volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
--
-- * 'volumeIds' - The volume IDs.
-- * 'nextToken' - The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
mkDescribeVolumes ::
  DescribeVolumes
mkDescribeVolumes =
  DescribeVolumes'
    { filters = Lude.Nothing,
      volumeIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @attachment.attach-time@ - The time stamp when the attachment initiated.
--
--
--     * @attachment.delete-on-termination@ - Whether the volume is deleted on instance termination.
--
--
--     * @attachment.device@ - The device name specified in the block device mapping (for example, @/dev/sda1@ ).
--
--
--     * @attachment.instance-id@ - The ID of the instance the volume is attached to.
--
--
--     * @attachment.status@ - The attachment state (@attaching@ | @attached@ | @detaching@ ).
--
--
--     * @availability-zone@ - The Availability Zone in which the volume was created.
--
--
--     * @create-time@ - The time stamp when the volume was created.
--
--
--     * @encrypted@ - Indicates whether the volume is encrypted (@true@ | @false@ )
--
--
--     * @multi-attach-enabled@ - Indicates whether the volume is enabled for Multi-Attach (@true@ | @false@ )
--
--
--     * @fast-restored@ - Indicates whether the volume was created from a snapshot that is enabled for fast snapshot restore (@true@ | @false@ ).
--
--
--     * @size@ - The size of the volume, in GiB.
--
--
--     * @snapshot-id@ - The snapshot from which the volume was created.
--
--
--     * @status@ - The state of the volume (@creating@ | @available@ | @in-use@ | @deleting@ | @deleted@ | @error@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @volume-id@ - The volume ID.
--
--
--     * @volume-type@ - The Amazon EBS volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvFilters :: Lens.Lens' DescribeVolumes (Lude.Maybe [Filter])
dvFilters = Lens.lens (filters :: DescribeVolumes -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVolumes)
{-# DEPRECATED dvFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The volume IDs.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVolumeIds :: Lens.Lens' DescribeVolumes (Lude.Maybe [Lude.Text])
dvVolumeIds = Lens.lens (volumeIds :: DescribeVolumes -> Lude.Maybe [Lude.Text]) (\s a -> s {volumeIds = a} :: DescribeVolumes)
{-# DEPRECATED dvVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

-- | The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvNextToken :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Text)
dvNextToken = Lens.lens (nextToken :: DescribeVolumes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumes)
{-# DEPRECATED dvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDryRun :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Bool)
dvDryRun = Lens.lens (dryRun :: DescribeVolumes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVolumes)
{-# DEPRECATED dvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvMaxResults :: Lens.Lens' DescribeVolumes (Lude.Maybe Lude.Int)
dvMaxResults = Lens.lens (maxResults :: DescribeVolumes -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVolumes)
{-# DEPRECATED dvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVolumes where
  page rq rs
    | Page.stop (rs Lens.^. dvsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvsrsVolumes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvNextToken Lens..~ rs Lens.^. dvsrsNextToken

instance Lude.AWSRequest DescribeVolumes where
  type Rs DescribeVolumes = DescribeVolumesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVolumesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "volumeSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVolumes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVolumes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVolumes where
  toQuery DescribeVolumes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVolumes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "VolumeId" Lude.<$> volumeIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the volumes.
    volumes :: Lude.Maybe [Volume],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'volumes' - Information about the volumes.
-- * 'responseStatus' - The response status code.
mkDescribeVolumesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVolumesResponse
mkDescribeVolumesResponse pResponseStatus_ =
  DescribeVolumesResponse'
    { nextToken = Lude.Nothing,
      volumes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsNextToken :: Lens.Lens' DescribeVolumesResponse (Lude.Maybe Lude.Text)
dvsrsNextToken = Lens.lens (nextToken :: DescribeVolumesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumesResponse)
{-# DEPRECATED dvsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the volumes.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsVolumes :: Lens.Lens' DescribeVolumesResponse (Lude.Maybe [Volume])
dvsrsVolumes = Lens.lens (volumes :: DescribeVolumesResponse -> Lude.Maybe [Volume]) (\s a -> s {volumes = a} :: DescribeVolumesResponse)
{-# DEPRECATED dvsrsVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsResponseStatus :: Lens.Lens' DescribeVolumesResponse Lude.Int
dvsrsResponseStatus = Lens.lens (responseStatus :: DescribeVolumesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVolumesResponse)
{-# DEPRECATED dvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

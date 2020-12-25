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
    dvfDryRun,
    dvfFilters,
    dvfMaxResults,
    dvfNextToken,
    dvfVolumeIds,

    -- * Destructuring the response
    DescribeVolumesResponse (..),
    mkDescribeVolumesResponse,

    -- ** Response lenses
    dvrfrsNextToken,
    dvrfrsVolumes,
    dvrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
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
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
    maxResults :: Core.Maybe Core.Int,
    -- | The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The volume IDs.
    volumeIds :: Core.Maybe [Types.VolumeId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumes' value with any optional fields omitted.
mkDescribeVolumes ::
  DescribeVolumes
mkDescribeVolumes =
  DescribeVolumes'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      volumeIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfDryRun :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Bool)
dvfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dvfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
dvfFilters :: Lens.Lens' DescribeVolumes (Core.Maybe [Types.Filter])
dvfFilters = Lens.field @"filters"
{-# DEPRECATED dvfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of volume results returned by @DescribeVolumes@ in paginated output. When this parameter is used, @DescribeVolumes@ only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeVolumes@ request with the returned @NextToken@ value. This value can be between 5 and 500; if @MaxResults@ is given a value larger than 500, only 500 results are returned. If this parameter is not used, then @DescribeVolumes@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfMaxResults :: Lens.Lens' DescribeVolumes (Core.Maybe Core.Int)
dvfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dvfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @NextToken@ value returned from a previous paginated @DescribeVolumes@ request where @MaxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @NextToken@ value. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfNextToken :: Lens.Lens' DescribeVolumes (Core.Maybe Types.NextToken)
dvfNextToken = Lens.field @"nextToken"
{-# DEPRECATED dvfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The volume IDs.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfVolumeIds :: Lens.Lens' DescribeVolumes (Core.Maybe [Types.VolumeId])
dvfVolumeIds = Lens.field @"volumeIds"
{-# DEPRECATED dvfVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

instance Core.AWSRequest DescribeVolumes where
  type Rs DescribeVolumes = DescribeVolumesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeVolumes")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryList "VolumeId" Core.<$> volumeIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "volumeSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeVolumes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"volumes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { -- | The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the volumes.
    volumes :: Core.Maybe [Types.Volume],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeVolumesResponse' value with any optional fields omitted.
mkDescribeVolumesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVolumesResponse
mkDescribeVolumesResponse responseStatus =
  DescribeVolumesResponse'
    { nextToken = Core.Nothing,
      volumes = Core.Nothing,
      responseStatus
    }

-- | The @NextToken@ value to include in a future @DescribeVolumes@ request. When the results of a @DescribeVolumes@ request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrfrsNextToken :: Lens.Lens' DescribeVolumesResponse (Core.Maybe Types.String)
dvrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dvrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the volumes.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrfrsVolumes :: Lens.Lens' DescribeVolumesResponse (Core.Maybe [Types.Volume])
dvrfrsVolumes = Lens.field @"volumes"
{-# DEPRECATED dvrfrsVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrfrsResponseStatus :: Lens.Lens' DescribeVolumesResponse Core.Int
dvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

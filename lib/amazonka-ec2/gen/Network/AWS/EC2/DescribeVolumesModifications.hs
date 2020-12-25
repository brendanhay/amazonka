{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumesModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the most recent volume modification request for the specified EBS volumes.
--
-- If a volume has never been modified, some information in the output will be null. If a volume has been modified more than once, the output includes only the most recent modification request.
-- You can also use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring volume modifications> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumesModifications
  ( -- * Creating a request
    DescribeVolumesModifications (..),
    mkDescribeVolumesModifications,

    -- ** Request lenses
    dvmDryRun,
    dvmFilters,
    dvmMaxResults,
    dvmNextToken,
    dvmVolumeIds,

    -- * Destructuring the response
    DescribeVolumesModificationsResponse (..),
    mkDescribeVolumesModificationsResponse,

    -- ** Response lenses
    dvmrrsNextToken,
    dvmrrsVolumesModifications,
    dvmrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @modification-state@ - The current modification state (modifying | optimizing | completed | failed).
    --
    --
    --     * @original-iops@ - The original IOPS rate of the volume.
    --
    --
    --     * @original-size@ - The original size of the volume, in GiB.
    --
    --
    --     * @original-volume-type@ - The original volume type of the volume (standard | io1 | io2 | gp2 | sc1 | st1).
    --
    --
    --     * @originalMultiAttachEnabled@ - Indicates whether Multi-Attach support was enabled (true | false).
    --
    --
    --     * @start-time@ - The modification start time.
    --
    --
    --     * @target-iops@ - The target IOPS rate of the volume.
    --
    --
    --     * @target-size@ - The target size of the volume, in GiB.
    --
    --
    --     * @target-volume-type@ - The target volume type of the volume (standard | io1 | io2 | gp2 | sc1 | st1).
    --
    --
    --     * @targetMultiAttachEnabled@ - Indicates whether Multi-Attach support is to be enabled (true | false).
    --
    --
    --     * @volume-id@ - The ID of the volume.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results (up to a limit of 500) to be returned in a paginated request.
    maxResults :: Core.Maybe Core.Int,
    -- | The @nextToken@ value returned by a previous paginated request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The IDs of the volumes.
    volumeIds :: Core.Maybe [Types.VolumeId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumesModifications' value with any optional fields omitted.
mkDescribeVolumesModifications ::
  DescribeVolumesModifications
mkDescribeVolumesModifications =
  DescribeVolumesModifications'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      volumeIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmDryRun :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Core.Bool)
dvmDryRun = Lens.field @"dryRun"
{-# DEPRECATED dvmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @modification-state@ - The current modification state (modifying | optimizing | completed | failed).
--
--
--     * @original-iops@ - The original IOPS rate of the volume.
--
--
--     * @original-size@ - The original size of the volume, in GiB.
--
--
--     * @original-volume-type@ - The original volume type of the volume (standard | io1 | io2 | gp2 | sc1 | st1).
--
--
--     * @originalMultiAttachEnabled@ - Indicates whether Multi-Attach support was enabled (true | false).
--
--
--     * @start-time@ - The modification start time.
--
--
--     * @target-iops@ - The target IOPS rate of the volume.
--
--
--     * @target-size@ - The target size of the volume, in GiB.
--
--
--     * @target-volume-type@ - The target volume type of the volume (standard | io1 | io2 | gp2 | sc1 | st1).
--
--
--     * @targetMultiAttachEnabled@ - Indicates whether Multi-Attach support is to be enabled (true | false).
--
--
--     * @volume-id@ - The ID of the volume.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmFilters :: Lens.Lens' DescribeVolumesModifications (Core.Maybe [Types.Filter])
dvmFilters = Lens.field @"filters"
{-# DEPRECATED dvmFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results (up to a limit of 500) to be returned in a paginated request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmMaxResults :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Core.Int)
dvmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dvmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned by a previous paginated request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmNextToken :: Lens.Lens' DescribeVolumesModifications (Core.Maybe Types.NextToken)
dvmNextToken = Lens.field @"nextToken"
{-# DEPRECATED dvmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the volumes.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmVolumeIds :: Lens.Lens' DescribeVolumesModifications (Core.Maybe [Types.VolumeId])
dvmVolumeIds = Lens.field @"volumeIds"
{-# DEPRECATED dvmVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

instance Core.AWSRequest DescribeVolumesModifications where
  type
    Rs DescribeVolumesModifications =
      DescribeVolumesModificationsResponse
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
            ( Core.pure ("Action", "DescribeVolumesModifications")
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
          DescribeVolumesModificationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "volumeModificationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeVolumesModifications where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"volumesModifications" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { -- | Token for pagination, null if there are no more results
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the volume modifications.
    volumesModifications :: Core.Maybe [Types.VolumeModification],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeVolumesModificationsResponse' value with any optional fields omitted.
mkDescribeVolumesModificationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeVolumesModificationsResponse
mkDescribeVolumesModificationsResponse responseStatus =
  DescribeVolumesModificationsResponse'
    { nextToken = Core.Nothing,
      volumesModifications = Core.Nothing,
      responseStatus
    }

-- | Token for pagination, null if there are no more results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrrsNextToken :: Lens.Lens' DescribeVolumesModificationsResponse (Core.Maybe Types.NextToken)
dvmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dvmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the volume modifications.
--
-- /Note:/ Consider using 'volumesModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrrsVolumesModifications :: Lens.Lens' DescribeVolumesModificationsResponse (Core.Maybe [Types.VolumeModification])
dvmrrsVolumesModifications = Lens.field @"volumesModifications"
{-# DEPRECATED dvmrrsVolumesModifications "Use generic-lens or generic-optics with 'volumesModifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrrsResponseStatus :: Lens.Lens' DescribeVolumesModificationsResponse Core.Int
dvmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dvmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    dvmFilters,
    dvmVolumeIds,
    dvmNextToken,
    dvmDryRun,
    dvmMaxResults,

    -- * Destructuring the response
    DescribeVolumesModificationsResponse (..),
    mkDescribeVolumesModificationsResponse,

    -- ** Response lenses
    dvmrsVolumesModifications,
    dvmrsNextToken,
    dvmrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVolumesModifications' smart constructor.
data DescribeVolumesModifications = DescribeVolumesModifications'
  { -- | The filters.
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
    filters :: Lude.Maybe [Filter],
    -- | The IDs of the volumes.
    volumeIds :: Lude.Maybe [Lude.Text],
    -- | The @nextToken@ value returned by a previous paginated request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results (up to a limit of 500) to be returned in a paginated request.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumesModifications' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
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
-- * 'volumeIds' - The IDs of the volumes.
-- * 'nextToken' - The @nextToken@ value returned by a previous paginated request.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results (up to a limit of 500) to be returned in a paginated request.
mkDescribeVolumesModifications ::
  DescribeVolumesModifications
mkDescribeVolumesModifications =
  DescribeVolumesModifications'
    { filters = Lude.Nothing,
      volumeIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

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
dvmFilters :: Lens.Lens' DescribeVolumesModifications (Lude.Maybe [Filter])
dvmFilters = Lens.lens (filters :: DescribeVolumesModifications -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVolumesModifications)
{-# DEPRECATED dvmFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the volumes.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmVolumeIds :: Lens.Lens' DescribeVolumesModifications (Lude.Maybe [Lude.Text])
dvmVolumeIds = Lens.lens (volumeIds :: DescribeVolumesModifications -> Lude.Maybe [Lude.Text]) (\s a -> s {volumeIds = a} :: DescribeVolumesModifications)
{-# DEPRECATED dvmVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

-- | The @nextToken@ value returned by a previous paginated request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmNextToken :: Lens.Lens' DescribeVolumesModifications (Lude.Maybe Lude.Text)
dvmNextToken = Lens.lens (nextToken :: DescribeVolumesModifications -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumesModifications)
{-# DEPRECATED dvmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmDryRun :: Lens.Lens' DescribeVolumesModifications (Lude.Maybe Lude.Bool)
dvmDryRun = Lens.lens (dryRun :: DescribeVolumesModifications -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVolumesModifications)
{-# DEPRECATED dvmDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results (up to a limit of 500) to be returned in a paginated request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmMaxResults :: Lens.Lens' DescribeVolumesModifications (Lude.Maybe Lude.Int)
dvmMaxResults = Lens.lens (maxResults :: DescribeVolumesModifications -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVolumesModifications)
{-# DEPRECATED dvmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVolumesModifications where
  page rq rs
    | Page.stop (rs Lens.^. dvmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvmrsVolumesModifications) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvmNextToken Lens..~ rs Lens.^. dvmrsNextToken

instance Lude.AWSRequest DescribeVolumesModifications where
  type
    Rs DescribeVolumesModifications =
      DescribeVolumesModificationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVolumesModificationsResponse'
            Lude.<$> ( x Lude..@? "volumeModificationSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVolumesModifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVolumesModifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVolumesModifications where
  toQuery DescribeVolumesModifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVolumesModifications" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "VolumeId" Lude.<$> volumeIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVolumesModificationsResponse' smart constructor.
data DescribeVolumesModificationsResponse = DescribeVolumesModificationsResponse'
  { -- | Information about the volume modifications.
    volumesModifications :: Lude.Maybe [VolumeModification],
    -- | Token for pagination, null if there are no more results
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumesModificationsResponse' with the minimum fields required to make a request.
--
-- * 'volumesModifications' - Information about the volume modifications.
-- * 'nextToken' - Token for pagination, null if there are no more results
-- * 'responseStatus' - The response status code.
mkDescribeVolumesModificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVolumesModificationsResponse
mkDescribeVolumesModificationsResponse pResponseStatus_ =
  DescribeVolumesModificationsResponse'
    { volumesModifications =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the volume modifications.
--
-- /Note:/ Consider using 'volumesModifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrsVolumesModifications :: Lens.Lens' DescribeVolumesModificationsResponse (Lude.Maybe [VolumeModification])
dvmrsVolumesModifications = Lens.lens (volumesModifications :: DescribeVolumesModificationsResponse -> Lude.Maybe [VolumeModification]) (\s a -> s {volumesModifications = a} :: DescribeVolumesModificationsResponse)
{-# DEPRECATED dvmrsVolumesModifications "Use generic-lens or generic-optics with 'volumesModifications' instead." #-}

-- | Token for pagination, null if there are no more results
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrsNextToken :: Lens.Lens' DescribeVolumesModificationsResponse (Lude.Maybe Lude.Text)
dvmrsNextToken = Lens.lens (nextToken :: DescribeVolumesModificationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumesModificationsResponse)
{-# DEPRECATED dvmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvmrsResponseStatus :: Lens.Lens' DescribeVolumesModificationsResponse Lude.Int
dvmrsResponseStatus = Lens.lens (responseStatus :: DescribeVolumesModificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVolumesModificationsResponse)
{-# DEPRECATED dvmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

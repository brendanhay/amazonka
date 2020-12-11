{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified volumes. Volume status provides the result of the checks performed on your volumes to determine events that can impair the performance of your volumes. The performance of a volume can be affected if an issue occurs on the volume's underlying host. If the volume's underlying host experiences a power outage or system issue, after the system is restored, there could be data inconsistencies on the volume. Volume events notify you if this occurs. Volume actions notify you if any action needs to be taken in response to the event.
--
-- The @DescribeVolumeStatus@ operation provides the following information about the specified volumes:
-- /Status/ : Reflects the current status of the volume. The possible values are @ok@ , @impaired@ , @warning@ , or @insufficient-data@ . If all checks pass, the overall status of the volume is @ok@ . If the check fails, the overall status is @impaired@ . If the status is @insufficient-data@ , then the checks may still be taking place on your volume at the time. We recommend that you retry the request. For more information about volume status, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html Monitoring the status of your volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
-- /Events/ : Reflect the cause of a volume status and may require you to take action. For example, if your volume returns an @impaired@ status, then the volume event might be @potential-data-inconsistency@ . This means that your volume has been affected by an issue with the underlying host, has all I/O operations disabled, and may have inconsistent data.
-- /Actions/ : Reflect the actions you may have to take in response to an event. For example, if the status of the volume is @impaired@ and the volume event shows @potential-data-inconsistency@ , then the action shows @enable-volume-io@ . This means that you may want to enable the I/O operations for the volume by calling the 'EnableVolumeIO' action and then check the volume for data consistency.
-- Volume status is based on the volume status checks, and does not reflect the volume state. Therefore, volume status does not indicate volumes in the @error@ state (for example, when a volume is incapable of accepting I/O.)
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumeStatus
  ( -- * Creating a request
    DescribeVolumeStatus (..),
    mkDescribeVolumeStatus,

    -- ** Request lenses
    dvssFilters,
    dvssVolumeIds,
    dvssNextToken,
    dvssDryRun,
    dvssMaxResults,

    -- * Destructuring the response
    DescribeVolumeStatusResponse (..),
    mkDescribeVolumeStatusResponse,

    -- ** Response lenses
    dvsrsNextToken,
    dvsrsVolumeStatuses,
    dvsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVolumeStatus' smart constructor.
data DescribeVolumeStatus = DescribeVolumeStatus'
  { filters ::
      Lude.Maybe [Filter],
    volumeIds :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeVolumeStatus' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).
--
--
--     * @action.description@ - A description of the action.
--
--
--     * @action.event-id@ - The event ID associated with the action.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.event-id@ - The event ID.
--
--
--     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).
--
--
--     * @event.not-after@ - The latest end time for the event.
--
--
--     * @event.not-before@ - The earliest start time for the event.
--
--
--     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).
--
--
--     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).
--
--
--     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
--
--
-- * 'maxResults' - The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
-- * 'nextToken' - The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'volumeIds' - The IDs of the volumes.
--
-- Default: Describes all your volumes.
mkDescribeVolumeStatus ::
  DescribeVolumeStatus
mkDescribeVolumeStatus =
  DescribeVolumeStatus'
    { filters = Lude.Nothing,
      volumeIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).
--
--
--     * @action.description@ - A description of the action.
--
--
--     * @action.event-id@ - The event ID associated with the action.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.event-id@ - The event ID.
--
--
--     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).
--
--
--     * @event.not-after@ - The latest end time for the event.
--
--
--     * @event.not-before@ - The earliest start time for the event.
--
--
--     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).
--
--
--     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).
--
--
--     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssFilters :: Lens.Lens' DescribeVolumeStatus (Lude.Maybe [Filter])
dvssFilters = Lens.lens (filters :: DescribeVolumeStatus -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVolumeStatus)
{-# DEPRECATED dvssFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the volumes.
--
-- Default: Describes all your volumes.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssVolumeIds :: Lens.Lens' DescribeVolumeStatus (Lude.Maybe [Lude.Text])
dvssVolumeIds = Lens.lens (volumeIds :: DescribeVolumeStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {volumeIds = a} :: DescribeVolumeStatus)
{-# DEPRECATED dvssVolumeIds "Use generic-lens or generic-optics with 'volumeIds' instead." #-}

-- | The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssNextToken :: Lens.Lens' DescribeVolumeStatus (Lude.Maybe Lude.Text)
dvssNextToken = Lens.lens (nextToken :: DescribeVolumeStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumeStatus)
{-# DEPRECATED dvssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssDryRun :: Lens.Lens' DescribeVolumeStatus (Lude.Maybe Lude.Bool)
dvssDryRun = Lens.lens (dryRun :: DescribeVolumeStatus -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVolumeStatus)
{-# DEPRECATED dvssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssMaxResults :: Lens.Lens' DescribeVolumeStatus (Lude.Maybe Lude.Int)
dvssMaxResults = Lens.lens (maxResults :: DescribeVolumeStatus -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVolumeStatus)
{-# DEPRECATED dvssMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVolumeStatus where
  page rq rs
    | Page.stop (rs Lens.^. dvsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvsrsVolumeStatuses) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvssNextToken Lens..~ rs Lens.^. dvsrsNextToken

instance Lude.AWSRequest DescribeVolumeStatus where
  type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVolumeStatusResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "volumeStatusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVolumeStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVolumeStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVolumeStatus where
  toQuery DescribeVolumeStatus' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVolumeStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "VolumeId" Lude.<$> volumeIds),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVolumeStatusResponse' smart constructor.
data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    volumeStatuses ::
      Lude.Maybe [VolumeStatusItem],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumeStatusResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'volumeStatuses' - Information about the status of the volumes.
mkDescribeVolumeStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVolumeStatusResponse
mkDescribeVolumeStatusResponse pResponseStatus_ =
  DescribeVolumeStatusResponse'
    { nextToken = Lude.Nothing,
      volumeStatuses = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsNextToken :: Lens.Lens' DescribeVolumeStatusResponse (Lude.Maybe Lude.Text)
dvsrsNextToken = Lens.lens (nextToken :: DescribeVolumeStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVolumeStatusResponse)
{-# DEPRECATED dvsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the status of the volumes.
--
-- /Note:/ Consider using 'volumeStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsVolumeStatuses :: Lens.Lens' DescribeVolumeStatusResponse (Lude.Maybe [VolumeStatusItem])
dvsrsVolumeStatuses = Lens.lens (volumeStatuses :: DescribeVolumeStatusResponse -> Lude.Maybe [VolumeStatusItem]) (\s a -> s {volumeStatuses = a} :: DescribeVolumeStatusResponse)
{-# DEPRECATED dvsrsVolumeStatuses "Use generic-lens or generic-optics with 'volumeStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrsResponseStatus :: Lens.Lens' DescribeVolumeStatusResponse Lude.Int
dvsrsResponseStatus = Lens.lens (responseStatus :: DescribeVolumeStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVolumeStatusResponse)
{-# DEPRECATED dvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeScheduledInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Scheduled Instances or all your Scheduled Instances.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeScheduledInstances
  ( -- * Creating a request
    DescribeScheduledInstances (..),
    mkDescribeScheduledInstances,

    -- ** Request lenses
    dsiFilters,
    dsiSlotStartTimeRange,
    dsiNextToken,
    dsiScheduledInstanceIds,
    dsiDryRun,
    dsiMaxResults,

    -- * Destructuring the response
    DescribeScheduledInstancesResponse (..),
    mkDescribeScheduledInstancesResponse,

    -- ** Response lenses
    dsirsNextToken,
    dsirsScheduledInstanceSet,
    dsirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeScheduledInstances.
--
-- /See:/ 'mkDescribeScheduledInstances' smart constructor.
data DescribeScheduledInstances = DescribeScheduledInstances'
  { filters ::
      Lude.Maybe [Filter],
    slotStartTimeRange ::
      Lude.Maybe SlotStartTimeRangeRequest,
    nextToken :: Lude.Maybe Lude.Text,
    scheduledInstanceIds ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'DescribeScheduledInstances' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).
--
--
--     * @instance-type@ - The instance type (for example, @c4.large@ ).
--
--
--     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
--
--     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
--
--
-- * 'maxResults' - The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'scheduledInstanceIds' - The Scheduled Instance IDs.
-- * 'slotStartTimeRange' - The time period for the first schedule to start.
mkDescribeScheduledInstances ::
  DescribeScheduledInstances
mkDescribeScheduledInstances =
  DescribeScheduledInstances'
    { filters = Lude.Nothing,
      slotStartTimeRange = Lude.Nothing,
      nextToken = Lude.Nothing,
      scheduledInstanceIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters.
--
--
--     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).
--
--
--     * @instance-type@ - The instance type (for example, @c4.large@ ).
--
--
--     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
--
--     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiFilters :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe [Filter])
dsiFilters = Lens.lens (filters :: DescribeScheduledInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'slotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe SlotStartTimeRangeRequest)
dsiSlotStartTimeRange = Lens.lens (slotStartTimeRange :: DescribeScheduledInstances -> Lude.Maybe SlotStartTimeRangeRequest) (\s a -> s {slotStartTimeRange = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiSlotStartTimeRange "Use generic-lens or generic-optics with 'slotStartTimeRange' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiNextToken :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe Lude.Text)
dsiNextToken = Lens.lens (nextToken :: DescribeScheduledInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Scheduled Instance IDs.
--
-- /Note:/ Consider using 'scheduledInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiScheduledInstanceIds :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe [Lude.Text])
dsiScheduledInstanceIds = Lens.lens (scheduledInstanceIds :: DescribeScheduledInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {scheduledInstanceIds = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiScheduledInstanceIds "Use generic-lens or generic-optics with 'scheduledInstanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDryRun :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe Lude.Bool)
dsiDryRun = Lens.lens (dryRun :: DescribeScheduledInstances -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiMaxResults :: Lens.Lens' DescribeScheduledInstances (Lude.Maybe Lude.Int)
dsiMaxResults = Lens.lens (maxResults :: DescribeScheduledInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeScheduledInstances)
{-# DEPRECATED dsiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeScheduledInstances where
  page rq rs
    | Page.stop (rs Lens.^. dsirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsirsScheduledInstanceSet) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsiNextToken Lens..~ rs Lens.^. dsirsNextToken

instance Lude.AWSRequest DescribeScheduledInstances where
  type
    Rs DescribeScheduledInstances =
      DescribeScheduledInstancesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeScheduledInstancesResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "scheduledInstanceSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScheduledInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScheduledInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScheduledInstances where
  toQuery DescribeScheduledInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeScheduledInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "SlotStartTimeRange" Lude.=: slotStartTimeRange,
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          ( Lude.toQueryList "ScheduledInstanceId"
              Lude.<$> scheduledInstanceIds
          ),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeScheduledInstances.
--
-- /See:/ 'mkDescribeScheduledInstancesResponse' smart constructor.
data DescribeScheduledInstancesResponse = DescribeScheduledInstancesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    scheduledInstanceSet ::
      Lude.Maybe
        [ScheduledInstance],
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

-- | Creates a value of 'DescribeScheduledInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'scheduledInstanceSet' - Information about the Scheduled Instances.
mkDescribeScheduledInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduledInstancesResponse
mkDescribeScheduledInstancesResponse pResponseStatus_ =
  DescribeScheduledInstancesResponse'
    { nextToken = Lude.Nothing,
      scheduledInstanceSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirsNextToken :: Lens.Lens' DescribeScheduledInstancesResponse (Lude.Maybe Lude.Text)
dsirsNextToken = Lens.lens (nextToken :: DescribeScheduledInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledInstancesResponse)
{-# DEPRECATED dsirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirsScheduledInstanceSet :: Lens.Lens' DescribeScheduledInstancesResponse (Lude.Maybe [ScheduledInstance])
dsirsScheduledInstanceSet = Lens.lens (scheduledInstanceSet :: DescribeScheduledInstancesResponse -> Lude.Maybe [ScheduledInstance]) (\s a -> s {scheduledInstanceSet = a} :: DescribeScheduledInstancesResponse)
{-# DEPRECATED dsirsScheduledInstanceSet "Use generic-lens or generic-optics with 'scheduledInstanceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirsResponseStatus :: Lens.Lens' DescribeScheduledInstancesResponse Lude.Int
dsirsResponseStatus = Lens.lens (responseStatus :: DescribeScheduledInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduledInstancesResponse)
{-# DEPRECATED dsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

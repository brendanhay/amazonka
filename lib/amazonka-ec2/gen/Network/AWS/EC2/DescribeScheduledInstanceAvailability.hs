{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeScheduledInstanceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds available schedules that meet the specified criteria.
--
-- You can search for an available schedule no more than 3 months in advance. You must meet the minimum required duration of 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
-- After you find a schedule that meets your needs, call 'PurchaseScheduledInstances' to purchase Scheduled Instances with that schedule.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeScheduledInstanceAvailability
  ( -- * Creating a request
    DescribeScheduledInstanceAvailability (..),
    mkDescribeScheduledInstanceAvailability,

    -- ** Request lenses
    dsiaMinSlotDurationInHours,
    dsiaFilters,
    dsiaFirstSlotStartTimeRange,
    dsiaNextToken,
    dsiaRecurrence,
    dsiaMaxSlotDurationInHours,
    dsiaDryRun,
    dsiaMaxResults,

    -- * Destructuring the response
    DescribeScheduledInstanceAvailabilityResponse (..),
    mkDescribeScheduledInstanceAvailabilityResponse,

    -- ** Response lenses
    dsiarsScheduledInstanceAvailabilitySet,
    dsiarsNextToken,
    dsiarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeScheduledInstanceAvailability.
--
-- /See:/ 'mkDescribeScheduledInstanceAvailability' smart constructor.
data DescribeScheduledInstanceAvailability = DescribeScheduledInstanceAvailability'
  { -- | The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
    minSlotDurationInHours :: Lude.Maybe Lude.Int,
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
    filters :: Lude.Maybe [Filter],
    -- | The time period for the first schedule to start.
    firstSlotStartTimeRange :: SlotDateTimeRangeRequest,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The schedule recurrence.
    recurrence :: ScheduledInstanceRecurrenceRequest,
    -- | The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
    maxSlotDurationInHours :: Lude.Maybe Lude.Int,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledInstanceAvailability' with the minimum fields required to make a request.
--
-- * 'minSlotDurationInHours' - The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
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
-- * 'firstSlotStartTimeRange' - The time period for the first schedule to start.
-- * 'nextToken' - The token for the next set of results.
-- * 'recurrence' - The schedule recurrence.
-- * 'maxSlotDurationInHours' - The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkDescribeScheduledInstanceAvailability ::
  -- | 'firstSlotStartTimeRange'
  SlotDateTimeRangeRequest ->
  -- | 'recurrence'
  ScheduledInstanceRecurrenceRequest ->
  DescribeScheduledInstanceAvailability
mkDescribeScheduledInstanceAvailability
  pFirstSlotStartTimeRange_
  pRecurrence_ =
    DescribeScheduledInstanceAvailability'
      { minSlotDurationInHours =
          Lude.Nothing,
        filters = Lude.Nothing,
        firstSlotStartTimeRange = pFirstSlotStartTimeRange_,
        nextToken = Lude.Nothing,
        recurrence = pRecurrence_,
        maxSlotDurationInHours = Lude.Nothing,
        dryRun = Lude.Nothing,
        maxResults = Lude.Nothing
      }

-- | The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
--
-- /Note:/ Consider using 'minSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMinSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe Lude.Int)
dsiaMinSlotDurationInHours = Lens.lens (minSlotDurationInHours :: DescribeScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {minSlotDurationInHours = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaMinSlotDurationInHours "Use generic-lens or generic-optics with 'minSlotDurationInHours' instead." #-}

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
dsiaFilters :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe [Filter])
dsiaFilters = Lens.lens (filters :: DescribeScheduledInstanceAvailability -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'firstSlotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaFirstSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstanceAvailability SlotDateTimeRangeRequest
dsiaFirstSlotStartTimeRange = Lens.lens (firstSlotStartTimeRange :: DescribeScheduledInstanceAvailability -> SlotDateTimeRangeRequest) (\s a -> s {firstSlotStartTimeRange = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaFirstSlotStartTimeRange "Use generic-lens or generic-optics with 'firstSlotStartTimeRange' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaNextToken :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe Lude.Text)
dsiaNextToken = Lens.lens (nextToken :: DescribeScheduledInstanceAvailability -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaRecurrence :: Lens.Lens' DescribeScheduledInstanceAvailability ScheduledInstanceRecurrenceRequest
dsiaRecurrence = Lens.lens (recurrence :: DescribeScheduledInstanceAvailability -> ScheduledInstanceRecurrenceRequest) (\s a -> s {recurrence = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
--
-- /Note:/ Consider using 'maxSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe Lude.Int)
dsiaMaxSlotDurationInHours = Lens.lens (maxSlotDurationInHours :: DescribeScheduledInstanceAvailability -> Lude.Maybe Lude.Int) (\s a -> s {maxSlotDurationInHours = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaMaxSlotDurationInHours "Use generic-lens or generic-optics with 'maxSlotDurationInHours' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaDryRun :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe Lude.Bool)
dsiaDryRun = Lens.lens (dryRun :: DescribeScheduledInstanceAvailability -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxResults :: Lens.Lens' DescribeScheduledInstanceAvailability (Lude.Maybe Lude.Natural)
dsiaMaxResults = Lens.lens (maxResults :: DescribeScheduledInstanceAvailability -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeScheduledInstanceAvailability)
{-# DEPRECATED dsiaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeScheduledInstanceAvailability where
  page rq rs
    | Page.stop (rs Lens.^. dsiarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsiarsScheduledInstanceAvailabilitySet) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsiaNextToken Lens..~ rs Lens.^. dsiarsNextToken

instance Lude.AWSRequest DescribeScheduledInstanceAvailability where
  type
    Rs DescribeScheduledInstanceAvailability =
      DescribeScheduledInstanceAvailabilityResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeScheduledInstanceAvailabilityResponse'
            Lude.<$> ( x Lude..@? "scheduledInstanceAvailabilitySet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScheduledInstanceAvailability where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScheduledInstanceAvailability where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScheduledInstanceAvailability where
  toQuery DescribeScheduledInstanceAvailability' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeScheduledInstanceAvailability" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "MinSlotDurationInHours" Lude.=: minSlotDurationInHours,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "FirstSlotStartTimeRange" Lude.=: firstSlotStartTimeRange,
        "NextToken" Lude.=: nextToken,
        "Recurrence" Lude.=: recurrence,
        "MaxSlotDurationInHours" Lude.=: maxSlotDurationInHours,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeScheduledInstanceAvailability.
--
-- /See:/ 'mkDescribeScheduledInstanceAvailabilityResponse' smart constructor.
data DescribeScheduledInstanceAvailabilityResponse = DescribeScheduledInstanceAvailabilityResponse'
  { -- | Information about the available Scheduled Instances.
    scheduledInstanceAvailabilitySet :: Lude.Maybe [ScheduledInstanceAvailability],
    -- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledInstanceAvailabilityResponse' with the minimum fields required to make a request.
--
-- * 'scheduledInstanceAvailabilitySet' - Information about the available Scheduled Instances.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeScheduledInstanceAvailabilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduledInstanceAvailabilityResponse
mkDescribeScheduledInstanceAvailabilityResponse pResponseStatus_ =
  DescribeScheduledInstanceAvailabilityResponse'
    { scheduledInstanceAvailabilitySet =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the available Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceAvailabilitySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarsScheduledInstanceAvailabilitySet :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Lude.Maybe [ScheduledInstanceAvailability])
dsiarsScheduledInstanceAvailabilitySet = Lens.lens (scheduledInstanceAvailabilitySet :: DescribeScheduledInstanceAvailabilityResponse -> Lude.Maybe [ScheduledInstanceAvailability]) (\s a -> s {scheduledInstanceAvailabilitySet = a} :: DescribeScheduledInstanceAvailabilityResponse)
{-# DEPRECATED dsiarsScheduledInstanceAvailabilitySet "Use generic-lens or generic-optics with 'scheduledInstanceAvailabilitySet' instead." #-}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarsNextToken :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Lude.Maybe Lude.Text)
dsiarsNextToken = Lens.lens (nextToken :: DescribeScheduledInstanceAvailabilityResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledInstanceAvailabilityResponse)
{-# DEPRECATED dsiarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarsResponseStatus :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse Lude.Int
dsiarsResponseStatus = Lens.lens (responseStatus :: DescribeScheduledInstanceAvailabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduledInstanceAvailabilityResponse)
{-# DEPRECATED dsiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

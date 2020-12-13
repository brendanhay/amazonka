{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified instances or all of your instances. By default, only running instances are described, unless you specifically indicate to return the status of all instances.
--
-- Instance status includes the following components:
--
--     * __Status checks__ - Amazon EC2 performs status checks on running EC2 instances to identify hardware and software issues. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html Status checks for your instances> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshooting instances with failed status checks> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--     * __Scheduled events__ - Amazon EC2 can schedule events (such as reboot, stop, or terminate) for your instances related to hardware issues, software updates, or system maintenance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html Scheduled events for your instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--     * __Instance state__ - You can manage your instances from the moment you launch them through their termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceStatus
  ( -- * Creating a request
    DescribeInstanceStatus (..),
    mkDescribeInstanceStatus,

    -- ** Request lenses
    dissIncludeAllInstances,
    dissFilters,
    dissNextToken,
    dissInstanceIds,
    dissDryRun,
    dissMaxResults,

    -- * Destructuring the response
    DescribeInstanceStatusResponse (..),
    mkDescribeInstanceStatusResponse,

    -- ** Response lenses
    disrsInstanceStatuses,
    disrsNextToken,
    disrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceStatus' smart constructor.
data DescribeInstanceStatus = DescribeInstanceStatus'
  { -- | When @true@ , includes the health status for all instances. When @false@ , includes the health status for running instances only.
    --
    -- Default: @false@
    includeAllInstances :: Lude.Maybe Lude.Bool,
    -- | The filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone of the instance.
    --
    --
    --     * @event.code@ - The code for the scheduled event (@instance-reboot@ | @system-reboot@ | @system-maintenance@ | @instance-retirement@ | @instance-stop@ ).
    --
    --
    --     * @event.description@ - A description of the event.
    --
    --
    --     * @event.instance-event-id@ - The ID of the event whose date and time you are modifying.
    --
    --
    --     * @event.not-after@ - The latest end time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
    --
    --
    --     * @event.not-before@ - The earliest start time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
    --
    --
    --     * @event.not-before-deadline@ - The deadline for starting the event (for example, @2014-09-15T17:15:20.000Z@ ).
    --
    --
    --     * @instance-state-code@ - The code for the instance state, as a 16-bit unsigned integer. The high byte is used for internal purposes and should be ignored. The low byte is set based on the state represented. The valid values are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
    --
    --
    --     * @instance-state-name@ - The state of the instance (@pending@ | @running@ | @shutting-down@ | @terminated@ | @stopping@ | @stopped@ ).
    --
    --
    --     * @instance-status.reachability@ - Filters on instance status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
    --
    --
    --     * @instance-status.status@ - The status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
    --
    --
    --     * @system-status.reachability@ - Filters on system status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
    --
    --
    --     * @system-status.status@ - The system status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    -- Constraints: Maximum 100 explicitly specified instance IDs.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceStatus' with the minimum fields required to make a request.
--
-- * 'includeAllInstances' - When @true@ , includes the health status for all instances. When @false@ , includes the health status for running instances only.
--
-- Default: @false@
-- * 'filters' - The filters.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.code@ - The code for the scheduled event (@instance-reboot@ | @system-reboot@ | @system-maintenance@ | @instance-retirement@ | @instance-stop@ ).
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.instance-event-id@ - The ID of the event whose date and time you are modifying.
--
--
--     * @event.not-after@ - The latest end time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @event.not-before@ - The earliest start time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @event.not-before-deadline@ - The deadline for starting the event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @instance-state-code@ - The code for the instance state, as a 16-bit unsigned integer. The high byte is used for internal purposes and should be ignored. The low byte is set based on the state represented. The valid values are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
--
--     * @instance-state-name@ - The state of the instance (@pending@ | @running@ | @shutting-down@ | @terminated@ | @stopping@ | @stopped@ ).
--
--
--     * @instance-status.reachability@ - Filters on instance status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
--
--
--     * @instance-status.status@ - The status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
--
--
--     * @system-status.reachability@ - Filters on system status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
--
--
--     * @system-status.status@ - The system status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
-- Constraints: Maximum 100 explicitly specified instance IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
mkDescribeInstanceStatus ::
  DescribeInstanceStatus
mkDescribeInstanceStatus =
  DescribeInstanceStatus'
    { includeAllInstances = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      instanceIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When @true@ , includes the health status for all instances. When @false@ , includes the health status for running instances only.
--
-- Default: @false@
--
-- /Note:/ Consider using 'includeAllInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissIncludeAllInstances :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe Lude.Bool)
dissIncludeAllInstances = Lens.lens (includeAllInstances :: DescribeInstanceStatus -> Lude.Maybe Lude.Bool) (\s a -> s {includeAllInstances = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissIncludeAllInstances "Use generic-lens or generic-optics with 'includeAllInstances' instead." #-}

-- | The filters.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.code@ - The code for the scheduled event (@instance-reboot@ | @system-reboot@ | @system-maintenance@ | @instance-retirement@ | @instance-stop@ ).
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.instance-event-id@ - The ID of the event whose date and time you are modifying.
--
--
--     * @event.not-after@ - The latest end time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @event.not-before@ - The earliest start time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @event.not-before-deadline@ - The deadline for starting the event (for example, @2014-09-15T17:15:20.000Z@ ).
--
--
--     * @instance-state-code@ - The code for the instance state, as a 16-bit unsigned integer. The high byte is used for internal purposes and should be ignored. The low byte is set based on the state represented. The valid values are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
--
--     * @instance-state-name@ - The state of the instance (@pending@ | @running@ | @shutting-down@ | @terminated@ | @stopping@ | @stopped@ ).
--
--
--     * @instance-status.reachability@ - Filters on instance status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
--
--
--     * @instance-status.status@ - The status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
--
--
--     * @system-status.reachability@ - Filters on system status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).
--
--
--     * @system-status.status@ - The system status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissFilters :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe [Filter])
dissFilters = Lens.lens (filters :: DescribeInstanceStatus -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissNextToken :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe Lude.Text)
dissNextToken = Lens.lens (nextToken :: DescribeInstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instance IDs.
--
-- Default: Describes all your instances.
-- Constraints: Maximum 100 explicitly specified instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissInstanceIds :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe [Lude.Text])
dissInstanceIds = Lens.lens (instanceIds :: DescribeInstanceStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissDryRun :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe Lude.Bool)
dissDryRun = Lens.lens (dryRun :: DescribeInstanceStatus -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dissMaxResults :: Lens.Lens' DescribeInstanceStatus (Lude.Maybe Lude.Int)
dissMaxResults = Lens.lens (maxResults :: DescribeInstanceStatus -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeInstanceStatus)
{-# DEPRECATED dissMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceStatus where
  page rq rs
    | Page.stop (rs Lens.^. disrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. disrsInstanceStatuses) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dissNextToken Lens..~ rs Lens.^. disrsNextToken

instance Lude.AWSRequest DescribeInstanceStatus where
  type Rs DescribeInstanceStatus = DescribeInstanceStatusResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceStatusResponse'
            Lude.<$> ( x Lude..@? "instanceStatusSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceStatus where
  toQuery DescribeInstanceStatus' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstanceStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "IncludeAllInstances" Lude.=: includeAllInstances,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery (Lude.toQueryList "InstanceId" Lude.<$> instanceIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInstanceStatusResponse' smart constructor.
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'
  { -- | Information about the status of the instances.
    instanceStatuses :: Lude.Maybe [InstanceStatus],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceStatusResponse' with the minimum fields required to make a request.
--
-- * 'instanceStatuses' - Information about the status of the instances.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceStatusResponse
mkDescribeInstanceStatusResponse pResponseStatus_ =
  DescribeInstanceStatusResponse'
    { instanceStatuses = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the status of the instances.
--
-- /Note:/ Consider using 'instanceStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsInstanceStatuses :: Lens.Lens' DescribeInstanceStatusResponse (Lude.Maybe [InstanceStatus])
disrsInstanceStatuses = Lens.lens (instanceStatuses :: DescribeInstanceStatusResponse -> Lude.Maybe [InstanceStatus]) (\s a -> s {instanceStatuses = a} :: DescribeInstanceStatusResponse)
{-# DEPRECATED disrsInstanceStatuses "Use generic-lens or generic-optics with 'instanceStatuses' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsNextToken :: Lens.Lens' DescribeInstanceStatusResponse (Lude.Maybe Lude.Text)
disrsNextToken = Lens.lens (nextToken :: DescribeInstanceStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceStatusResponse)
{-# DEPRECATED disrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DescribeInstanceStatusResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceStatusResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

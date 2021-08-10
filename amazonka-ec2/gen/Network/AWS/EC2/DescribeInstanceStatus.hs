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
-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified instances or all of your
-- instances. By default, only running instances are described, unless you
-- specifically indicate to return the status of all instances.
--
-- Instance status includes the following components:
--
-- -   __Status checks__ - Amazon EC2 performs status checks on running EC2
--     instances to identify hardware and software issues. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html Status checks for your instances>
--     and
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshooting instances with failed status checks>
--     in the /Amazon EC2 User Guide/.
--
-- -   __Scheduled events__ - Amazon EC2 can schedule events (such as
--     reboot, stop, or terminate) for your instances related to hardware
--     issues, software updates, or system maintenance. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html Scheduled events for your instances>
--     in the /Amazon EC2 User Guide/.
--
-- -   __Instance state__ - You can manage your instances from the moment
--     you launch them through their termination. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle>
--     in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceStatus
  ( -- * Creating a Request
    DescribeInstanceStatus (..),
    newDescribeInstanceStatus,

    -- * Request Lenses
    describeInstanceStatus_instanceIds,
    describeInstanceStatus_nextToken,
    describeInstanceStatus_dryRun,
    describeInstanceStatus_maxResults,
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_filters,

    -- * Destructuring the Response
    DescribeInstanceStatusResponse (..),
    newDescribeInstanceStatusResponse,

    -- * Response Lenses
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceStatus' smart constructor.
data DescribeInstanceStatus = DescribeInstanceStatus'
  { -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    --
    -- Constraints: Maximum 100 explicitly specified instance IDs.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 5 and 1000. You cannot specify this
    -- parameter and the instance IDs parameter in the same call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | When @true@, includes the health status for all instances. When @false@,
    -- includes the health status for running instances only.
    --
    -- Default: @false@
    includeAllInstances :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @availability-zone@ - The Availability Zone of the instance.
    --
    -- -   @event.code@ - The code for the scheduled event (@instance-reboot@ |
    --     @system-reboot@ | @system-maintenance@ | @instance-retirement@ |
    --     @instance-stop@).
    --
    -- -   @event.description@ - A description of the event.
    --
    -- -   @event.instance-event-id@ - The ID of the event whose date and time
    --     you are modifying.
    --
    -- -   @event.not-after@ - The latest end time for the scheduled event (for
    --     example, @2014-09-15T17:15:20.000Z@).
    --
    -- -   @event.not-before@ - The earliest start time for the scheduled event
    --     (for example, @2014-09-15T17:15:20.000Z@).
    --
    -- -   @event.not-before-deadline@ - The deadline for starting the event
    --     (for example, @2014-09-15T17:15:20.000Z@).
    --
    -- -   @instance-state-code@ - The code for the instance state, as a 16-bit
    --     unsigned integer. The high byte is used for internal purposes and
    --     should be ignored. The low byte is set based on the state
    --     represented. The valid values are 0 (pending), 16 (running), 32
    --     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
    --
    -- -   @instance-state-name@ - The state of the instance (@pending@ |
    --     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
    --     @stopped@).
    --
    -- -   @instance-status.reachability@ - Filters on instance status where
    --     the name is @reachability@ (@passed@ | @failed@ | @initializing@ |
    --     @insufficient-data@).
    --
    -- -   @instance-status.status@ - The status of the instance (@ok@ |
    --     @impaired@ | @initializing@ | @insufficient-data@ |
    --     @not-applicable@).
    --
    -- -   @system-status.reachability@ - Filters on system status where the
    --     name is @reachability@ (@passed@ | @failed@ | @initializing@ |
    --     @insufficient-data@).
    --
    -- -   @system-status.status@ - The system status of the instance (@ok@ |
    --     @impaired@ | @initializing@ | @insufficient-data@ |
    --     @not-applicable@).
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeInstanceStatus_instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
--
-- 'nextToken', 'describeInstanceStatus_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeInstanceStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeInstanceStatus_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
--
-- 'includeAllInstances', 'describeInstanceStatus_includeAllInstances' - When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
--
-- 'filters', 'describeInstanceStatus_filters' - The filters.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.code@ - The code for the scheduled event (@instance-reboot@ |
--     @system-reboot@ | @system-maintenance@ | @instance-retirement@ |
--     @instance-stop@).
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.instance-event-id@ - The ID of the event whose date and time
--     you are modifying.
--
-- -   @event.not-after@ - The latest end time for the scheduled event (for
--     example, @2014-09-15T17:15:20.000Z@).
--
-- -   @event.not-before@ - The earliest start time for the scheduled event
--     (for example, @2014-09-15T17:15:20.000Z@).
--
-- -   @event.not-before-deadline@ - The deadline for starting the event
--     (for example, @2014-09-15T17:15:20.000Z@).
--
-- -   @instance-state-code@ - The code for the instance state, as a 16-bit
--     unsigned integer. The high byte is used for internal purposes and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   @instance-state-name@ - The state of the instance (@pending@ |
--     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
--     @stopped@).
--
-- -   @instance-status.reachability@ - Filters on instance status where
--     the name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @instance-status.status@ - The status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
--
-- -   @system-status.reachability@ - Filters on system status where the
--     name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @system-status.status@ - The system status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
newDescribeInstanceStatus ::
  DescribeInstanceStatus
newDescribeInstanceStatus =
  DescribeInstanceStatus'
    { instanceIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeAllInstances = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
describeInstanceStatus_instanceIds :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe [Prelude.Text])
describeInstanceStatus_instanceIds = Lens.lens (\DescribeInstanceStatus' {instanceIds} -> instanceIds) (\s@DescribeInstanceStatus' {} a -> s {instanceIds = a} :: DescribeInstanceStatus) Prelude.. Lens.mapping Lens._Coerce

-- | The token to retrieve the next page of results.
describeInstanceStatus_nextToken :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Text)
describeInstanceStatus_nextToken = Lens.lens (\DescribeInstanceStatus' {nextToken} -> nextToken) (\s@DescribeInstanceStatus' {} a -> s {nextToken = a} :: DescribeInstanceStatus)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceStatus_dryRun :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Bool)
describeInstanceStatus_dryRun = Lens.lens (\DescribeInstanceStatus' {dryRun} -> dryRun) (\s@DescribeInstanceStatus' {} a -> s {dryRun = a} :: DescribeInstanceStatus)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000. You cannot specify this
-- parameter and the instance IDs parameter in the same call.
describeInstanceStatus_maxResults :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Int)
describeInstanceStatus_maxResults = Lens.lens (\DescribeInstanceStatus' {maxResults} -> maxResults) (\s@DescribeInstanceStatus' {} a -> s {maxResults = a} :: DescribeInstanceStatus)

-- | When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
describeInstanceStatus_includeAllInstances :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Bool)
describeInstanceStatus_includeAllInstances = Lens.lens (\DescribeInstanceStatus' {includeAllInstances} -> includeAllInstances) (\s@DescribeInstanceStatus' {} a -> s {includeAllInstances = a} :: DescribeInstanceStatus)

-- | The filters.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.code@ - The code for the scheduled event (@instance-reboot@ |
--     @system-reboot@ | @system-maintenance@ | @instance-retirement@ |
--     @instance-stop@).
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.instance-event-id@ - The ID of the event whose date and time
--     you are modifying.
--
-- -   @event.not-after@ - The latest end time for the scheduled event (for
--     example, @2014-09-15T17:15:20.000Z@).
--
-- -   @event.not-before@ - The earliest start time for the scheduled event
--     (for example, @2014-09-15T17:15:20.000Z@).
--
-- -   @event.not-before-deadline@ - The deadline for starting the event
--     (for example, @2014-09-15T17:15:20.000Z@).
--
-- -   @instance-state-code@ - The code for the instance state, as a 16-bit
--     unsigned integer. The high byte is used for internal purposes and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   @instance-state-name@ - The state of the instance (@pending@ |
--     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
--     @stopped@).
--
-- -   @instance-status.reachability@ - Filters on instance status where
--     the name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @instance-status.status@ - The status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
--
-- -   @system-status.reachability@ - Filters on system status where the
--     name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @system-status.status@ - The system status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
describeInstanceStatus_filters :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe [Filter])
describeInstanceStatus_filters = Lens.lens (\DescribeInstanceStatus' {filters} -> filters) (\s@DescribeInstanceStatus' {} a -> s {filters = a} :: DescribeInstanceStatus) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeInstanceStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInstanceStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInstanceStatusResponse_instanceStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeInstanceStatus_nextToken
          Lens..~ rs
          Lens.^? describeInstanceStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstanceStatus where
  type
    AWSResponse DescribeInstanceStatus =
      DescribeInstanceStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceStatusResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "instanceStatusSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceStatus

instance Prelude.NFData DescribeInstanceStatus

instance Core.ToHeaders DescribeInstanceStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstanceStatus where
  toQuery DescribeInstanceStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceStatus" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "IncludeAllInstances" Core.=: includeAllInstances,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeInstanceStatusResponse' smart constructor.
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the instances.
    instanceStatuses :: Prelude.Maybe [InstanceStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstanceStatusResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'instanceStatuses', 'describeInstanceStatusResponse_instanceStatuses' - Information about the status of the instances.
--
-- 'httpStatus', 'describeInstanceStatusResponse_httpStatus' - The response's http status code.
newDescribeInstanceStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceStatusResponse
newDescribeInstanceStatusResponse pHttpStatus_ =
  DescribeInstanceStatusResponse'
    { nextToken =
        Prelude.Nothing,
      instanceStatuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInstanceStatusResponse_nextToken :: Lens.Lens' DescribeInstanceStatusResponse (Prelude.Maybe Prelude.Text)
describeInstanceStatusResponse_nextToken = Lens.lens (\DescribeInstanceStatusResponse' {nextToken} -> nextToken) (\s@DescribeInstanceStatusResponse' {} a -> s {nextToken = a} :: DescribeInstanceStatusResponse)

-- | Information about the status of the instances.
describeInstanceStatusResponse_instanceStatuses :: Lens.Lens' DescribeInstanceStatusResponse (Prelude.Maybe [InstanceStatus])
describeInstanceStatusResponse_instanceStatuses = Lens.lens (\DescribeInstanceStatusResponse' {instanceStatuses} -> instanceStatuses) (\s@DescribeInstanceStatusResponse' {} a -> s {instanceStatuses = a} :: DescribeInstanceStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstanceStatusResponse_httpStatus :: Lens.Lens' DescribeInstanceStatusResponse Prelude.Int
describeInstanceStatusResponse_httpStatus = Lens.lens (\DescribeInstanceStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceStatusResponse' {} a -> s {httpStatus = a} :: DescribeInstanceStatusResponse)

instance
  Prelude.NFData
    DescribeInstanceStatusResponse

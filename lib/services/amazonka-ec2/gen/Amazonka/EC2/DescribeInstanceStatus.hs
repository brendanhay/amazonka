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
-- Module      : Amazonka.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshoot instances with failed status checks>
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
module Amazonka.EC2.DescribeInstanceStatus
  ( -- * Creating a Request
    DescribeInstanceStatus (..),
    newDescribeInstanceStatus,

    -- * Request Lenses
    describeInstanceStatus_dryRun,
    describeInstanceStatus_filters,
    describeInstanceStatus_includeAllInstances,
    describeInstanceStatus_instanceIds,
    describeInstanceStatus_maxResults,
    describeInstanceStatus_nextToken,

    -- * Destructuring the Response
    DescribeInstanceStatusResponse (..),
    newDescribeInstanceStatusResponse,

    -- * Response Lenses
    describeInstanceStatusResponse_instanceStatuses,
    describeInstanceStatusResponse_nextToken,
    describeInstanceStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstanceStatus' smart constructor.
data DescribeInstanceStatus = DescribeInstanceStatus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    filters :: Prelude.Maybe [Filter],
    -- | When @true@, includes the health status for all instances. When @false@,
    -- includes the health status for running instances only.
    --
    -- Default: @false@
    includeAllInstances :: Prelude.Maybe Prelude.Bool,
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    --
    -- Constraints: Maximum 100 explicitly specified instance IDs.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    --
    -- You cannot specify this parameter and the instance IDs parameter in the
    -- same request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'describeInstanceStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
--
-- 'includeAllInstances', 'describeInstanceStatus_includeAllInstances' - When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
--
-- 'instanceIds', 'describeInstanceStatus_instanceIds' - The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
--
-- 'maxResults', 'describeInstanceStatus_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- You cannot specify this parameter and the instance IDs parameter in the
-- same request.
--
-- 'nextToken', 'describeInstanceStatus_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
newDescribeInstanceStatus ::
  DescribeInstanceStatus
newDescribeInstanceStatus =
  DescribeInstanceStatus'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      includeAllInstances = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceStatus_dryRun :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Bool)
describeInstanceStatus_dryRun = Lens.lens (\DescribeInstanceStatus' {dryRun} -> dryRun) (\s@DescribeInstanceStatus' {} a -> s {dryRun = a} :: DescribeInstanceStatus)

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
describeInstanceStatus_filters = Lens.lens (\DescribeInstanceStatus' {filters} -> filters) (\s@DescribeInstanceStatus' {} a -> s {filters = a} :: DescribeInstanceStatus) Prelude.. Lens.mapping Lens.coerced

-- | When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
describeInstanceStatus_includeAllInstances :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Bool)
describeInstanceStatus_includeAllInstances = Lens.lens (\DescribeInstanceStatus' {includeAllInstances} -> includeAllInstances) (\s@DescribeInstanceStatus' {} a -> s {includeAllInstances = a} :: DescribeInstanceStatus)

-- | The instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
describeInstanceStatus_instanceIds :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe [Prelude.Text])
describeInstanceStatus_instanceIds = Lens.lens (\DescribeInstanceStatus' {instanceIds} -> instanceIds) (\s@DescribeInstanceStatus' {} a -> s {instanceIds = a} :: DescribeInstanceStatus) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- You cannot specify this parameter and the instance IDs parameter in the
-- same request.
describeInstanceStatus_maxResults :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Int)
describeInstanceStatus_maxResults = Lens.lens (\DescribeInstanceStatus' {maxResults} -> maxResults) (\s@DescribeInstanceStatus' {} a -> s {maxResults = a} :: DescribeInstanceStatus)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeInstanceStatus_nextToken :: Lens.Lens' DescribeInstanceStatus (Prelude.Maybe Prelude.Text)
describeInstanceStatus_nextToken = Lens.lens (\DescribeInstanceStatus' {nextToken} -> nextToken) (\s@DescribeInstanceStatus' {} a -> s {nextToken = a} :: DescribeInstanceStatus)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeInstanceStatus_nextToken
          Lens..~ rs
          Lens.^? describeInstanceStatusResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeInstanceStatus where
  type
    AWSResponse DescribeInstanceStatus =
      DescribeInstanceStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceStatusResponse'
            Prelude.<$> ( x
                            Data..@? "instanceStatusSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceStatus where
  hashWithSalt _salt DescribeInstanceStatus' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includeAllInstances
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeInstanceStatus where
  rnf DescribeInstanceStatus' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeAllInstances
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeInstanceStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstanceStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstanceStatus where
  toQuery DescribeInstanceStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeInstanceStatus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "IncludeAllInstances" Data.=: includeAllInstances,
        Data.toQuery
          ( Data.toQueryList "InstanceId"
              Prelude.<$> instanceIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeInstanceStatusResponse' smart constructor.
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'
  { -- | Information about the status of the instances.
    instanceStatuses :: Prelude.Maybe [InstanceStatus],
    -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'instanceStatuses', 'describeInstanceStatusResponse_instanceStatuses' - Information about the status of the instances.
--
-- 'nextToken', 'describeInstanceStatusResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'httpStatus', 'describeInstanceStatusResponse_httpStatus' - The response's http status code.
newDescribeInstanceStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceStatusResponse
newDescribeInstanceStatusResponse pHttpStatus_ =
  DescribeInstanceStatusResponse'
    { instanceStatuses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the status of the instances.
describeInstanceStatusResponse_instanceStatuses :: Lens.Lens' DescribeInstanceStatusResponse (Prelude.Maybe [InstanceStatus])
describeInstanceStatusResponse_instanceStatuses = Lens.lens (\DescribeInstanceStatusResponse' {instanceStatuses} -> instanceStatuses) (\s@DescribeInstanceStatusResponse' {} a -> s {instanceStatuses = a} :: DescribeInstanceStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeInstanceStatusResponse_nextToken :: Lens.Lens' DescribeInstanceStatusResponse (Prelude.Maybe Prelude.Text)
describeInstanceStatusResponse_nextToken = Lens.lens (\DescribeInstanceStatusResponse' {nextToken} -> nextToken) (\s@DescribeInstanceStatusResponse' {} a -> s {nextToken = a} :: DescribeInstanceStatusResponse)

-- | The response's http status code.
describeInstanceStatusResponse_httpStatus :: Lens.Lens' DescribeInstanceStatusResponse Prelude.Int
describeInstanceStatusResponse_httpStatus = Lens.lens (\DescribeInstanceStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceStatusResponse' {} a -> s {httpStatus = a} :: DescribeInstanceStatusResponse)

instance
  Prelude.NFData
    DescribeInstanceStatusResponse
  where
  rnf DescribeInstanceStatusResponse' {..} =
    Prelude.rnf instanceStatuses
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

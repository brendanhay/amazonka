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
-- Module      : Network.AWS.EC2.RunScheduledInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified Scheduled Instances.
--
-- Before you can launch a Scheduled Instance, you must purchase it and
-- obtain an identifier using PurchaseScheduledInstances.
--
-- You must launch a Scheduled Instance during its scheduled time period.
-- You can\'t stop or reboot a Scheduled Instance, but you can terminate it
-- as needed. If you terminate a Scheduled Instance before the current
-- scheduled time period ends, you can launch it again after a few minutes.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-scheduled-instances.html Scheduled Instances>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.RunScheduledInstances
  ( -- * Creating a Request
    RunScheduledInstances (..),
    newRunScheduledInstances,

    -- * Request Lenses
    runScheduledInstances_dryRun,
    runScheduledInstances_clientToken,
    runScheduledInstances_instanceCount,
    runScheduledInstances_launchSpecification,
    runScheduledInstances_scheduledInstanceId,

    -- * Destructuring the Response
    RunScheduledInstancesResponse (..),
    newRunScheduledInstancesResponse,

    -- * Response Lenses
    runScheduledInstancesResponse_instanceIdSet,
    runScheduledInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RunScheduledInstances.
--
-- /See:/ 'newRunScheduledInstances' smart constructor.
data RunScheduledInstances = RunScheduledInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Unique, case-sensitive identifier that ensures the idempotency of the
    -- request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The number of instances.
    --
    -- Default: 1
    instanceCount :: Core.Maybe Core.Int,
    -- | The launch specification. You must match the instance type, Availability
    -- Zone, network, and platform of the schedule that you purchased.
    launchSpecification :: ScheduledInstancesLaunchSpecification,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunScheduledInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'runScheduledInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'runScheduledInstances_clientToken' - Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'instanceCount', 'runScheduledInstances_instanceCount' - The number of instances.
--
-- Default: 1
--
-- 'launchSpecification', 'runScheduledInstances_launchSpecification' - The launch specification. You must match the instance type, Availability
-- Zone, network, and platform of the schedule that you purchased.
--
-- 'scheduledInstanceId', 'runScheduledInstances_scheduledInstanceId' - The Scheduled Instance ID.
newRunScheduledInstances ::
  -- | 'launchSpecification'
  ScheduledInstancesLaunchSpecification ->
  -- | 'scheduledInstanceId'
  Core.Text ->
  RunScheduledInstances
newRunScheduledInstances
  pLaunchSpecification_
  pScheduledInstanceId_ =
    RunScheduledInstances'
      { dryRun = Core.Nothing,
        clientToken = Core.Nothing,
        instanceCount = Core.Nothing,
        launchSpecification = pLaunchSpecification_,
        scheduledInstanceId = pScheduledInstanceId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
runScheduledInstances_dryRun :: Lens.Lens' RunScheduledInstances (Core.Maybe Core.Bool)
runScheduledInstances_dryRun = Lens.lens (\RunScheduledInstances' {dryRun} -> dryRun) (\s@RunScheduledInstances' {} a -> s {dryRun = a} :: RunScheduledInstances)

-- | Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
runScheduledInstances_clientToken :: Lens.Lens' RunScheduledInstances (Core.Maybe Core.Text)
runScheduledInstances_clientToken = Lens.lens (\RunScheduledInstances' {clientToken} -> clientToken) (\s@RunScheduledInstances' {} a -> s {clientToken = a} :: RunScheduledInstances)

-- | The number of instances.
--
-- Default: 1
runScheduledInstances_instanceCount :: Lens.Lens' RunScheduledInstances (Core.Maybe Core.Int)
runScheduledInstances_instanceCount = Lens.lens (\RunScheduledInstances' {instanceCount} -> instanceCount) (\s@RunScheduledInstances' {} a -> s {instanceCount = a} :: RunScheduledInstances)

-- | The launch specification. You must match the instance type, Availability
-- Zone, network, and platform of the schedule that you purchased.
runScheduledInstances_launchSpecification :: Lens.Lens' RunScheduledInstances ScheduledInstancesLaunchSpecification
runScheduledInstances_launchSpecification = Lens.lens (\RunScheduledInstances' {launchSpecification} -> launchSpecification) (\s@RunScheduledInstances' {} a -> s {launchSpecification = a} :: RunScheduledInstances)

-- | The Scheduled Instance ID.
runScheduledInstances_scheduledInstanceId :: Lens.Lens' RunScheduledInstances Core.Text
runScheduledInstances_scheduledInstanceId = Lens.lens (\RunScheduledInstances' {scheduledInstanceId} -> scheduledInstanceId) (\s@RunScheduledInstances' {} a -> s {scheduledInstanceId = a} :: RunScheduledInstances)

instance Core.AWSRequest RunScheduledInstances where
  type
    AWSResponse RunScheduledInstances =
      RunScheduledInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RunScheduledInstancesResponse'
            Core.<$> ( x Core..@? "instanceIdSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RunScheduledInstances

instance Core.NFData RunScheduledInstances

instance Core.ToHeaders RunScheduledInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RunScheduledInstances where
  toPath = Core.const "/"

instance Core.ToQuery RunScheduledInstances where
  toQuery RunScheduledInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RunScheduledInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ClientToken" Core.=: clientToken,
        "InstanceCount" Core.=: instanceCount,
        "LaunchSpecification" Core.=: launchSpecification,
        "ScheduledInstanceId" Core.=: scheduledInstanceId
      ]

-- | Contains the output of RunScheduledInstances.
--
-- /See:/ 'newRunScheduledInstancesResponse' smart constructor.
data RunScheduledInstancesResponse = RunScheduledInstancesResponse'
  { -- | The IDs of the newly launched instances.
    instanceIdSet :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RunScheduledInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIdSet', 'runScheduledInstancesResponse_instanceIdSet' - The IDs of the newly launched instances.
--
-- 'httpStatus', 'runScheduledInstancesResponse_httpStatus' - The response's http status code.
newRunScheduledInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RunScheduledInstancesResponse
newRunScheduledInstancesResponse pHttpStatus_ =
  RunScheduledInstancesResponse'
    { instanceIdSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the newly launched instances.
runScheduledInstancesResponse_instanceIdSet :: Lens.Lens' RunScheduledInstancesResponse (Core.Maybe [Core.Text])
runScheduledInstancesResponse_instanceIdSet = Lens.lens (\RunScheduledInstancesResponse' {instanceIdSet} -> instanceIdSet) (\s@RunScheduledInstancesResponse' {} a -> s {instanceIdSet = a} :: RunScheduledInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
runScheduledInstancesResponse_httpStatus :: Lens.Lens' RunScheduledInstancesResponse Core.Int
runScheduledInstancesResponse_httpStatus = Lens.lens (\RunScheduledInstancesResponse' {httpStatus} -> httpStatus) (\s@RunScheduledInstancesResponse' {} a -> s {httpStatus = a} :: RunScheduledInstancesResponse)

instance Core.NFData RunScheduledInstancesResponse

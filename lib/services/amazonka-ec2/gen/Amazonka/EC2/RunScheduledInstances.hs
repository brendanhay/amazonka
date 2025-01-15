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
-- Module      : Amazonka.EC2.RunScheduledInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.RunScheduledInstances
  ( -- * Creating a Request
    RunScheduledInstances (..),
    newRunScheduledInstances,

    -- * Request Lenses
    runScheduledInstances_clientToken,
    runScheduledInstances_dryRun,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for RunScheduledInstances.
--
-- /See:/ 'newRunScheduledInstances' smart constructor.
data RunScheduledInstances = RunScheduledInstances'
  { -- | Unique, case-sensitive identifier that ensures the idempotency of the
    -- request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The number of instances.
    --
    -- Default: 1
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The launch specification. You must match the instance type, Availability
    -- Zone, network, and platform of the schedule that you purchased.
    launchSpecification :: ScheduledInstancesLaunchSpecification,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunScheduledInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'runScheduledInstances_clientToken' - Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'runScheduledInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
  Prelude.Text ->
  RunScheduledInstances
newRunScheduledInstances
  pLaunchSpecification_
  pScheduledInstanceId_ =
    RunScheduledInstances'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        instanceCount = Prelude.Nothing,
        launchSpecification = pLaunchSpecification_,
        scheduledInstanceId = pScheduledInstanceId_
      }

-- | Unique, case-sensitive identifier that ensures the idempotency of the
-- request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
runScheduledInstances_clientToken :: Lens.Lens' RunScheduledInstances (Prelude.Maybe Prelude.Text)
runScheduledInstances_clientToken = Lens.lens (\RunScheduledInstances' {clientToken} -> clientToken) (\s@RunScheduledInstances' {} a -> s {clientToken = a} :: RunScheduledInstances)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
runScheduledInstances_dryRun :: Lens.Lens' RunScheduledInstances (Prelude.Maybe Prelude.Bool)
runScheduledInstances_dryRun = Lens.lens (\RunScheduledInstances' {dryRun} -> dryRun) (\s@RunScheduledInstances' {} a -> s {dryRun = a} :: RunScheduledInstances)

-- | The number of instances.
--
-- Default: 1
runScheduledInstances_instanceCount :: Lens.Lens' RunScheduledInstances (Prelude.Maybe Prelude.Int)
runScheduledInstances_instanceCount = Lens.lens (\RunScheduledInstances' {instanceCount} -> instanceCount) (\s@RunScheduledInstances' {} a -> s {instanceCount = a} :: RunScheduledInstances)

-- | The launch specification. You must match the instance type, Availability
-- Zone, network, and platform of the schedule that you purchased.
runScheduledInstances_launchSpecification :: Lens.Lens' RunScheduledInstances ScheduledInstancesLaunchSpecification
runScheduledInstances_launchSpecification = Lens.lens (\RunScheduledInstances' {launchSpecification} -> launchSpecification) (\s@RunScheduledInstances' {} a -> s {launchSpecification = a} :: RunScheduledInstances)

-- | The Scheduled Instance ID.
runScheduledInstances_scheduledInstanceId :: Lens.Lens' RunScheduledInstances Prelude.Text
runScheduledInstances_scheduledInstanceId = Lens.lens (\RunScheduledInstances' {scheduledInstanceId} -> scheduledInstanceId) (\s@RunScheduledInstances' {} a -> s {scheduledInstanceId = a} :: RunScheduledInstances)

instance Core.AWSRequest RunScheduledInstances where
  type
    AWSResponse RunScheduledInstances =
      RunScheduledInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RunScheduledInstancesResponse'
            Prelude.<$> ( x Data..@? "instanceIdSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunScheduledInstances where
  hashWithSalt _salt RunScheduledInstances' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` launchSpecification
      `Prelude.hashWithSalt` scheduledInstanceId

instance Prelude.NFData RunScheduledInstances where
  rnf RunScheduledInstances' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf instanceCount `Prelude.seq`
          Prelude.rnf launchSpecification `Prelude.seq`
            Prelude.rnf scheduledInstanceId

instance Data.ToHeaders RunScheduledInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RunScheduledInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery RunScheduledInstances where
  toQuery RunScheduledInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RunScheduledInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "InstanceCount" Data.=: instanceCount,
        "LaunchSpecification" Data.=: launchSpecification,
        "ScheduledInstanceId" Data.=: scheduledInstanceId
      ]

-- | Contains the output of RunScheduledInstances.
--
-- /See:/ 'newRunScheduledInstancesResponse' smart constructor.
data RunScheduledInstancesResponse = RunScheduledInstancesResponse'
  { -- | The IDs of the newly launched instances.
    instanceIdSet :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RunScheduledInstancesResponse
newRunScheduledInstancesResponse pHttpStatus_ =
  RunScheduledInstancesResponse'
    { instanceIdSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the newly launched instances.
runScheduledInstancesResponse_instanceIdSet :: Lens.Lens' RunScheduledInstancesResponse (Prelude.Maybe [Prelude.Text])
runScheduledInstancesResponse_instanceIdSet = Lens.lens (\RunScheduledInstancesResponse' {instanceIdSet} -> instanceIdSet) (\s@RunScheduledInstancesResponse' {} a -> s {instanceIdSet = a} :: RunScheduledInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
runScheduledInstancesResponse_httpStatus :: Lens.Lens' RunScheduledInstancesResponse Prelude.Int
runScheduledInstancesResponse_httpStatus = Lens.lens (\RunScheduledInstancesResponse' {httpStatus} -> httpStatus) (\s@RunScheduledInstancesResponse' {} a -> s {httpStatus = a} :: RunScheduledInstancesResponse)

instance Prelude.NFData RunScheduledInstancesResponse where
  rnf RunScheduledInstancesResponse' {..} =
    Prelude.rnf instanceIdSet `Prelude.seq`
      Prelude.rnf httpStatus
